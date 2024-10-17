{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE RequiredTypeArguments #-}

module Web.URL.Shortener.Worker (handlers, JSHandlers, JSObject (..)) where

import Control.Exception (Exception)
import Control.Exception.Safe (handle, throwIO, throwString)
import Control.Lens ((%~), (.~))
import Control.Monad (forM, unless, when)
import Data.Aeson qualified as J
import Data.ByteString.Builder qualified as BB
import Data.ByteString.Char8 qualified as BS8
import Data.ByteString.Lazy.Char8 qualified as LBS
import Data.ByteString.Lazy.Char8 qualified as LBS8
import Data.DList qualified as DL
import Data.Function ((&))
import Data.Generics.Labels ()
import Data.List qualified as L
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe (catMaybes, isJust, isNothing)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import Data.Word (Word32)
import Effectful hiding (inject)
import Effectful.Concurrent
import Effectful.Dispatch.Static (unsafeEff_)
import Effectful.Prim (runPrim)
import Effectful.Random.Static
import Effectful.Reader.Static (Reader, asks, runReader)
import Effectful.Time (runClock)
import GHC.Generics (Generic)
import GHC.Stack (HasCallStack)
import GHC.Wasm.Object.Builtins
import GHC.Wasm.Web.Generated.Headers qualified as Headers
import Network.Cloudflare.Worker.Binding.Cache qualified as Cache
import Network.Cloudflare.Worker.Binding.KV (KVClass)
import Network.Cloudflare.Worker.Binding.KV qualified as KV
import Network.Cloudflare.Worker.Request qualified as Req
import Network.Cloudflare.Worker.Response (WorkerResponse)
import Network.Cloudflare.Worker.Response qualified as Resp
import Network.HTTP.Types (encodePathSegments)
import Network.HTTP.Types.Status qualified as H
import Network.HTTP.Types.URI (decodePathSegments)
import Network.URI (URI)
import Network.URI qualified as URI
import Network.URI.Lens (uriPathLens)
import Steward.Workers
import Wasm.Prelude.Linear qualified as PL
import Web.URL.Shortener.API

handlers :: IO JSHandlers
handlers = toJSHandlers Handlers {fetch = fetcher}

type Env = BindingsClass '["ROOT_URI", "CF_TEAM_NAME"] '["CF_AUD_TAG"] '[ '("KV", KVClass)]

data WorkerEnv = WorkerEnv {rootUri :: !URI.URI}
  deriving (Show, Eq, Ord, Generic)

fetcher :: FetchHandler Env
fetcher = runWorker' $ handle (fmap Right . handleStatus) do
  g <- newStdGen
  runClock $ runConcurrent $ runPrim $ evalRandom g $ do
    wenv <- buildWorkerEnv
    runReader wenv do
      env <- getWorkerEnv @Env
      let !rawTeam = getEnv "CF_TEAM_NAME" env
      teamName <- case J.fromJSON rawTeam of
        J.Error e ->
          throwString $
            "Could not parse CF_TEAM_NAME (" <> show rawTeam <> "): " <> e
        J.Success x -> pure x
      let !appAudienceID = getSecret "CF_AUD_TAG" env
          !authCfg = CloudflareTunnelConfig {..}
      req <- getRawRequest @Env
      root <- asks @WorkerEnv (.rootUri)
      let rawReqUri = T.unpack $ Req.getUrl req
      reqUri <-
        maybe (throwString $ "Invalid request URI: " <> show rawReqUri) pure $
          URI.parseURI rawReqUri
      let goodReq =
            reqUri.uriAuthority == root.uriAuthority
              && reqUri.uriScheme == root.uriScheme
          notFound = throwIO $ StatusCodeException H.notFound404 $ "Not Found: " <> LBS.pack rawReqUri
      unless goodReq notFound
      reqPaths <-
        maybe notFound pure $
          L.stripPrefix (decodePathSegments (BS8.pack root.uriPath)) $
            decodePathSegments (BS8.pack reqUri.uriPath)
      case reqPaths of
        "api" : _ -> do
          Right <$> withCloudflareTunnelAuth @Env authCfg (const $ pure True) \_ ->
            fromHandlers @Env endpoints
        [dest] ->
          Left
            <$> serveCached
              CacheOptions
                { cacheTTL = 90
                , onlyOk = True
                , includeQuery = True
                }
              reqUri
              (redirect dest)
        _ -> notFound

buildWorkerEnv :: (Worker Env :> es) => Eff es WorkerEnv
buildWorkerEnv = do
  rootUri <-
    either (throwString . ("Invalid ROOT_URI: " <>)) pure
      . eitherResult
      . J.fromJSON @URI.URI
      . getEnv "ROOT_URI"
      =<< getWorkerEnv @Env
  pure WorkerEnv {..}

handleStatus :: StatusCodeException -> Eff [Worker Env, IOE] StewardResponse
handleStatus (StatusCodeException code msg) =
  pure
    StewardResponse {headers = mempty, status = code, body = msg}

endpoints :: (Worker Env :> es, Reader WorkerEnv :> es) => AdminAPI (Handler (Eff es))
endpoints =
  AdminAPI
    { putAlias = Handler putAlias
    , postAlias = Handler postAlias
    , listAliases = Handler listAliases
    , getAlias = Handler getAlias
    }

data CacheOptions = CacheOptions
  { cacheTTL :: !Word32
  , onlyOk :: !Bool
  , includeQuery :: !Bool
  }
  deriving (Show, Eq, Ord, Generic)

serveCached ::
  (IOE :> es, Worker Env :> es) =>
  CacheOptions ->
  URI ->
  Eff es WorkerResponse ->
  Eff es WorkerResponse
serveCached opts uri act = do
  req <- getRawRequest @Env
  ctx <- getContext @Env
  let cachePath =
        T.pack $
          show $
            uri
              & #uriFragment .~ ""
              & if opts.includeQuery then id else #uriQuery .~ ""
  reqHdrs0 <- liftIO $ Resp.toHeaders $ Map.fromList (Req.getHeaders req)
  keyReq <-
    liftIO $
      Req.newRequest (Just cachePath) $
        Just $
          newDictionary
            PL.$ setPartialField "headers" (upcast reqHdrs0)
  mcache <- liftIO $ fmap fromNullable . await =<< Cache.match (inject keyReq) Nothing
  case mcache of
    Just resp -> pure resp
    Nothing -> do
      resp <- act
      code <- liftIO $ Resp.getStatus resp
      when (not opts.onlyOk && code == 200) do
        respHdrs0 <- liftIO $ Resp.getHeaders resp
        cacheControlHdr <- liftIO $ fromHaskellByteString "Cache-Control"
        cacheControl <-
          liftIO $
            fromHaskellByteString $
              "public, max-age=" <> BS8.pack (show opts.cacheTTL)
        liftIO $
          Headers.js_fun_set_ByteString_ByteString_undefined
            respHdrs0
            cacheControlHdr
            cacheControl
        liftIO $ Resp.setHeaders resp respHdrs0
        liftIO $ waitUntil ctx =<< Cache.put keyReq resp
      pure resp

redirect :: (Worker Env :> es, IOE :> es) => Text -> Eff es WorkerResponse
redirect alias = do
  url <-
    maybe (throwIO $ StatusCodeException H.notFound404 $ "URL Alias Not Found: " <> LBS.fromStrict (TE.encodeUtf8 alias)) pure
      =<< withKV (\kv -> KV.get kv (T.unpack alias))
  fromStewardResponse
    StewardResponse
      { headers = [("location", BS8.pack url)]
      , status = H.status301
      , body = "<html><head><title>Moved</title><meta http-equiv=\"refresh\" content=\"0;" <> LBS8.pack url <> "\"></head><body><a href=\"" <> LBS8.pack url <> "\">moved here</a></body></html>"
      }

data StatusCodeException = StatusCodeException !H.Status !LBS.ByteString
  deriving (Show, Generic)
  deriving anyclass (Exception)

withKV :: (Worker Env :> es) => (KV.KV -> IO a) -> Eff es a
withKV f = do
  !kv <- getBinding "KV" <$> getWorkerEnv @Env
  unsafeEff_ $ f kv

getAlias :: (HasCallStack, Worker Env :> es, Reader WorkerEnv :> es) => Text -> Eff es AliasInfo
getAlias alias =
  maybe
    ( throwIO $
        StatusCodeException H.notFound404 $
          "No alias found: " <> LBS.fromStrict (TE.encodeUtf8 alias)
    )
    pure
    =<< getAliasInfo alias

postAlias ::
  ( HasCallStack
  , Worker Env :> es
  , Reader WorkerEnv :> es
  ) =>
  Text ->
  Alias ->
  Eff es AliasInfo
postAlias alias Alias {..} = do
  minfo <- getAliasInfo alias
  when (isJust minfo) do
    throwIO $ StatusCodeException H.conflict409 $ "Alias already exists: " <> LBS.fromStrict (TE.encodeUtf8 alias)
  withKV \kv ->
    KV.put
      kv
      KV.PutOptions {expirationTtl = Nothing, metadata = Nothing, expiration = Nothing}
      (T.unpack alias)
      (show dest)
  aliasUrl <- aliasUrlFor alias
  pure AliasInfo {..}

aliasUrlFor :: (Reader WorkerEnv :> es) => Text -> Eff es URI
aliasUrlFor alias = do
  root <- asks @WorkerEnv (.rootUri)
  pure $
    root
      & uriPathLens
        %~ LBS.unpack . BB.toLazyByteString . encodePathSegments . (<> [alias]) . decodePathSegments . BS8.pack

putAlias :: (Worker Env :> es, Reader WorkerEnv :> es) => Text -> Alias -> Eff es AliasInfo
putAlias alias Alias {..} = do
  minfo <- getAliasInfo alias
  when (isNothing minfo) do
    throwIO $ StatusCodeException H.notFound404 $ "Alias not found: " <> LBS.fromStrict (TE.encodeUtf8 alias)
  withKV \kv ->
    KV.put
      kv
      KV.PutOptions {expirationTtl = Nothing, metadata = Nothing, expiration = Nothing}
      (T.unpack alias)
      (show dest)
  aliasUrl <- aliasUrlFor alias
  pure AliasInfo {..}

listAliases :: (HasCallStack, Worker Env :> es, Reader WorkerEnv :> es) => Eff es (Map Text AliasInfo)
listAliases = collect =<< withKV (go Nothing mempty)
  where
    go !mcursor !keys kv = do
      resl <-
        either throwString pure
          =<< KV.listKeys
            kv
            KV.ListKeys
              { cursor = mcursor
              , limit = Nothing
              , prefix = Nothing
              }
      let keys' = keys <> DL.fromList resl.keys
      case resl.cursor of
        Nothing -> pure keys'
        Just cursor -> go (Just cursor) keys' kv
    collect =
      fmap (Map.fromList . catMaybes)
        . mapM do
          \k -> do
            info <- getAliasInfo $ T.pack k.name
            pure $ (T.pack k.name,) <$> info
        . DL.toList

getAliasInfo ::
  (Worker Env :> es, Reader WorkerEnv :> es) =>
  T.Text ->
  Eff es (Maybe AliasInfo)
getAliasInfo key = do
  murl <- withKV \kv -> KV.get kv $ T.unpack key
  forM murl \url -> do
    dest <- maybe (throwString $ "Invalid redirect destination: " <> url <> " for alias: " <> T.unpack key) pure $ URI.parseURI url
    aliasUrl <- aliasUrlFor key
    pure AliasInfo {aliasUrl, dest}

eitherResult :: J.Result a -> Either String a
eitherResult = \case
  J.Error err -> Left err
  J.Success x -> Right x
