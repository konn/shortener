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

import Control.Exception.Safe (throwString)
import Control.Lens ((%~))
import Control.Monad (forM, guard, join, when)
import Data.Aeson qualified as A
import Data.Aeson qualified as J
import Data.ByteString.Builder qualified as BB
import Data.ByteString.Char8 qualified as BS8
import Data.ByteString.Lazy.Char8 qualified as LBS
import Data.ByteString.Lazy.Char8 qualified as LBS8
import Data.DList qualified as DL
import Data.Function ((&))
import Data.Generics.Labels ()
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe (catMaybes, fromMaybe, isJust, isNothing)
import Data.String (IsString (..))
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import Effectful hiding (inject, (:>))
import Effectful.Dispatch.Static (unsafeEff_)
import Effectful.Reader.Static (Reader, asks, runReader)
import Effectful.Servant.Cloudflare.Workers
import Effectful.Servant.Cloudflare.Workers.Assets
import Effectful.Servant.Cloudflare.Workers.Cache (CacheOptions (..), serveCached, serveCachedRaw)
import Effectful.Servant.Cloudflare.Workers.KV (KVClass)
import Effectful.Servant.Cloudflare.Workers.KV qualified as KV
import GHC.Generics (Generic)
import GHC.Wasm.Object.Builtins
import Network.Cloudflare.Worker.Binding (BindingsClass)
import Network.Cloudflare.Worker.Binding qualified as B
import Network.Cloudflare.Worker.Binding.Assets qualified as RawAssets
import Network.Cloudflare.Worker.Handler (JSHandlers)
import Network.Cloudflare.Worker.Request qualified as Req
import Network.HTTP.Types (encodePathSegments)
import Network.HTTP.Types.URI (decodePathSegments)
import Network.URI (URI, parseURI, uriPath)
import Network.URI qualified as URI
import Network.URI.Lens (uriPathLens)
import Servant.API (Header, Headers, Raw, toUrlPiece)
import Servant.API.ResponseHeaders (addHeader)
import Servant.Auth ()
import Servant.Auth.Cloudflare.Workers
import Servant.Cloudflare.Workers (Tagged (..), WorkerT)
import Servant.Cloudflare.Workers.Internal.Response (toWorkerResponse)
import Servant.Cloudflare.Workers.Internal.RouteResult (RouteResult (..))
import Servant.Cloudflare.Workers.Internal.ServerError (responseServerError)
import Servant.Cloudflare.Workers.Prelude (RawM)
import Web.URL.Shortener.API

handlers :: IO JSHandlers
handlers = do
  genericCompileWorkerContextWith @Env
    withWorkerEnv
    ( \env _ -> do
        let audience = do
              let aud = B.getSecret "CF_AUD_TAG" env
              guard $ not $ T.null aud
              pure aud
        team0 <- case A.fromJSON $ B.getEnv "CF_TEAM_NAME" env of
          J.Error e -> throwString $ "Could not parse CF_TEAM_NAME: " <> e
          J.Success x -> pure x
        let team = do
              guard $ not $ null team0
              pure team0
        !sett <- defaultCloudflareZeroTrustSettings audience team
        let !jwt = toJWTSettings sett
        pure $ sett :. jwt :. EmptyContext
    )
    $ workers

buildWorkerEnv :: (HasUniqueWorkerWith Env es) => Eff es WorkerEnv
buildWorkerEnv = do
  rootUri <- getEnv "ROOT_URI"
  let redirectUri = rootUri
  let adminUri = rootUri & addPathSegments ["admin"]
  pure WorkerEnv {..}

withWorkerEnv ::
  (HasUniqueWorkerWith Env es) =>
  Eff (Reader WorkerEnv : es) a ->
  Eff es a
withWorkerEnv act = do
  wenv <- buildWorkerEnv
  runReader wenv act

type Env =
  BindingsClass
    '["ROOT_URI", "CF_TEAM_NAME"]
    '["CF_AUD_TAG"]
    '[ '("KV", KVClass)
     , '("ASSETS", AssetsClass)
     ]

data WorkerEnv = WorkerEnv {rootUri, redirectUri, adminUri :: !URI.URI}
  deriving (Show, Eq, Ord, Generic)

workers ::
  ( HasUniqueWorkerWith Env es
  , Reader WorkerEnv ∈ es
  ) =>
  RootAPI (AsWorkerT Env (Eff es))
workers = RootAPI {adminApi, redirect, adminApp}

guardIfNonEmptyM ::
  AuthResult ShortenerUser ->
  WorkerT Env RawM (Eff es) ->
  WorkerT Env RawM (Eff es)
guardIfNonEmptyM auth act = \req env ctx respond -> do
  let audience = B.getSecret "CF_AUD_TAG" env
  if T.null audience
    then act req env ctx respond
    else case auth of
      Authenticated ShortenerUser {} -> act req env ctx respond
      _ -> unsafeEff_ $ respond $ FailFatal err403 {errBody = "Unauthorised"}

guardIfNonEmpty ::
  AuthResult ShortenerUser ->
  WorkerT Env Raw (Eff es) ->
  WorkerT Env Raw (Eff es)
guardIfNonEmpty auth (Tagged act) = Tagged \req env ctx -> do
  let audience = B.getSecret "CF_AUD_TAG" env
  if T.null audience
    then act req env ctx
    else case auth of
      Authenticated ShortenerUser {} -> act req env ctx
      _ -> toWorkerResponse $ responseServerError err403 {errBody = "Unauthorised"}

adminApp ::
  forall es.
  AuthResult ShortenerUser ->
  AdminApp (AsWorkerT Env (Eff es))
adminApp auth =
  AdminApp
    { editAlias = const $ guardIfNonEmpty auth $ serveIndexAsset
    , newAlias = guardIfNonEmpty auth serveIndexAsset
    , resources = guardIfNonEmpty auth $ serveCachedRaw defaultCacheOpts $ serveAssets "ASSETS"
    }

defaultCacheOpts :: CacheOptions
defaultCacheOpts =
  CacheOptions
    { cacheTTL = 90
    , onlyOk = True
    , includeQuery = True
    }

serveIndexAsset :: WorkerT Env Raw (Eff es)
serveIndexAsset = Tagged \req be _ -> do
  let link = "/" <> toUrlPiece rootApiLinks.adminApp.resources <> "/index.html"
  consoleLog "Generated Link."
  let rawUrl = Req.getUrl req.rawRequest
  consoleLog $ fromText $ "Raw URL: " <> rawUrl
  let !url = fromString @USVString $ show $ (fromMaybe (error $ "Invalid Url: " <> show rawUrl) $ parseURI $ T.unpack rawUrl) {uriPath = T.unpack link}
  consoleLog "Generated URL."
  resp <- await =<< RawAssets.fetch (B.getBinding "ASSETS" be) (inject url)
  consoleLog "ASSETS attained"
  pure resp

redirect ::
  (HasUniqueWorkerWith Env es) =>
  AliasName ->
  Eff es (Headers '[Header "Location" Text] LBS8.ByteString)
redirect alias = do
  url <-
    maybe (serverError err404 {errBody = "URL Alias Not Found: " <> LBS.fromStrict (TE.encodeUtf8 $ fromAliasName alias)}) pure
      =<< KV.get "KV" (T.unpack $ fromAliasName alias)
  serveCached defaultCacheOpts
  pure $
    addHeader (T.pack url) $
      "<!DOCTYPE html><html><head><meta http-equiv=\"refresh\" content=\"0;url=" <> LBS8.pack url <> "\"></head><body><a href=\"" <> LBS8.pack url <> "\">moved here</a></body></html>"

adminApi ::
  (HasUniqueWorkerWith Env es, Reader WorkerEnv ∈ es) =>
  AuthResult ShortenerUser ->
  AdminAPI (AsWorkerT e (Eff es))
adminApi auth =
  AdminAPI
    { listAliases = guardAuth listAliases
    , getAlias = guardAuth . getAlias
    , postAlias = fmap guardAuth . postAlias
    , putAlias = fmap guardAuth . putAlias
    }
  where
    guardAuth act = do
      sec <- getSecret "CF_AUD_TAG"
      if T.null sec
        then act
        else case auth of
          Authenticated ShortenerUser {} -> act
          _ -> unauthorised
    unauthorised = serverError err403 {errBody = "Unauthorised"}

postAlias ::
  (HasUniqueWorkerWith Env es, Reader WorkerEnv ∈ es) =>
  AliasName ->
  Alias ->
  Eff es AliasInfo
postAlias alias Alias {..} = do
  minfo <- getAliasInfo alias
  when (isJust minfo) do
    serverError $ err409 {errBody = "Alias already exists: " <> LBS.fromStrict (TE.encodeUtf8 $ fromAliasName alias)}
  KV.put
    "KV"
    KV.PutOptions {expirationTtl = Nothing, metadata = Nothing, expiration = Nothing}
    (T.unpack $ fromAliasName alias)
    (show dest)
  aliasUrl <- aliasUrlFor alias
  pure AliasInfo {..}

getAlias ::
  ( HasUniqueWorkerWith Env es
  , Reader WorkerEnv ∈ es
  ) =>
  AliasName ->
  Eff es AliasInfo
getAlias alias =
  maybe
    ( serverError $
        err404
          { errBody = "No alias found: " <> LBS.fromStrict (TE.encodeUtf8 $ fromAliasName alias)
          }
    )
    pure
    =<< getAliasInfo alias

listAliases ::
  (HasUniqueWorkerWith Env es, Reader WorkerEnv ∈ es) =>
  Eff es (Map AliasName AliasInfo)
listAliases = collect =<< go Nothing mempty
  where
    go !mcursor !keys = do
      resl <-
        either throwString pure
          =<< KV.listKeys
            "KV"
            KV.ListKeys
              { cursor = mcursor
              , limit = Nothing
              , prefix = Nothing
              }
      let keys' = keys <> DL.fromList resl.keys
      case resl.cursor of
        Nothing -> pure keys'
        Just cursor -> go (Just cursor) keys'
    collect =
      fmap (Map.fromList . catMaybes)
        . mapM do
          \k -> do
            let name =
                  either (const Nothing) Just $ parseAliasName $ T.pack k.name
            fmap join $ forM name \kname -> fmap (kname,) <$> getAliasInfo kname
        . DL.toList

aliasUrlFor :: (Reader WorkerEnv ∈ es) => AliasName -> Eff es URI
aliasUrlFor alias = do
  root <- asks @WorkerEnv (.redirectUri)
  pure $
    root
      & uriPathLens
        %~ LBS.unpack . BB.toLazyByteString . encodePathSegments . (<> [fromAliasName alias]) . decodePathSegments . BS8.pack

getAliasInfo ::
  (HasUniqueWorkerWith Env es, Reader WorkerEnv ∈ es) =>
  AliasName ->
  Eff es (Maybe AliasInfo)
getAliasInfo key = do
  murl <- KV.get "KV" $ T.unpack $ fromAliasName key
  forM murl \url -> do
    dest <- maybe (throwString $ "Invalid redirect destination: " <> url <> " for alias: " <> T.unpack (fromAliasName key)) pure $ URI.parseURI url
    aliasUrl <- aliasUrlFor key
    pure AliasInfo {aliasUrl, dest}

addPathSegments :: [T.Text] -> URI -> URI
addPathSegments fps =
  uriPathLens
    %~ LBS.unpack
      . BB.toLazyByteString
      . encodePathSegments
      . (<> fps)
      . decodePathSegments
      . BS8.pack

putAlias :: (HasUniqueWorkerWith Env es, Reader WorkerEnv ∈ es) => AliasName -> Alias -> Eff es AliasInfo
putAlias alias Alias {..} = do
  minfo <- getAliasInfo alias
  when (isNothing minfo) do
    serverError err404 {errBody = "Alias not found: " <> LBS.fromStrict (TE.encodeUtf8 $ fromAliasName alias)}
  KV.put
    "KV"
    KV.PutOptions {expirationTtl = Nothing, metadata = Nothing, expiration = Nothing}
    (T.unpack $ fromAliasName alias)
    (show dest)
  aliasUrl <- aliasUrlFor alias
  pure AliasInfo {..}

foreign import javascript unsafe "console.log($1)"
  consoleLog :: USVString -> IO ()
