{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE NoFieldSelectors #-}

module Web.URL.Shortener.API (
  RootAPI (..),
  Alias (..),
  AliasInfo (..),
  AdminApp (..),
  AdminAPI (..),
  AliasName (),
  ShortenerUser (..),
  HTML,
  parseAliasName,
  fromAliasName,
) where

import Control.Monad (unless, when)
import Data.Aeson
import Data.Aeson.Types (toJSONKeyText)
import Data.Bifunctor qualified as Bi
import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as LBS
import Data.CaseInsensitive (CI)
import Data.CaseInsensitive qualified as CI
import Data.Char qualified as C
import Data.List.NonEmpty qualified as NE
import Data.Map (Map)
import Data.Text qualified as T
import GHC.Generics
import Lucid (renderBS, toHtml)
import Lucid.Base (ToHtml)
import Network.HTTP.Media qualified as M
import Servant.API
import Servant.Auth
import Servant.Auth.JWT (FromJWT, ToJWT)

newtype Alias = Alias {dest :: URI}
  deriving (Show, Eq, Ord, Generic)
  deriving anyclass (FromJSON, ToJSON)

data AliasInfo = AliasInfo {aliasUrl :: !URI, dest :: !URI}
  deriving (Show, Eq, Ord, Generic)
  deriving anyclass (FromJSON, ToJSON)

type RequireUser = Auth '[CloudflareZeroTrust, JWT] ShortenerUser

data HTML

instance Accept HTML where
  contentTypes _ =
    "text"
      M.// "html"
      M./: ("charset", "utf-8")
      NE.:| ["text" M.// "html"]

instance (ToHtml a) => MimeRender HTML a where
  mimeRender _ = renderBS . toHtml

instance MimeUnrender HTML LBS.ByteString where
  mimeUnrender _ = Right

instance MimeUnrender HTML BS.ByteString where
  mimeUnrender _ = Right . LBS.toStrict

data RootAPI mode = RootAPI
  { adminApi :: mode :- "admin" :> "api" :> RequireUser :> NamedRoutes AdminAPI
  , adminApp :: mode :- "admin" :> RequireUser :> NamedRoutes AdminApp
  , redirect :: mode :- Capture "alias" AliasName :> Verb 'GET 301 '[HTML] (Headers '[Header "Location" T.Text] LBS.ByteString)
  }
  deriving (Generic)

data AdminApp mode = AdminApp
  { editAlias :: mode :- "alias" :> Capture "alias" AliasName :> Raw
  , newAlias :: mode :- "new" :> Raw
  , resources :: mode :- Raw
  }
  deriving (Generic)

data AdminAPI mode = AdminAPI
  { listAliases :: mode :- "aliases" :> Get '[JSON] (Map AliasName AliasInfo)
  , getAlias :: mode :- "aliases" :> Capture "alias" AliasName :> Get '[JSON] AliasInfo
  , postAlias :: mode :- "aliases" :> Capture "alias" AliasName :> ReqBody '[JSON] Alias :> Post '[JSON] AliasInfo
  , putAlias :: mode :- "aliases" :> Capture "alias" AliasName :> ReqBody '[JSON] Alias :> Put '[JSON] AliasInfo
  }
  deriving (Generic)

data ShortenerUser = ShortenerUser {email :: T.Text}
  deriving (Show, Eq, Ord, Generic)
  deriving anyclass (FromJSON, ToJSON)
  deriving anyclass (FromJWT, ToJWT)

newtype AliasName = AliasName {rawAliasName :: CI T.Text}
  deriving (Eq, Ord, Generic)

instance Show AliasName where
  showsPrec d = showsPrec d . fromAliasName
  {-# INLINE showsPrec #-}

instance FromJSON AliasName where
  parseJSON = withText "AliasName" $ either fail pure . parseAliasName
  {-# INLINE parseJSON #-}

instance FromJSONKey AliasName where
  fromJSONKey = FromJSONKeyTextParser (either fail pure . parseAliasName)
  {-# INLINE fromJSONKey #-}

instance ToJSON AliasName where
  toJSON = toJSON . fromAliasName
  {-# INLINE toJSON #-}

instance ToJSONKey AliasName where
  toJSONKey = toJSONKeyText fromAliasName
  {-# INLINE toJSONKey #-}

parseAliasName :: T.Text -> Either String AliasName
parseAliasName txt = do
  when (T.null txt) do
    Left "Alias name cannot be empty"
  when (txt `elem` ["api", "admin"]) do
    Left $ "Alias name not allowed: " <> show txt
  let isGood =
        T.isAscii txt
          && T.all C.isAlpha (T.take 1 txt)
          && T.all (\c -> C.isAlphaNum c || c == '-' || c == '_') txt
  unless isGood do
    Left $ "Alas name must be an ASCII string starting with alphabet, containing alphanum and -_ only: " <> show txt
  pure $ AliasName $ CI.mk txt

fromAliasName :: AliasName -> T.Text
fromAliasName = CI.foldedCase . (.rawAliasName)

instance FromHttpApiData AliasName where
  parseUrlPiece inp = case parseUrlPiece @T.Text inp of
    Left err -> Left err
    Right t ->
      Bi.first T.pack $ parseAliasName t
  {-# INLINE parseUrlPiece #-}

instance ToHttpApiData AliasName where
  toUrlPiece = toUrlPiece . fromAliasName
  {-# INLINE toUrlPiece #-}
