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
  Alias (..),
  AliasInfo (..),
  AdminAPI (..),
  AliasName (),
  parseAliasName,
  fromAliasName,
) where

import Control.Monad (unless, when)
import Data.Aeson
import Data.Aeson.Types (toJSONKeyText)
import Data.CaseInsensitive (CI)
import Data.CaseInsensitive qualified as CI
import Data.Char qualified as C
import Data.Map (Map)
import Data.Text qualified as T
import GHC.Generics
import Network.URI (URI)
import Steward.Types

newtype Alias = Alias {dest :: URI}
  deriving (Show, Eq, Ord, Generic)
  deriving anyclass (FromJSON, ToJSON)

data AliasInfo = AliasInfo {aliasUrl :: !URI, dest :: !URI}
  deriving (Show, Eq, Ord, Generic)
  deriving anyclass (FromJSON, ToJSON)

data AdminAPI mode = AdminAPI
  { listAliases :: mode ::: "admin" /> "api" /> "aliases" /> Get (JSON (Map AliasName AliasInfo))
  , getAlias :: mode ::: "admin" /> "api" /> "aliases" /> AliasName /> Get (JSON AliasInfo)
  , postAlias :: mode ::: "admin" /> "api" /> "aliases" /> AliasName /> JSONBody Alias /> Post (JSON AliasInfo)
  , putAlias :: mode ::: "admin" /> "api" /> "aliases" /> AliasName /> JSONBody Alias /> Put (JSON AliasInfo)
  }
  deriving (Generic)
  deriving anyclass (HasClient, HasHandler m)

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

instance FromPathPieces AliasName where
  parsePathPieces inp = case parsePathPieces @T.Text inp of
    NoMatch -> NoMatch
    Failed err -> Failed err
    Parsed (t, rest) ->
      either Failed (pure . (,rest)) $ parseAliasName t
  {-# INLINE parsePathPieces #-}

instance ToPathPieces AliasName where
  toPathPieces = toPathPieces . fromAliasName
  {-# INLINE toPathPieces #-}
