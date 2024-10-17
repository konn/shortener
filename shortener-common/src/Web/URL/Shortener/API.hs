{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE NoFieldSelectors #-}

module Web.URL.Shortener.API (
  Alias (..),
  AliasInfo (..),
  AdminAPI (..),
) where

import Data.Aeson
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
  { listAliases :: mode ::: "api" /> "aliases" /> Get (JSON (Map T.Text AliasInfo))
  , getAlias :: mode ::: "api" /> "aliases" /> T.Text /> Get (JSON AliasInfo)
  , postAlias :: mode ::: "api" /> "aliases" /> T.Text /> JSONBody Alias /> Post (JSON AliasInfo)
  , putAlias :: mode ::: "api" /> "aliases" /> T.Text /> JSONBody Alias /> Put (JSON AliasInfo)
  }
  deriving (Generic)
  deriving anyclass (HasClient, HasHandler m)
