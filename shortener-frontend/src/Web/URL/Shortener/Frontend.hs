{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Web.URL.Shortener.Frontend (defaultMain, defaultApp) where

import Control.Lens
import Data.Generics.Labels ()
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Text qualified as T
import GHC.Generics (Generic)
import Language.Javascript.JSaddle.Runner qualified as Runner
import Miso
import Steward.Client.Fetch (ClientM, runClient)
import Steward.Types
import Web.URL.Shortener.API

defaultMain :: IO ()
defaultMain = Runner.run defaultApp

defaultApp :: JSM ()
defaultApp =  startApp App {subs = [], view = viewModel, ..}
  where
    initialAction = Init
    model = initialModel
    update = updateModel

    events = defaultEvents
    mountPoint = Nothing
    logLevel = Off

initialModel :: Model
initialModel = Model {mode = Idle, aliases = mempty}

api :: AdminAPI (Client ClientM)
api = client

updateModel :: Action -> Model -> Effect Action Model
updateModel NoOp m = noEff m
updateModel Init m = m <# pure SyncAll
updateModel SyncAll m =
  m <# do
    aliases <- callApi api.listAliases.call
    pure $ SetAliases aliases
updateModel (SetAliases aliases) m = noEff m {aliases}
updateModel (SyncAlias alias) m =
  m <# do
    aliasInfo <- callApi $ api.getAlias.call alias
    pure $ UpdateAlias alias aliasInfo
updateModel (UpdateAlias alias aliasInfo) m =
  noEff m {aliases = Map.insert alias aliasInfo $ aliases m}
updateModel (OpenAlias alias) m =
  pure (SyncAlias alias)
    #> m {mode = FocusExistingAlias alias}
updateModel (SaveAlias name alias') m =
  m <# do
    ainfo <- callApi $ api.putAlias.call name alias'
    pure $ UpdateAlias name ainfo

callApi :: ClientM a -> JSM a
callApi act = do
  uri <-
    getCurrentURI
      <&> #uriPath .~ ""
      <&> #uriQuery .~ ""
      <&> #uriFragment .~ ""
  runClient (show uri) act

viewModel :: Model -> View Action
viewModel m@Model {..} =
  div_
    []
    [ section_
        [class_ "hero"]
        [ div_
            [class_ "hero-body"]
            [ div_ [class_ "title"] ["URL Shortener"]
            , div_ [class_ "subtitle"] ["Admin Panel"]
            ]
        ]
    , div_
        [class_ "columns"]
        [ div_
            [class_ "is-one-third"]
            [ aside_
                [class_ "menu"]
                [ p_ [class_ "menu-label"] ["Aliases"]
                , ul_ [class_ "menu-list"] $ map (li_ [] . pure . uncurry renderAlias) $ Map.toList aliases
                ]
            ]
        , renderMain m
        ]
    ]

renderMain :: Model -> View Action
renderMain Model {..} =
  case mode of
    Idle -> div_ [] []
    FocusExistingAlias name -> div_ [] [text name]

renderAlias :: T.Text -> AliasInfo -> View Action
renderAlias name _ =
  a_ [onClick $ OpenAlias name] [text name]

data Mode
  = Idle
  | FocusExistingAlias !T.Text
  deriving (Show, Eq, Ord, Generic)

type AliasMap = Map T.Text AliasInfo

data Model = Model
  { mode :: !Mode
  , aliases :: !AliasMap
  }
  deriving (Show, Eq, Ord, Generic)

data Action
  = NoOp
  | Init
  | SyncAll
  | SyncAlias !T.Text
  | SetAliases !AliasMap
  | UpdateAlias !T.Text !AliasInfo
  | OpenAlias !T.Text
  | SaveAlias !T.Text !Alias
  deriving (Show, Eq, Ord, Generic)
