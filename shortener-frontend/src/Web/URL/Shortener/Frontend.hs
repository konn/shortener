{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Web.URL.Shortener.Frontend (defaultMain, defaultApp) where

import Control.Lens
import Data.Char qualified as C
import Data.Generics.Labels ()
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.String (IsString (..))
import Data.Text qualified as T
import GHC.Generics (Generic)
import Language.Javascript.JSaddle.Runner qualified as Runner
import Miso
import Miso.String (MisoString)
import Steward.Client.Fetch (ClientM, runClient)
import Steward.Types
import Web.URL.Shortener.API

defaultMain :: IO ()
defaultMain = Runner.run defaultApp

defaultApp :: JSM ()
defaultApp = startApp App {subs = [], view = viewModel, ..}
  where
    initialAction = Init
    model = initialModel
    update = updateModel

    events = defaultEvents
    mountPoint = Nothing
    logLevel = Off

initialModel :: Model
initialModel =
  Model
    { mode = Idle
    , aliases = mempty
    , activeAlias = Nothing
    }

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
updateModel NewAlias m = noEff m {mode = CreatingNewAlias "" (Left "")}
updateModel (SetNewAliasName name) m =
  case m.mode of
    CreatingNewAlias _ ainfo -> noEff m {mode = CreatingNewAlias name ainfo}
    _ -> noEff m

callApi :: ClientM a -> JSM a
callApi act = do
  uri <-
    getCurrentURI
      <&> #uriPath .~ ""
      <&> #uriQuery .~ ""
      <&> #uriFragment .~ ""
  consoleLog $ "Running: " <> fromString (show uri)
  runClient (show uri) act

viewModel :: Model -> View Action
viewModel m@Model {..} =
  section_
    [class_ "section"]
    [ div_
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
            [class_ "column menu is-3 theme-light"]
            [ div_
                [class_ "box"]
                [ p_
                    [class_ "menu-label"]
                    [ a_ [onClick NewAlias, class_ "button is-small is-primary"] [mdiDark "add"]
                    ]
                , p_ [class_ "menu-label"] ["Aliases"]
                , ul_
                    [class_ "menu-list"]
                    [ li_ [] [a_ attrs [text name]]
                    | name <- Map.keys aliases
                    , let attrs =
                            if Just name == activeAlias
                              then [class_ "is-active"]
                              else [onClick $ OpenAlias name]
                    ]
                ]
            ]
        , div_
            [class_ "column"]
            [div_ [class_ "box theme-light content"] (renderMain m)]
        ]
    ]

renderMain :: Model -> [View Action]
renderMain Model {..} =
  case mode of
    Idle -> [h2_ [] ["Select an alias or create New"]]
    FocusExistingAlias name ->
      case Map.lookup name aliases of
        Nothing -> [h2_ [] ["Alias not found"]]
        Just aliasInfo -> renderAlias name aliasInfo
    CreatingNewAlias name mailias ->
      let isOkName = isGoodAliasName name
          aliasClass = fromString $ unwords $ "input" : ["is-danger" | not isOkName]
       in [ h2_ [] ["New Alias"]
          , div_
              [class_ "field"]
              [ label_ [class_ "label"] ["Name"]
              , div_
                  [class_ "control"]
                  [ input_
                      [ class_ "input"
                      , type_ "text"
                      , value_ name
                      , placeholder_ "name"
                      , class_ aliasClass
                      , onInput SetNewAliasName
                      ]
                  ]
              ]
          ]

isGoodAliasName :: T.Text -> Bool
isGoodAliasName txt =
  not (T.null txt)
    && txt /= "api"
    && T.isAscii txt
    && T.all C.isAlpha (T.take 1 txt)
    && T.all (\c -> C.isAlphaNum c || c == '-' || c == '_') txt

isGoodDest :: T.Text -> Bool
isGoodDest txt = not (T.null txt) && T.isPrefixOf "http" txt

renderAlias :: T.Text -> AliasInfo -> [View Action]
renderAlias name AliasInfo {..} =
  []

mdiDark :: MisoString -> View a
mdiDark name =
  span_
    [class_ "icon"]
    [ span_
        [ class_ "material-symbols-outlined"
        ]
        [text name]
    ]

data Mode
  = Idle
  | FocusExistingAlias !T.Text
  | CreatingNewAlias !T.Text !(Either T.Text Alias)
  deriving (Show, Eq, Ord, Generic)

type AliasMap = Map T.Text AliasInfo

data Model = Model
  { mode :: !Mode
  , aliases :: !AliasMap
  , activeAlias :: !(Maybe MisoString)
  }
  deriving (Show, Eq, Ord, Generic)

data Action
  = NoOp
  | Init
  | SyncAll
  | SyncAlias !MisoString
  | SetAliases !AliasMap
  | UpdateAlias !MisoString !AliasInfo
  | OpenAlias !MisoString
  | SaveAlias !MisoString !Alias
  | NewAlias
  | SetNewAliasName !MisoString
  deriving (Show, Eq, Ord, Generic)
