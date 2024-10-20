{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Web.URL.Shortener.Frontend (defaultMain, defaultApp) where

import Control.Concurrent (threadDelay)
import Control.Lens
import Control.Monad (guard)
import Control.Monad.IO.Class (liftIO)
import Data.Bool (bool)
import Data.ByteString.Char8 qualified as BS8
import Data.Either (isRight)
import Data.Functor (void)
import Data.Generics.Labels ()
import Data.List qualified as L
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe (fromJust, isJust)
import Data.String (IsString (..))
import Data.Text qualified as T
import GHC.Generics (Generic)
import Language.Javascript.JSaddle qualified as JSM
import Language.Javascript.JSaddle.Runner qualified as Runner
import Miso
import Miso.String (MisoString, ToMisoString (toMisoString))
import Network.HTTP.Types.URI (decodePathSegments)
import Network.URI (URIAuth (..), parseURI)
import Steward.Client.Fetch (ClientM, runClient)
import Steward.Types
import Web.URL.Shortener.API

defaultMain :: IO ()
defaultMain = Runner.run defaultApp

defaultApp :: JSM ()
defaultApp = do
  uri <-
    getCurrentURI
      <&> #uriPath .~ ""
      <&> #uriQuery .~ ""
      <&> #uriFragment .~ ""
  let model = initialModel uri
  startApp App {subs = [], view = viewModel, ..}
  where
    initialAction = Init
    update = updateModel

    events = defaultEvents
    mountPoint = Nothing
    logLevel = Off

initialModel :: URI -> Model
initialModel rootUri =
  Model
    { mode = Idle
    , aliases = mempty
    , rootUri
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
    #> m {mode = Editing alias (maybe (Left "N/A") (Right . Alias . (.dest)) $ Map.lookup alias $ aliases m)}
updateModel (SaveAlias name alias') m =
  m <# do
    ainfo <- callApi $ api.putAlias.call name alias'
    pure $ UpdateAlias name ainfo
updateModel StartCreatingAlias m = noEff m {mode = CreatingNewAlias "" (Left "")}
updateModel (SetNewAliasName name) m =
  case m.mode of
    CreatingNewAlias _ ainfo -> noEff m {mode = CreatingNewAlias name ainfo}
    _ -> noEff m
updateModel (SetNewAliasUrl url) m =
  case m.mode of
    CreatingNewAlias name _ ->
      noEff
        m
          { mode =
              CreatingNewAlias name $
                maybe (Left url) (Right . Alias) $
                  parseAbsUrl m url
          }
    _ -> noEff m
updateModel (RegisterAlias name alias) m =
  m
    `batchEff` [ UpdateAlias name <$> callApi (api.postAlias.call name alias)
               , OpenAlias name <$ liftIO (threadDelay 1000_000)
               ]
updateModel (UpdateEditingUrl url) m =
  case m.mode of
    Editing name _ ->
      noEff
        m
          { mode =
              Editing name $
                maybe (Left url) (Right . Alias) $
                  parseAbsUrl m url
          }
    _ -> noEff m
updateModel CopyUrl m =
  case m.mode of
    Editing name _
      | Just AliasInfo {..} <- Map.lookup name m.aliases ->
          m <# do
            clip <- JSM.eval ("navigator.clipboard" :: String)
            void $ JSM.liftJSM $ clip JSM.# ("writeText" :: String) $ [show aliasUrl]
            pure NoOp
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
                    [ div_
                        [class_ "level"]
                        [ div_ [class_ "level-left"] [div_ [class_ "level-item"] ["Aliases"]]
                        , div_
                            [class_ "level-right"]
                            [ div_
                                [class_ "level-item"]
                                [ a_ [onClick StartCreatingAlias, class_ "button is-small is-primary"] [mdiDark "add"]
                                ]
                            ]
                        ]
                    ]
                , ul_
                    [class_ "menu-list"]
                    [ li_ [] [a_ attrs [text $ fromAliasName name]]
                    | name <- Map.keys aliases
                    , let attrs =
                            if Just name == activeAlias m
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
renderMain m@Model {..} =
  case mode of
    Idle -> [h2_ [] ["Select an alias or create New"]]
    Editing name eith ->
      case Map.lookup name aliases of
        Nothing -> [h2_ [] ["Alias not found"]]
        Just aliasInfo -> renderAlias name aliasInfo eith
    CreatingNewAlias name malias ->
      let aliasName = parseAliasName name
          isOkName = isRight aliasName
          dest = either id (T.pack . show . (.dest)) malias
          aliasClass = fromString $ unwords $ "input" : ["is-danger" | not isOkName]
          muri = parseAbsUrl m dest
          isOkUrl = isJust muri
          urlClass = fromString $ unwords $ "input" : ["is-danger" | not isOkUrl]
          isValid = isOkName && isOkUrl
          btnClass =
            fromString $
              unwords $
                "button"
                  : if isValid then ["is-primary"] else ["is-disabled"]
          entry =
            if isValid
              then RegisterAlias (either error id aliasName) Alias {dest = fromJust muri}
              else NoOp
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
                      , onEnter entry
                      ]
                  ]
              ]
          , div_
              [class_ "field"]
              [ label_ [class_ "label"] ["URL"]
              , div_
                  [class_ "control"]
                  [ input_
                      [ class_ "input"
                      , type_ "text"
                      , value_ dest
                      , placeholder_ "url"
                      , class_ urlClass
                      , onInput SetNewAliasUrl
                      , onEnter entry
                      ]
                  ]
              ]
          , div_
              [class_ "field"]
              [ div_
                  [class_ "control"]
                  [ button_
                      [ class_ btnClass
                      , onClick entry
                      , disabled_ $ not isValid
                      ]
                      ["Create"]
                  ]
              ]
          ]

onEnter :: Action -> Attribute Action
onEnter action = onKeyDown $ bool NoOp action . (== KeyCode 13)

parseAbsUrl :: Model -> T.Text -> Maybe URI
parseAbsUrl m txt = do
  uri <- parseURI $ T.unpack txt
  guard $ maybe False (not . null . uriRegName) $ uriAuthority uri
  guard $ not $ null $ uriScheme uri
  guard $ not $ uri `isSubUriOf` m.rootUri
  pure uri

isSubUriOf :: URI -> URI -> Bool
isSubUriOf sub super =
  fmap uriRegName super.uriAuthority == fmap uriRegName sub.uriAuthority
    && decodePathSegments (BS8.pack super.uriPath) `L.isPrefixOf` decodePathSegments (BS8.pack sub.uriPath)

renderAlias :: AliasName -> AliasInfo -> Either MisoString Alias -> [View Action]
renderAlias name ainfo einfo =
  let isValid = isRight einfo
      dest = either id (fromString . show . (.dest)) einfo
      entry = case einfo of
        Left _ -> NoOp
        Right alias -> SaveAlias name Alias {dest = alias.dest}
      btnClass = fromString $ unwords $ "button" : ["is-primary" | isValid]
   in [ h2_ [] ["Alias: ", code_ [] [text $ fromAliasName name]]
      , div_
          [class_ "field has-addons"]
          [ p_
              [class_ "control has-icons-left is-expanded"]
              [ input_
                  [ class_ "input"
                  , type_ "text"
                  , value_ $ toMisoString $ show ainfo.aliasUrl
                  , readonly_ True
                  ]
              , span_
                  [class_ "icon is-left"]
                  [ span_
                      [ class_ "material-symbols-outlined"
                      ]
                      [text "link"]
                  ]
              ]
          , p_
              [class_ "control"]
              [ button_
                  [class_ "button", onClick CopyUrl]
                  [mdiDark "content_copy"]
              ]
          ]
      , div_
          [class_ "field"]
          [ label_ [class_ "label"] ["Full URL"]
          , div_
              [class_ "control"]
              [ input_
                  [ class_ "input"
                  , type_ "text"
                  , value_ dest
                  , onEnter entry
                  , onInput UpdateEditingUrl
                  ]
              ]
          ]
      , div_
          [class_ "field"]
          [ div_
              [class_ "control"]
              [ button_
                  [ class_ btnClass
                  , onClick entry
                  , disabled_ $ not isValid
                  ]
                  ["Save"]
              ]
          ]
      ]

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
  | Editing !AliasName !(Either T.Text Alias)
  | CreatingNewAlias !T.Text !(Either T.Text Alias)
  deriving (Show, Eq, Ord, Generic)

type AliasMap = Map AliasName AliasInfo

data Model = Model
  { mode :: !Mode
  , aliases :: !AliasMap
  , rootUri :: !URI
  }
  deriving (Show, Eq, Ord, Generic)

activeAlias :: Model -> Maybe AliasName
activeAlias m = case m.mode of
  Editing name _ -> Just name
  _ -> Nothing

data Action
  = NoOp
  | Init
  | SyncAll
  | SyncAlias !AliasName
  | SetAliases !AliasMap
  | UpdateAlias !AliasName !AliasInfo
  | OpenAlias !AliasName
  | SaveAlias !AliasName !Alias
  | StartCreatingAlias
  | RegisterAlias !AliasName !Alias
  | SetNewAliasName !MisoString
  | SetNewAliasUrl !MisoString
  | UpdateEditingUrl !MisoString
  | CopyUrl
  deriving (Show, Eq, Ord, Generic)
