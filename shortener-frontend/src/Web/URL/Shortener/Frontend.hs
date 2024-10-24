{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Web.URL.Shortener.Frontend (defaultMain, defaultApp) where

import Control.Concurrent (threadDelay)
import Control.Exception.Safe (SomeException, tryAny, tryAsync)
import Control.Lens
import Control.Monad (guard)
import Control.Monad.IO.Class (liftIO)
import Data.Bool (bool)
import Data.ByteString.Char8 qualified as BS8
import Data.Either (isRight)
import Data.Functor (void)
import Data.Generics.Labels ()
import Data.Kind (Type)
import Data.List qualified as L
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe (fromJust, isJust)
import Data.Proxy (Proxy (..))
import Data.String (IsString (..))
import Data.Text qualified as T
import Language.Javascript.JSaddle qualified as JSM
import Language.Javascript.JSaddle.Runner qualified as Runner
import Miso
import Miso.String (MisoString, ToMisoString (toMisoString))
import Network.HTTP.Types.URI (decodePathSegments)
import Network.URI (URIAuth (..), parseURI)
import Servant.API
import Servant.API.Generic
import Servant.Auth.Client
import Servant.Client.FetchAPI
import Servant.Client.Generic (genericClient)
import Servant.Links (linkURI)
import Web.URL.Shortener.API

defaultMain :: IO ()
defaultMain = Runner.run defaultApp

defaultApp :: JSM ()
defaultApp = do
  url <- getCurrentURI
  let model = initialModel url
  startApp
    App
      { subs =
          [ uriSub parseURIAction
          ]
      , view = viewModel
      , ..
      }
  where
    initialAction = Init
    update = updateModel

    events = defaultEvents
    mountPoint = Nothing
    logLevel = Off

parseURIAction :: URI -> Action
parseURIAction = ChangeWindowUrl

initialModel :: URI -> Model
initialModel url =
  Model
    { mode = Idle
    , aliases = mempty
    , rootUri =
        url & #uriPath .~ "" & #uriQuery .~ "" & #uriFragment .~ ""
    }

api :: RootAPI (AsClientT (FetchT JSM))
api = genericClient

adminApi :: AdminAPI (AsClientT (FetchT JSM))
adminApi = api.adminApi (CloudflareToken Nothing)

updateUrl :: URI -> Model -> Effect Action Model
updateUrl uri m = do
  Effect () [const $ pushURI uri]
  either
    (const notFound)
    id
    (route (Proxy @("admin" :> ToServantApi AdminApp)) routeUpdater uri)
    m

updateModel :: Action -> Model -> Effect Action Model
updateModel NoOp m = noEff m
updateModel (ChangeWindowUrl uri) m = updateUrl uri m
updateModel Init m =
  m <# do
    parseURIAction <$> getCurrentURI
updateModel SyncAll m =
  m <# do
    aliases <- callApi $ adminApi.listAliases
    pure $ SetAliases aliases
updateModel (SetAliases aliases) m = noEff m {aliases}
updateModel (SyncAlias alias) m =
  m <# do
    eith <- tryAsync @_ @SomeException $ callApi $ adminApi.getAlias alias
    case eith of
      Right aliasInfo -> pure $ UpdateAlias alias aliasInfo
      Left exc -> NoOp <$ consoleLog ("Exception: " <> toMisoString (show exc))
updateModel (UpdateAlias alias aliasInfo) m =
  noEff m {aliases = Map.insert alias aliasInfo $ aliases m}
updateModel (SaveAlias name alias') m =
  m <# do
    ainfo <- callApi $ adminApi.putAlias name alias'
    pure $ UpdateAlias name ainfo
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
    `batchEff` [ UpdateAlias name <$> callApi (adminApi.postAlias name alias)
               , openAlias name <$ liftIO (threadDelay 1000_000)
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

openAlias :: AliasName -> Action
openAlias = ChangeWindowUrl . linkURI . rootApiLinks.adminApp.editAlias

callApi :: FetchT JSM a -> JSM a
callApi act = do
  uri <-
    getCurrentURI
      <&> #uriPath .~ ""
      <&> #uriQuery .~ ""
      <&> #uriFragment .~ ""
  baseUrl <- parseBaseUrl $ show uri
  runFetch baseUrl act

type AsRoute :: Type -> Type
data AsRoute a

instance GenericMode (AsRoute a) where
  type AsRoute a :- xs = RouteT xs a

routeUpdater :: RouteT ("admin" :> ToServantApi AdminApp) (Model -> Effect Action Model)
routeUpdater = toServant routes
  where
    routes :: AdminApp (AsRoute (Model -> Effect Action Model))
    routes =
      AdminApp
        { newAlias = \name m ->
            m {mode = CreatingNewAlias (maybe "" fromAliasName name) (Left "")} <# pure NoOp
        , editAlias = \alias m ->
            let m' = m {mode = Editing alias (maybe (Left "N/A") (Right . Alias . (.dest)) $ Map.lookup alias m.aliases)}
             in m' <# pure (SyncAlias alias)
        , resources = \m -> noEff m {mode = Idle}
        }

setIdle :: Action
setIdle = ChangeWindowUrl $ linkURI rootApiLinks.adminApp.resources

notFound :: Model -> Effect Action Model
notFound m =
  m <# do
    pure $ ChangeWindowUrl $ linkURI rootApiLinks.adminApp.resources

viewModel :: Model -> View Action
viewModel m@Model {..} =
  section_
    [class_ "section"]
    [ div_
        [class_ "hero"]
        [ div_
            [class_ "hero-body"]
            [ div_ [class_ "title"] [a_ [onClick setIdle] ["URL Shortener"]]
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
                                [ a_ [onClick $ startCreatingAlias Nothing, class_ "button is-small is-primary"] [mdiDark "add"]
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
                              else [onClick $ openAlias name]
                    ]
                ]
            ]
        , div_
            [class_ "column"]
            [div_ [class_ "box theme-light content"] (renderMain m)]
        ]
    ]

startCreatingAlias :: Maybe AliasName -> Action
startCreatingAlias = ChangeWindowUrl . linkURI . rootApiLinks.adminApp.newAlias

renderMain :: Model -> [View Action]
renderMain m@Model {..} =
  case mode of
    Idle -> [h2_ [] ["Select an alias or create New"]]
    Editing name eith ->
      case Map.lookup name aliases of
        Nothing ->
          [ h2_ [] ["Alias not found"]
          , div_
              [class_ "field is-grouped"]
              [ div_
                  [class_ "control"]
                  [ button_
                      [ class_ "button is-link"
                      , onClick $ startCreatingAlias $ Just name
                      ]
                      ["Create"]
                  ]
              , div_
                  [class_ "control"]
                  [ button_
                      [ class_ "button is-link is-light"
                      , onClick setIdle
                      ]
                      ["Cancel"]
                  ]
              ]
          ]
        Just aliasInfo -> renderAlias name aliasInfo eith
    CreatingNewAlias name malias ->
      let aliasName = parseAliasName name
          isOkName = isRight aliasName && either (const False) (not . flip Map.member m.aliases) aliasName
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
  | SaveAlias !AliasName !Alias
  | RegisterAlias !AliasName !Alias
  | SetNewAliasName !MisoString
  | SetNewAliasUrl !MisoString
  | UpdateEditingUrl !MisoString
  | CopyUrl
  | ChangeWindowUrl !URI
  deriving (Show, Eq, Ord, Generic)
