module Try.Container where

import Prelude

import Ace (Annotation)
import CSS as CSS
import CSS.Common as CSSC
import CSS.Flexbox as FB
import CSS.Overflow as CSSO
import Control.Monad.Except (runExceptT)
import Data.Array (fold)
import Data.Array as Array
import Data.Either (Either(..), hush)
import Data.Foldable (for_, oneOf)
import Data.FoldableWithIndex (foldMapWithIndex)
import Data.Int (toNumber)
import Data.Int as I
import Data.Maybe (Maybe(..), fromMaybe, isJust, isNothing, maybe)
import Data.Monoid (guard)
import Data.Number as N
import Data.String.Base64 (encode)
import Data.Symbol (SProxy(..))
import Effect (Effect)
import Effect.Aff (Aff, makeAff)
import Effect.Aff as Aff
import Effect.Class.Console (error)
import Effect.Uncurried (EffectFn3, runEffectFn3)
import Foreign.Object (Object)
import Foreign.Object as Object
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.CSS as HCSS
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Try.API (CompileError(..), CompileResult(..), CompilerError, ErrorPosition)
import Try.API as API
import Try.Config as Config
import Try.Editor (MarkerType(..), toStringMarkerType)
import Try.Editor as Editor
import Try.Gist (getGistById, tryLoadFileFromGist)
import Try.GitHub (getRawGitHubFile)
import Try.Loader (Loader, makeLoader, runLoader)
import Try.QueryString (getQueryStringMaybe)
import Try.Session (createSessionIdIfNecessary, storeSession, tryRetrieveSession)
import Try.Types (JS(..))
import Try.Uploader (makeUploadLink)
import Try.Url (getRawB64Url)
import Web.HTML (window)
import Web.HTML.Window (alert)

type Slots = (editor :: Editor.Slot Unit)

data SourceFile = GitHub String | Gist String | Url String

type Settings =
  { autoCompile :: Boolean
  , showJs :: Boolean
  , wagsEmbed :: Boolean
  , viewMode :: ViewMode
  , iframeDim :: CSS.Size CSS.Abs
  }

defaultSettings :: Settings
defaultSettings =
  { autoCompile: true
  , showJs: false
  , viewMode: SideBySide
  , wagsEmbed: false
  , iframeDim: CSSC.auto
  }

type State =
  { settings :: Settings
  , uploadLink :: Maybe String
  , sourceFile :: Maybe SourceFile
  , compiled :: Maybe (Either String CompileResult)
  }

data ViewMode
  = SideBySide
  | Code
  | Output

derive instance eqViewMode :: Eq ViewMode

parseViewModeParam :: String -> Maybe ViewMode
parseViewModeParam = case _ of
  "sidebyside" -> Just SideBySide
  "code" -> Just Code
  "output" -> Just Output
  _ -> Nothing

data Action
  = Initialize
  | Cache String
  | UpdateSettings (Settings -> Settings)
  | Compile (Maybe String)
  | HandleEditor Editor.Output
  | GenerateUrl

_editor :: SProxy "editor"
_editor = SProxy

loader :: Loader
loader = makeLoader Config.loaderUrl

type LoadCb = Effect Unit
type FailCb = Effect Unit

foreign import setupIFrame :: EffectFn3 (Object JS) LoadCb FailCb Unit
foreign import teardownIFrame :: Effect Unit

component :: forall q i o. H.Component HH.HTML q i o Aff
component = H.mkComponent
  { initialState
  , render
  , eval: H.mkEval $ H.defaultEval
      { handleAction = handleAction
      , initialize = Just Initialize
      }
  }
  where
  initialState :: i -> State
  initialState _ =
    { settings: defaultSettings
    , sourceFile: Nothing
    , compiled: Nothing
    , uploadLink: Nothing
    }

  handleAction :: Action -> H.HalogenM State Action Slots o Aff Unit
  handleAction = case _ of
    Initialize -> do
      -- make wags embed appear immediately
      wagsEmbed <- map isJust <$> H.liftEffect $ getQueryStringMaybe "wags"
      iframeDim <- H.liftEffect $ getQueryStringMaybe "idim" <#> case _ of
        Just "auto" -> CSSC.auto
        Just a -> case I.fromString a of
          Just i -> CSS.px (toNumber i)
          Nothing -> case N.fromString a of
            Just i -> CSS.px i
            Nothing -> CSSC.auto
        Nothing -> CSSC.auto
      H.modify_ \s -> s { settings = s.settings { wagsEmbed = wagsEmbed, iframeDim = iframeDim } }
      sessionId <- H.liftEffect $ createSessionIdIfNecessary
      { code, sourceFile } <- H.liftAff $ withSession sessionId

      -- Load parameters
      mbViewModeParam <- H.liftEffect $ getQueryStringMaybe "view"
      let viewMode = fromMaybe SideBySide $ parseViewModeParam =<< mbViewModeParam

      mbShowJsParam <- H.liftEffect $ getQueryStringMaybe "js"
      let showJs = mbShowJsParam == Just "true"

      mbAutoCompile <- H.liftEffect $ getQueryStringMaybe "compile"
      let autoCompile = mbAutoCompile /= Just "false"

      H.modify_ _
        { settings = { viewMode, showJs, autoCompile, wagsEmbed, iframeDim }
        , sourceFile = sourceFile
        }

      -- Set the editor contents. This will trigger a change event, causing a
      -- cache + compile step.
      void $ H.query _editor unit $ H.tell $ Editor.SetEditorContent code

    UpdateSettings k -> do
      old <- H.get
      new <- H.modify \state -> state { settings = k state.settings }
      when (old.settings.showJs /= new.settings.showJs) do
        if new.settings.showJs then
          H.liftEffect teardownIFrame
        else
          handleAction $ Compile Nothing

    Cache text -> H.liftEffect do
      sessionId <- getQueryStringMaybe "session"
      case sessionId of
        Just sessionId_ -> do
          storeSession sessionId_ { code: text }
        Nothing ->
          error "No session ID"

    Compile mbCode -> do
      H.modify_ _ { compiled = Nothing }
      code <- case mbCode of
        Nothing -> do
          mbText <- H.query _editor unit $ H.request $ Editor.GetEditorContent
          pure $ fold $ join mbText
        Just text ->
          pure text
      _ <- H.query _editor unit $ H.tell $ Editor.SetAnnotations []
      _ <- H.query _editor unit $ H.tell $ Editor.RemoveMarkers
      runExceptT (API.compile Config.compileUrl code) >>= case _ of
        Left err -> do
          H.liftEffect teardownIFrame
          H.modify_ _ { compiled = Just (Left err) }

        Right (Left err) -> do
          H.liftEffect teardownIFrame
          H.liftEffect $ error err
          H.modify_ _ { compiled = Just (Left err) }

        Right (Right res@(CompileFailed { error })) -> do
          H.liftEffect teardownIFrame
          H.modify_ _ { compiled = Just (Right res) }
          case error of
            OtherError _ ->
              pure unit
            CompilerErrors errs -> do
              let anns = Array.mapMaybe (toAnnotation MarkerError) errs
              _ <- H.query _editor unit $ H.tell $ Editor.SetAnnotations anns
              for_ errs \{ position } ->
                for_ position \pos -> do
                  _ <- H.query _editor unit $ H.tell $ Editor.AddMarker MarkerError pos
                  pure unit

        Right (Right res@(CompileSuccess { js, warnings })) -> do
          { settings } <- H.get
          if settings.showJs then
            H.liftEffect teardownIFrame
          else do
            mbSources <- H.liftAff $ map hush $ runExceptT $ runLoader loader (JS js)
            for_ warnings \warnings_ -> do
              let anns = Array.mapMaybe (toAnnotation MarkerWarning) warnings_
              _ <- H.query _editor unit $ H.tell $ Editor.SetAnnotations anns
              pure unit
            for_ mbSources \sources -> do
              let eventData = Object.insert "<file>" (JS js) sources
              H.liftAff $ makeAff \f -> do
                runEffectFn3 setupIFrame eventData (f (Right unit)) (f (Left $ Aff.error "Could not load iframe"))
                mempty
            H.modify_ _ { compiled = Just (Right res) }

    HandleEditor (Editor.TextChanged text) -> do
      _ <- H.fork $ handleAction $ Cache text
      { autoCompile } <- H.gets _.settings
      when autoCompile $ handleAction $ Compile $ Just text
    GenerateUrl -> do
      sessionId <- H.liftEffect $ getQueryStringMaybe "session"
      case sessionId of
        Just sessionId_ -> do
          state <- H.liftEffect $ tryRetrieveSession sessionId_
          case state of
            Just { code } -> do
              link <- makeUploadLink code
              H.modify_ _ { uploadLink = Just link }
            Nothing -> error "no code"
        Nothing ->
          error "No session ID"

  render :: State -> H.ComponentHTML Action Slots Aff
  render state =
    HH.div
      [ HP.id_ "wrapper" ]
      [ HH.div
          [ HP.id_ "body" ]
          ( ( guard (not state.settings.wagsEmbed)
                [ renderMenu
                , renderMobileBanner
                , HH.p [] [ HH.text (maybe "Click Generate URL to generate a url to this session" (\l -> ("Direct link: " <> "https://purescript-wags.netlify.app/?b64url=" <> encode l)) state.uploadLink) ]
                ]
            ) <>
              [ renderEditor ]
          )
      ]
    where
    renderMenu =
      HH.ul
        [ HP.id_ "menu" ]
        [ HH.a
            [ HP.class_ $ HH.ClassName "menu-item"
            , HP.id_ "home_link"
            , HP.href "/"
            , HP.title "Try PureScript!"
            ]
            [ HH.img
                [ HP.src "img/favicon-white.svg"
                , HP.width 40
                , HP.height 40
                ]
            ]
        , HH.li
            [ HP.class_ $ HH.ClassName "menu-item menu-dropdown no-mobile" ]
            [ HH.label
                [ HP.title "Select a view mode" ]
                [ HH.text "View Mode" ]
            , let
                name = "view_mode"
              in
                HH.ul
                  [ HP.id_ name ]
                  [ menuRadio
                      { checked: state.settings.viewMode == SideBySide
                      , name
                      , value: "sidebyside"
                      , id: "view_sidebyside"
                      , title: "Show the code and output side by side"
                      , label: "Side-by-side"
                      , onClick: UpdateSettings (_ { viewMode = SideBySide })
                      }
                  , menuRadio
                      { checked: state.settings.viewMode == Code
                      , name
                      , value: "code"
                      , id: "view_code"
                      , title: "Show only the code"
                      , label: "Code"
                      , onClick: UpdateSettings (_ { viewMode = Code })
                      }
                  , menuRadio
                      { checked: state.settings.viewMode == Output
                      , name
                      , value: "output"
                      , id: "view_output"
                      , title: "Show only the output"
                      , label: "Output"
                      , onClick: UpdateSettings (_ { viewMode = Output })
                      }
                  , maybeElem state.sourceFile \source ->
                      HH.li
                        [ HP.class_ $ HH.ClassName "view_sourcefile_li" ]
                        [ case source of
                            GitHub githubId ->
                              renderGitHubLink githubId
                            Gist gistId ->
                              renderGistLink gistId
                            Url url ->
                              renderUrl url
                        ]
                  ]
            ]
        , maybeElem state.sourceFile \source ->
            HH.li
              [ HP.class_ $ HH.ClassName "menu-item view_sourcefile_li mobile-only" ]
              [ case source of
                  GitHub githubId ->
                    renderGitHubLink githubId
                  Gist gistId ->
                    renderGistLink gistId
                  Url url ->
                    renderUrl url
              ]
        , HH.li
            [ HP.class_ $ HH.ClassName "menu-item no-mobile" ]
            [ HH.label
                [ HP.id_ "compile_label"
                , HP.title "Compile Now"
                , HE.onClick \_ -> Just (Compile Nothing)
                ]
                [ HH.text "Compile" ]
            ]
        , HH.li
            [ HP.class_ $ HH.ClassName "menu-item nowrap no-mobile" ]
            [ HH.input
                [ HP.id_ "auto_compile"
                , HP.name "auto_compile"
                , HP.title "Toggle auto-compilation of the file on code changes"
                , HP.value "auto_compile"
                , HP.type_ HP.InputCheckbox
                , HP.checked state.settings.autoCompile
                , HE.onChecked \bool -> Just $ UpdateSettings (_ { autoCompile = bool })
                ]
            , HH.label
                [ HP.id_ "auto_compile-label"
                , HP.for "auto_compile"
                , HP.title "Compile on code changes"
                ]
                [ HH.text "Auto-Compile" ]
            ]
        , HH.li
            [ HP.class_ $ HH.ClassName "menu-item nowrap" ]
            [ HH.input
                [ HP.id_ "showjs"
                , HP.name "showjs"
                , HP.title "Show resulting JavaScript code instead of output"
                , HP.value "showjs"
                , HP.type_ HP.InputCheckbox
                , HP.checked state.settings.showJs
                , HE.onChecked \bool -> Just $ UpdateSettings (_ { showJs = bool })
                ]
            , HH.label
                [ HP.id_ "showjs_label"
                , HP.for "showjs"
                , HP.title "Show resulting JavaScript code instead of output"
                ]
                [ HH.text "Show JS" ]
            ]
        , HH.li
            [ HP.class_ $ HH.ClassName "menu-item" ]
            [ HH.a
                [ HP.id_ "helplink"
                , HP.href "https://github.com/purescript/trypurescript/blob/master/README.md"
                , HP.target "trypurs_readme"
                ]
                [ HH.label
                    [ HP.id_ "help"
                    , HP.title "Learn more about Try PureScript"
                    ]
                    [ HH.text "Help" ]
                ]
            ]
        , HH.li
            [ HP.class_ $ HH.ClassName "menu-item" ]
            [ HH.button
                [ HP.id_ "genurl_button"
                , HE.onClick \_ -> Just GenerateUrl
                ]
                [ HH.label
                    [ HP.id_ "helgenurlp"
                    , HP.title "Generate URL to to the current session"
                    ]
                    [ HH.text "Generate URL" ]
                ]
            ]
        ]

    renderMobileBanner =
      HH.div
        [ HP.class_ $ HH.ClassName "mobile-only mobile-banner" ]
        [ HH.text "Your screen size is too small. Code editing has been disabled." ]

    renderEditor =
      HH.div
        [ HP.id_ "editor_view"
        , HCSS.style do
            CSS.display CSS.flex
            CSS.flexDirection (if state.settings.wagsEmbed then CSS.column else CSS.row)
            FB.flex 1 1 CSSC.auto
            CSS.marginTop (CSS.px 0.0)
            CSS.position CSS.relative
        , HP.attr (HH.AttrName "data-view-mode") case state.settings.viewMode of
            SideBySide -> "sidebyside"
            Code -> "code"
            Output -> "output"
        ]
        [ HH.div
            ( [ HP.id_ "column1"
              , HCSS.style do
                  when (state.settings.viewMode == Output) (CSS.display CSS.displayNone)
                  FB.flex 1 1 CSSC.auto
                  CSS.position CSS.relative

              ] <> (guard (not state.settings.wagsEmbed) [ HP.class_ $ HH.ClassName "no-mobile" ])
            )
            [ HH.slot _editor unit Editor.component unit (Just <<< HandleEditor) ]
        , HH.div
            [ HP.class_ $ HH.ClassName "separator" ]
            []
        , HH.div
            [ HP.id_ "column2_wrapper"
            , HCSS.style do
                when (state.settings.viewMode == Code) (CSS.display CSS.displayNone)
                (if state.settings.wagsEmbed then FB.flex 0 1 else FB.flex 1 1) state.settings.iframeDim
                CSS.position CSS.relative
                CSSO.overflow CSSO.overflowAuto
                (CSS.key $ CSS.fromString "-webkit-overflow-scrolling") "touch"
            ]
            [ HH.div
                [ HP.id_ "column2" ]
                [ maybeElem state.compiled renderCompiled ]
            , whenElem (isNothing state.compiled) \_ ->
                HH.div
                  [ HP.id_ "loading" ]
                  []
            ]
        ]

    renderCompiled = case _ of
      Left err ->
        renderPlaintext "Unable to parse the response from the server."
      Right res -> case res of
        CompileFailed { error } -> case error of
          OtherError err ->
            renderPlaintext err
          CompilerErrors errs ->
            HH.div_ $ renderCompilerErrors errs
        CompileSuccess { js } ->
          whenElem state.settings.showJs \_ ->
            renderPlaintext js

whenElem :: forall w i. Boolean -> (Unit -> HH.HTML w i) -> HH.HTML w i
whenElem cond f = if cond then f unit else HH.text ""

maybeElem :: forall w i a. Maybe a -> (a -> HH.HTML w i) -> HH.HTML w i
maybeElem val f = case val of
  Just x -> f x
  _ -> HH.text ""

renderPlaintext :: forall w i. String -> HH.HTML w i
renderPlaintext contents = HH.pre_ [ HH.code_ [ HH.text contents ] ]

renderCompilerErrors :: forall w i. Array CompilerError -> Array (HH.HTML w i)
renderCompilerErrors errors = do
  let total = Array.length errors
  errors # foldMapWithIndex \ix { message } ->
    [ HH.h1
        [ HP.class_ $ HH.ClassName "error-banner" ]
        [ HH.text $ "Error " <> show (ix + 1) <> " of " <> show total ]
    , renderPlaintext message
    ]

menuRadio
  :: forall w
   . { name :: String
     , id :: String
     , value :: String
     , checked :: Boolean
     , title :: String
     , label :: String
     , onClick :: Action
     }
  -> HH.HTML w Action
menuRadio props =
  HH.li_
    [ HH.input
        [ HP.type_ HP.InputRadio
        , HP.name props.name
        , HP.value props.value
        , HP.id_ props.id
        , HE.onClick \_ -> Just props.onClick
        , HP.checked props.checked
        ]
    , HH.label
        [ HP.for props.id
        , HP.title props.title
        ]
        [ HH.text props.label ]
    ]

renderGistLink :: forall w i. String -> HH.HTML w i
renderGistLink gistId =
  HH.a
    [ HP.href $ "https://gist.github.com/" <> gistId
    , HP.target "trypurs_gist"
    ]
    [ HH.label
        [ HP.title "Open the original gist in a new window." ]
        [ HH.text "Gist" ]
    ]

renderGitHubLink :: forall w i. String -> HH.HTML w i
renderGitHubLink githubId =
  HH.a
    [ HP.href $ "https://github.com/" <> githubId
    , HP.target "trypurs_github"
    ]
    [ HH.label
        [ HP.title "Open the original source file in a new window." ]
        [ HH.text "GitHub" ]
    ]

renderUrl :: forall w i. String -> HH.HTML w i
renderUrl url =
  HH.a
    [ HP.href $ url
    , HP.target "trypurs_url"
    ]
    [ HH.label
        [ HP.title "Open the original source file in a new window." ]
        [ HH.text "URL" ]
    ]

toAnnotation
  :: forall r
   . MarkerType
  -> { position :: Maybe ErrorPosition, message :: String | r }
  -> Maybe Annotation
toAnnotation markerType { position, message } =
  position <#> \pos ->
    { row: pos.startLine - 1
    , column: pos.startColumn - 1
    , type: toStringMarkerType markerType
    , text: message
    }

withSession :: String -> Aff { sourceFile :: Maybe SourceFile, code :: String }
withSession sessionId = do
  state <- H.liftEffect $ tryRetrieveSession sessionId
  githubId <- H.liftEffect $ getQueryStringMaybe "github"
  gistId <- H.liftEffect $ getQueryStringMaybe "gist"
  b64url <- H.liftEffect $ getQueryStringMaybe "b64url"
  code <- case state of
    Just { code } -> pure code
    Nothing -> do
      let
        action = oneOf
          [ map loadFromGitHub githubId
          , map loadFromGist gistId
          , map loadFromB64Url b64url
          ]
      fromMaybe (loadFromGitHub Config.mainGitHubExample) action
  let sourceFile = oneOf [ map GitHub githubId, map Gist gistId ]
  pure { sourceFile, code }
  where
  handleResult = case _ of
    Left err -> do
      H.liftEffect $ window >>= alert err
      pure ""
    Right code ->
      pure code

  loadFromGist id =
    runExceptT (getGistById id >>= \gi -> tryLoadFileFromGist gi "Main.purs") >>= handleResult

  loadFromGitHub id =
    runExceptT (getRawGitHubFile id) >>= handleResult

  loadFromB64Url b64url =
    runExceptT (getRawB64Url b64url) >>= handleResult
