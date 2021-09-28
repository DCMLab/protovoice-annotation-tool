module App.Tabs where

import Prelude
import App.Common (AppSettings, AppSlots, AppState, GraphAction(..), ImportThing(..), Tab(..), ImportOutput, defaultSettings)
import App.Render (class_)
import App.Utils (convertMusicXML, copyToClipboard, examplePiece, examplePieceLong, showJSONErrors)
import DOM.HTML.Indexed.InputAcceptType (InputAcceptTypeAtom(..))
import Data.Either (Either(..), either)
import Data.List as L
import Data.List.NonEmpty (NonEmptyList)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Number (fromString)
import Data.String (Pattern(..), stripSuffix)
import Data.Tuple (Tuple(..))
import Effect.Aff.Class (class MonadAff)
import Effect.Class (class MonadEffect, liftEffect)
import Foreign (ForeignError)
import Halogen (liftAff)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import ProtoVoices.Folding (reductionToLeftmost)
import ProtoVoices.JSONTransport (ModelJSON, addJSONIds, modelFromJSON, modelToJSON, pieceFromJSON, pieceToJSON, stripJSONIds, writeJSONPretty)
import ProtoVoices.Model (Model, Piece, loadPiece)
import ProtoVoices.Validation (validateReduction, validationIsOk)
import Simple.JSON (readJSON)
import Simple.JSON as JSON
import Type.Proxy (Proxy(..))
import Web.DownloadJs (download)
import Web.File.File (File, name, toBlob) as File
import Web.File.FileReader.Aff (readAsText) as File
import Web.HTML (window)
import Web.HTML.Window (alert, localStorage)
import Web.Storage.Storage as WStore

--------------------
-- sub-components --
--------------------
tabHandle :: forall r p. { tab :: Maybe Tab | r } -> Tab -> String -> HH.HTML p GraphAction
tabHandle st tab name =
  HH.li [ class_ $ "pure-menu-item" <> if st.tab == Just tab then " pure-menu-selected" else "" ]
    [ HH.a
        [ class_ $ "pure-menu-link"
        , HE.onClick \_ -> SwitchTab if st.tab == Just tab then Nothing else Just tab
        , HP.href "javascript:;"
        ]
        [ HH.text name ]
    ]

renderTabs :: forall m. MonadEffect m => MonadAff m => AppState -> HH.ComponentHTML GraphAction AppSlots m
renderTabs st =
  HH.div_
    [ HH.div [ class_ "pure-menu pure-menu-horizontal" ]
        [ HH.ul [ class_ "pure-menu-list" ]
            [ tabHandle st ImportTab "Import"
            , tabHandle st ExportTab "Export"
            , tabHandle st HelpTab "Help"
            , tabHandle st SettingsTab "Settings"
            , tabHandle st DebugTab "Debug"
            ]
        ]
    , case st.tab of
        Nothing -> HH.text ""
        Just HelpTab -> helpText
        Just ImportTab -> HH.slot (Proxy :: Proxy "importTab") 1 importComponent unit HandleImport
        Just ExportTab -> HH.slot_ (Proxy :: Proxy "exportTab") 0 exportComponent { model: st.model, name: st.name }
        Just SettingsTab -> HH.slot (Proxy :: Proxy "settingsTab") 2 settingsComponent st.settings HandleSettings
        Just DebugTab -> debugComponent st
    ]

-- help text
-- ---------
helpText :: forall p. HH.HTML p GraphAction
helpText =
  HH.div [ class_ "tab" ]
    [ HH.h2_ [ HH.text "Manual" ]
    , HH.p_ [ HH.text "This app can be used to create protovoice analyses of pieces." ]
    , HH.p_ [ HH.text "Load pieces or analyses using the Import tab. Save analyses using the Export tab." ]
    , HH.p_
        [ HH.text "A piece is analyzed by reducing it using outer structure operations. "
        , HH.text "Reducing a slice creates a 'split' operation, reducing a transition creates a horizontalization. "
        , HH.text "Select a top-level slice or transition and press Enter to reduce it. "
        , HH.text "Pressing Backspace removes the operation below the selected slice or transition, if possible. "
        ]
    , HH.p_
        [ HH.text "The inner structure is determined by providing an \"explanation\" for every note. "
        , HH.text "This done by selecting the parent(s) of a note: "
        , HH.text "Click on a note in a slice below the top-level to select it. "
        , HH.text "Then hold CTRL and click on a note in one of its parent slices. "
        , HH.text "For notes that result from a split, you also have to provide the type of operation. "
        , HH.text "This is automatically determined from to the selected pitches, but can be overriden if desired. "
        , HH.text "Split notes can have either one (left/right ornament) or two parents (double-parent ornament). "
        , HH.text "Double-parent ornaments create new \"mandatory edges\", which must be used in the next reduction step."
        , HH.text "Correctly reduced notes are shown in grey, unreduced notes are shown in black, and inconsistencies are shown in orange or red."
        ]
    ]

-- settings component
-- ------------------
data SettingsAction
  = SettingsToggleFlatHori
  | SettingsToggleShowAllEdges
  | SettingsToggleShowScore
  | SettingsSetXScale String
  | SettingsSetYScale String

settingsComponent :: forall query m. H.Component query AppSettings AppSettings m
settingsComponent = H.mkComponent { initialState, render, eval: H.mkEval H.defaultEval { handleAction = handleOptAction } }
  where
  initialState settings = { settings }

  render { settings } =
    HH.div [ class_ "tab pure-form" ]
      [ HH.p_
          [ HH.input
              [ HP.type_ $ HP.InputCheckbox
              , HP.checked settings.flatHori
              , HE.onChange \_ -> SettingsToggleFlatHori
              , HP.id "flatHori"
              ]
          , HH.label [ HP.for "flatHori" ] [ HH.text " render horis flat" ]
          ]
      , HH.p_
          [ HH.input
              [ HP.type_ $ HP.InputCheckbox
              , HP.checked settings.showAllEdges
              , HE.onChange \_ -> SettingsToggleShowAllEdges
              , HP.id "showAllEdges"
              ]
          , HH.label [ HP.for "showAllEdges" ] [ HH.text " show all edges" ]
          ]
      , HH.p_
          [ HH.input
              [ HP.type_ $ HP.InputCheckbox
              , HP.checked settings.showScore
              , HE.onChange \_ -> SettingsToggleShowScore
              , HP.id "showScore"
              ]
          , HH.label [ HP.for "showScore" ] [ HH.text " show score" ]
          ]
      , HH.p [ class_ "pure-g" ]
          [ HH.label [ class_ "pure-u-1-5", HP.for "xscale" ] [ HH.text $ "horizontal zoom: " <> show settings.xscale ]
          , HH.input
              [ class_ "pure-u-3-5"
              , HP.type_ $ HP.InputRange
              , HP.min (-5.0)
              , HP.max 0.0
              , HP.step $ HP.Step 0.01
              , HP.value $ show settings.xscale
              , HE.onValueChange SettingsSetXScale
              , HP.name "xscale"
              ]
          , HH.div [ class_ "pure-u-1-5" ] [ HH.button [ class_ "pure-button center", HE.onClick $ \_ -> SettingsSetXScale $ show defaultSettings.xscale ] [ HH.text "Reset" ] ]
          ]
      , HH.p [ class_ "pure-g" ]
          [ HH.label [ class_ "pure-u-1-5", HP.for "yscale" ] [ HH.text $ "yscale: " <> show settings.yscale ]
          , HH.input
              [ class_ "pure-u-3-5"
              , HP.type_ $ HP.InputRange
              , HP.min (-2.0)
              , HP.max 2.0
              , HP.step $ HP.Step 0.01
              , HP.value $ show settings.yscale
              , HE.onValueChange SettingsSetYScale
              , HP.name "vertical zoom"
              ]
          , HH.div [ class_ "pure-u-1-5" ] [ HH.button [ class_ "pure-button center", HE.onClick $ \_ -> SettingsSetYScale $ show defaultSettings.yscale ] [ HH.text "Reset" ] ]
          ]
      ]

  handleOptAction msg = do
    case msg of
      SettingsToggleFlatHori -> H.modify_ \st -> st { settings { flatHori = not st.settings.flatHori } }
      SettingsToggleShowAllEdges -> H.modify_ \st -> st { settings { showAllEdges = not st.settings.showAllEdges } }
      SettingsToggleShowScore -> H.modify_ \st -> st { settings { showScore = not st.settings.showScore } }
      SettingsSetXScale s -> case fromString s of
        Nothing -> pure unit
        Just n -> H.modify_ \st -> st { settings { xscale = n } }
      SettingsSetYScale s -> case fromString s of
        Nothing -> pure unit
        Just n -> H.modify_ \st -> st { settings { yscale = n } }
    H.raise =<< H.gets _.settings

-- import component
-- ----------------
data ImportAction
  = ImportUpdateModelInput String
  | ImportUpdatePieceInput String
  | ImportUpdateName String
  | ImportUpdateMusicXMLInput String
  | ImportToggleMusicXMLUnfold
  | ImportUploadModel (Maybe File.File)
  | ImportUploadPiece (Maybe File.File)
  | ImportUploadMusicXML (Maybe File.File)
  | ImportLoadPiece String Piece
  | ImportLoadModel String Model
  | ImportConvertMusicXML String
  | ImportRestoreAutosave

importComponent :: forall query input m. MonadAff m => H.Component query input ImportOutput m
importComponent = H.mkComponent { initialState, render, eval: H.mkEval H.defaultEval { handleAction = handleImportAction } }
  where
  initialState _ =
    { modelText: ""
    , pieceText: ""
    , musicXMLText: ""
    , musicXMLUnfold: true
    , name: "unnamed"
    }

  render { modelText, pieceText, name, musicXMLText, musicXMLUnfold } =
    HH.div [ class_ "tab" ]
      [ HH.button [ class_ "pure-button", HE.onClick $ \_ -> ImportLoadPiece "example" examplePiece ] [ HH.text "Load Example" ]
      , HH.button [ class_ "pure-button", HE.onClick $ \_ -> ImportLoadPiece "example-long" examplePieceLong ] [ HH.text "Load Example (Long)" ]
      , HH.button [ class_ "pure-button pure-button-primary", HE.onClick $ \_ -> ImportRestoreAutosave ] [ HH.text "Restore Autosave Data" ]
      , HH.div_
          [ HH.p [ class_ "pure-g" ]
              [ HH.label [ HP.for "piece-name", class_ "pure-u-1-4" ] [ HH.text "Piece Name:" ]
              , HH.input
                  [ HP.type_ HP.InputText
                  , class_ "pure-u-1-2"
                  , HP.id "piece-name"
                  , HP.value name
                  , HE.onValueInput ImportUpdateName
                  ]
              ]
          , HH.h3_ [ HH.text "Import Piece" ]
          , HH.div_
              [ HH.label [ HP.for "upload-piece" ] [ HH.text "Choose a file: " ]
              , HH.input
                  [ HP.type_ HP.InputFile
                  , HP.id "upload-piece"
                  , HP.accept $ HP.InputAcceptType [ AcceptFileExtension ".piece.json" ]
                  , HE.onFileUpload \files -> ImportUploadPiece $ L.head files
                  ]
              ]
          , HH.p_ [ HH.text "or enter JSON directly:" ]
          , HH.textarea [ HP.value pieceText, HE.onValueInput ImportUpdatePieceInput ]
          , if pieceText == "" then
              HH.text ""
            else case pieceEither of
              Left err -> HH.p [ class_ "alert" ] [ HH.text $ "Invalid input:  ", HH.pre_ [ HH.text err ] ]
              Right model -> case model of
                Left modelWithNewIds ->
                  HH.div_
                    [ HH.p_ [ HH.text "Input valid, but no (or not all) IDs were given. Use generated IDs?" ]
                    , HH.button [ class_ "pure-button pure-button-primary", HE.onClick $ const $ ImportLoadModel name modelWithNewIds ]
                        [ HH.text "Import Piece (New IDs)" ]
                    ]
                Right modelWithGivenIds ->
                  HH.div_
                    [ HH.p_ [ HH.text "Input valid!" ]
                    , HH.button [ class_ "pure-button pure-button-primary", HE.onClick $ const $ ImportLoadModel name modelWithGivenIds ]
                        [ HH.text "Import Piece" ]
                    ]
          ]
      , HH.div_
          [ HH.h3_ [ HH.text "Import MusicXML" ]
          , HH.div_
              [ HH.label [ HP.for "upload-musicxml" ] [ HH.text "Choose a file: " ]
              , HH.input
                  [ HP.type_ HP.InputFile
                  , HP.id "upload-musicxml"
                  , HP.accept $ HP.InputAcceptType [ AcceptFileExtension ".musicxml" ]
                  , HE.onFileUpload \files -> ImportUploadMusicXML $ L.head files
                  ]
              ]
          , HH.p_ [ HH.text "or enter MusicXML directly:" ]
          , HH.textarea [ HP.value musicXMLText, HE.onValueInput ImportUpdateMusicXMLInput ]
          , HH.p_
              [ HH.input
                  [ HP.type_ $ HP.InputCheckbox
                  , HP.checked musicXMLUnfold
                  , HE.onChange \_ -> ImportToggleMusicXMLUnfold
                  , HP.id "unfoldReps"
                  ]
              , HH.label [ HP.for "unfoldReps" ] [ HH.text " unfold repetitions" ]
              ]
          , if musicXMLText == "" then
              HH.text ""
            else
              HH.div_
                [ HH.button
                    [ class_ "pure-button pure-button-primary", HE.onClick $ const $ ImportConvertMusicXML musicXMLText ]
                    [ HH.text "Convert MusicXML to Piece JSON" ]
                ]
          ]
      , HH.div_
          [ HH.h3_ [ HH.text "Import Analysis" ]
          , HH.div_
              [ HH.label [ HP.for "upload-analysis" ] [ HH.text "Choose a file: " ]
              , HH.input
                  [ HP.type_ HP.InputFile
                  , HP.id "upload-analysis"
                  , HP.accept $ HP.InputAcceptType [ AcceptFileExtension ".analysis.json" ]
                  , HE.onFileUpload \files -> ImportUploadModel $ L.head files
                  ]
              ]
          , HH.p_ [ HH.text "or enter JSON directly:" ]
          , HH.textarea [ HP.value modelText, HE.onValueInput ImportUpdateModelInput ]
          , if modelText == "" then
              HH.text ""
            else case modelEither of
              Left err -> HH.p [ class_ "alert" ] [ HH.text $ "Invalid input:  ", HH.pre_ [ HH.text err ] ]
              Right model ->
                HH.div_
                  [ HH.p_ [ HH.text "Input valid!" ]
                  , HH.button [ class_ "pure-button pure-button-primary", HE.onClick $ const $ ImportLoadModel name model ]
                      [ HH.text "Import Analysis" ]
                  ]
          ]
      ]
    where
    pieceEither = case readJSON pieceText of
      Right json -> case pieceFromJSON json of
        Just piece -> Right $ Right $ loadPiece piece
        Nothing -> Left "Invalid piece!"
      Left errs1 -> case readJSON pieceText of
        Right jsonNoIds -> case pieceFromJSON (addJSONIds jsonNoIds) of
          Just pieceNewIds -> Right $ Left $ loadPiece pieceNewIds
          Nothing -> Left "Invalid piece!"
        Left errs2 -> showJSONErrors (if errs1 == errs2 then errs1 else errs1 <> errs2)

    modelEither = either showJSONErrors modelFromJSON $ modeljson
      where
      modeljson :: Either (NonEmptyList ForeignError) ModelJSON
      modeljson = readJSON modelText

  handleImportAction = case _ of
    ImportUpdateModelInput str -> H.modify_ \st -> st { modelText = str }
    ImportUpdatePieceInput str -> H.modify_ \st -> st { pieceText = str }
    ImportUpdateMusicXMLInput str -> H.modify_ \st -> st { musicXMLText = str }
    ImportToggleMusicXMLUnfold -> H.modify_ \st -> st { musicXMLUnfold = not st.musicXMLUnfold }
    ImportUpdateName str -> H.modify_ \st -> st { name = str }
    ImportUploadModel f -> loadFile f ".analysis.json" ImportUpdateModelInput
    ImportUploadPiece f -> loadFile f ".piece.json" ImportUpdatePieceInput
    ImportUploadMusicXML f -> loadFile f ".musicxml" ImportUpdateMusicXMLInput
    ImportLoadPiece n p -> H.raise $ { name: n, thing: ImportPiece p }
    ImportLoadModel n m -> H.raise $ { name: n, thing: ImportModel m }
    ImportConvertMusicXML xml -> do
      unfold <- H.gets _.musicXMLUnfold
      res <- liftAff $ convertMusicXML unfold xml
      w <- liftEffect $ window
      case res of
        Left err -> liftEffect $ alert ("Error converting file:" <> err) w
        Right str -> handleImportAction $ ImportUpdatePieceInput str
    ImportRestoreAutosave -> do
      w <- liftEffect window
      s <- liftEffect $ localStorage w
      autodata <- liftEffect $ WStore.getItem "autosave" s
      case autodata of
        Nothing -> liftEffect $ alert "No autosave data found." w
        Just str -> case either showJSONErrors modelFromAutosave $ readJSON str of
          Left err -> liftEffect $ alert ("Error loading autosave data: " <> err) w
          Right { model, name } -> H.raise $ { name, thing: ImportModel model }
    where
    loadFile f ext action = case f of
      Nothing -> pure unit
      Just file -> do
        str <- H.liftAff $ File.readAsText $ File.toBlob file
        let
          fname = File.name file

          name = fromMaybe fname $ stripSuffix (Pattern ext) fname
        handleImportAction $ action str
        H.modify_ \st -> st { name = name }

    modelFromAutosave ::
      { name :: String, model :: ModelJSON } ->
      Either String { name :: String, model :: Model }
    modelFromAutosave { name, model } = do
      model' <- modelFromJSON model
      pure { name, model: model' }

-- export component
-- ----------------
type ExportInput
  = { model :: (Maybe Model), name :: String }

data ExportAction
  = CopyToClipboard String
  | DownloadJSON String String
  | TogglePretty
  | Receive ExportInput

exportComponent :: forall query output m. MonadEffect m => H.Component query ExportInput output m
exportComponent =
  H.mkComponent
    { initialState
    , render
    , eval:
        H.mkEval
          H.defaultEval
            { handleAction = handleExportAction
            , receive = Just <<< Receive
            }
    }
  where
  initialState { model, name } = { model, name, pretty: false }

  render { model: modelMaybe, pretty, name } = case modelMaybe of
    Nothing -> HH.text ""
    Just model ->
      let
        json = modelToJSON model

        jsonStrOrErr = (if pretty then writeJSONPretty else JSON.writeJSON) <$> json

        val = validateReduction model.reduction

        reLoad = modelFromJSON =<< json
      in
        HH.div [ class_ "tab" ]
          [ if validationIsOk val then
              HH.text ""
            else
              HH.p [ class_ "alert" ] [ HH.text "Warning: reduction is incomplete and/or contains errors." ]
          , HH.h3_ [ HH.text "JSON String" ]
          , case jsonStrOrErr of
              Right jsonStr ->
                HH.div_
                  [ HH.div [ class_ "pure-g" ]
                      [ HH.div [ class_ "pure-u-3-5" ]
                          [ HH.input
                              [ HP.type_ $ HP.InputCheckbox
                              , HP.checked pretty
                              , HE.onChange \_ -> TogglePretty
                              , HP.id "prettyJSON"
                              ]
                          , HH.label [ HP.for "prettyJSON" ] [ HH.text " pretty" ]
                          ]
                      , HH.button
                          [ class_ "pure-button pure-u-1-5"
                          , HE.onClick \_ -> CopyToClipboard jsonStr
                          ]
                          [ HH.text "Copy to Clipboard" ]
                      , HH.button
                          [ class_ "pure-button pure-button-primary pure-u-1-5"
                          , HE.onClick \_ -> DownloadJSON (name <> ".analysis.json") jsonStr
                          ]
                          [ HH.text "Download" ]
                      ]
                  , HH.pre_ [ HH.text $ jsonStr ]
                  ]
              Left err -> HH.p [ class_ "alert" ] [ HH.text $ "Cannot serialize reduction to JSON:" <> err ]
          , HH.div_ case reLoad of
              Left err -> [ HH.text $ "Error re-reading reduction: " <> err ]
              Right re ->
                if re == model then
                  [ HH.text "roundtrip ok" ]
                else if re.piece /= model.piece then
                  [ HH.text "pieces are different! original"
                  , HH.pre_ [ HH.text $ show model.piece ]
                  , HH.text "re-read:"
                  , HH.pre_ [ HH.text $ show re.piece ]
                  ]
                else case L.find (\(Tuple a b) -> a /= b) $ L.zip model.reduction.segments re.reduction.segments of
                  Nothing ->
                    [ HH.text "roundtrip not ok (but can't find wrong segment)! original:"
                    , HH.pre_ [ HH.text $ show model ]
                    , HH.text "re-read:"
                    , HH.pre_ [ HH.text $ show re ]
                    ]
                  Just (Tuple sorig sre) ->
                    [ HH.text "roundtrip not ok! offending segment original: "
                    , HH.pre_ [ HH.text $ show sorig ]
                    , HH.text " re-read:"
                    , HH.pre_ [ HH.text $ show sre ]
                    ]
          ]

  handleExportAction = case _ of
    CopyToClipboard str -> liftEffect $ copyToClipboard str
    DownloadJSON filename json -> do
      _ <- liftEffect $ download json filename "application/json"
      pure unit
    TogglePretty -> H.modify_ \st -> st { pretty = not st.pretty }
    Receive { model, name } -> H.modify_ \st -> st { model = model, name = name }

-- debug component
-- ---------------
debugComponent :: forall p. AppState -> HH.HTML p GraphAction
debugComponent st =
  HH.div [ class_ "tab" ]
    [ HH.h3_ [ HH.text "Selection" ]
    , HH.p_
        [ HH.text "Selection: "
        , HH.text $ show st.selected
        ]
    , HH.h3_ [ HH.text "Settings" ]
    , HH.p_ [ HH.text $ show st.settings ]
    , HH.h3_ [ HH.text "Validation" ]
    , case st.model of
        Nothing -> HH.text "No active reduction."
        Just model -> HH.p_ [ HH.text $ show $ validateReduction model.reduction ]
    , HH.h3_ [ HH.text "Reduction Steps" ]
    , case st.model of
        Nothing -> HH.text "No active reduction."
        Just model -> case reductionToLeftmost model of
          Right steps -> HH.ol_ $ map (\step -> HH.li_ [ HH.text $ show step ]) steps
          Left err -> HH.p [ class_ "alert" ] [ HH.text $ "Warning: reduction cannot be turned into leftmost derivation: " <> err ]
    , HH.h3_ [ HH.text "Piece JSON" ]
    , case st.model of
        Nothing -> HH.text "No active piece."
        Just model ->
          HH.div_
            [ HH.p_ [ HH.text "with IDs:" ]
            , HH.pre_ [ HH.text $ JSON.writeJSON $ pieceToJSON model.piece ]
            , HH.p_ [ HH.text "without IDs:" ]
            , HH.pre_ [ HH.text $ JSON.writeJSON $ stripJSONIds $ pieceToJSON model.piece ]
            ]
    ]
