module App.Tabs.Import where

import Prelude

import App.Common (ImportOutput, ImportThing(..))
import App.Utils (convertMusicXML, examplePiece, examplePieceLong, showJSONErrors, class_)
import DOM.HTML.Indexed.InputAcceptType (InputAcceptTypeAtom(..))
import Data.Either (Either(..), either)
import Data.List as L
import Data.List.NonEmpty (NonEmptyList)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.String (Pattern(..), stripSuffix)
import Effect.Aff.Class (class MonadAff)
import Effect.Class (liftEffect)
import Foreign (ForeignError)
import Halogen (liftAff)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import ProtoVoices.JSONTransport (ModelJSON, addJSONIds, modelFromJSON, pieceFromJSON)
import ProtoVoices.Model (Model, Piece, Staff(..), loadPiece)
import Simple.JSON (readJSON)
import Web.File.File (File, name, toBlob) as File
import Web.File.FileReader.Aff (readAsText) as File
import Web.HTML (window)
import Web.HTML.Window (alert, localStorage)
import Web.Storage.Storage as WStore

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
  -- | ImportLoadSurface String
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
    HH.div [ class_ "content-np tab" ]
      [ HH.button
          [ class_ "pure-button"
          , HE.onClick $ \_ ->
              let
                model = loadPiece examplePiece
              in
                ImportLoadModel "example" $ model { styles { staff = TrebleStaff } }
          ]
          [ HH.text "Load Example" ]
      , HH.button
          [ class_ "pure-button"
          , HE.onClick $ \_ -> ImportLoadPiece "example-long" examplePieceLong
          ]
          [ HH.text "Load Example (Long)" ]
      , HH.button
          [ class_ "pure-button pure-button-primary"
          , HE.onClick $ \_ -> ImportRestoreAutosave
          ]
          [ HH.text "Restore Autosave Data" ]
      , HH.p [ class_ "pure-g" ]
          [ HH.label [ HP.for "piece-name", class_ "pure-u-1-4" ] [ HH.text "Piece Name:" ]
          , HH.input
              [ HP.type_ HP.InputText
              , class_ "pure-u-1-2"
              , HP.id "piece-name"
              , HP.value name
              , HE.onValueInput ImportUpdateName
              ]
          ]
      -- , HH.button
      --     [ class_ "pure-button pure-button-primary"
      --     , HE.onClick $ \_ -> ImportLoadSurface name
      --     ]
      --     [ HH.text "Restart from Current Surface" ]
      , HH.div_
          [ HH.h3_ [ HH.text "Import Piece" ]
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
    -- ImportLoadSurface n -> H.raise $ { name: n, thing: ImportCurrentSurface }
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

    modelFromAutosave
      :: { name :: String, model :: ModelJSON }
      -> Either String { name :: String, model :: Model }
    modelFromAutosave { name, model } = do
      model' <- modelFromJSON model
      pure { name, model: model' }
