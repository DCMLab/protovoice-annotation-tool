module Main where

import Prelude
import CommonApp (GraphAction(..), ImportOutput(..), Selection(..), Tab(..), getSelSlice, getSelTrans, outerSelected)
import Control.Monad.State (class MonadState)
import Data.Either (Either(..), either)
import Data.Foldable (for_, intercalate)
import Data.List as L
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.Class (class MonadEffect, liftEffect)
import Effect.Class.Console (log)
import Folding (evalGraph, reductionToLeftmost)
import Foreign (renderForeignError)
import Halogen as H
import Halogen.Aff as HA
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.Query.Event (eventListener)
import Halogen.VDom.Driver (runUI)
import JSONTransport (reductionFromJSON, reductionToJSON)
import Model (Model, Piece, loadPiece, mergeAtSlice, noteSetExplanation, undoMergeAtTrans, undoVertAtSlice, vertAtMid)
import Render (class_, renderNoteExplanation, renderReduction)
import Simple.JSON (readJSON)
import Simple.JSON as JSON
import Type.Proxy (Proxy(..))
import Utils (addJSONIds, copyToClipboard, examplePiece, examplePieceLong, pieceFromJSON, writeJSONPretty)
import Validation (validateReduction, validationIsOk)
import Web.DOM.ParentNode (QuerySelector(..))
import Web.DownloadJs (download)
import Web.Event.Event as E
import Web.HTML (window)
import Web.HTML.HTMLDocument (toEventTarget)
import Web.HTML.Window (document)
import Web.UIEvent.KeyboardEvent as KE
import Web.UIEvent.KeyboardEvent.EventTypes as KET

main :: Effect Unit
main =
  HA.runHalogenAff
    $ do
        HA.awaitLoad
        elt <- HA.selectElement (QuerySelector "#app")
        for_ elt (runUI appComponent unit)

-- some comment
--------------------
-- main component --
--------------------
type AppState
  = { selected :: Selection
    , model :: Maybe Model
    , tab :: Maybe Tab
    }

type AppSlots
  = ( exportTab :: forall query. H.Slot query Void Int
    , importTab :: forall query. H.Slot query ImportOutput Int
    )

tryModelAction ::
  forall a m.
  (MonadState AppState m) =>
  (MonadEffect m) =>
  (Selection -> Maybe a) ->
  (a -> Model -> Either String Model) ->
  Boolean ->
  m Unit
tryModelAction selector action clearSel = do
  st <- H.get
  let
    modelAndSel = do
      sel <- selector st.selected
      model <- st.model
      pure { sel, model }
  case modelAndSel of
    Nothing -> pure unit
    Just { sel, model } -> case action sel model of
      Left err -> log err
      Right model' -> H.put st { model = Just model', selected = (if clearSel then SelNone else st.selected) }

combineAny :: forall o m. (MonadEffect m) => H.HalogenM AppState GraphAction AppSlots o m Unit
combineAny = do
  st <- H.get
  case st.selected of
    SelTrans _ -> handleAction VertAtSelected
    SelSlice _ -> handleAction MergeAtSelected
    _ -> pure unit

removeAny :: forall o m. (MonadEffect m) => H.HalogenM AppState GraphAction AppSlots o m Unit
removeAny = do
  st <- H.get
  case st.selected of
    SelTrans _ -> handleAction UnMergeAtSelected
    SelSlice _ -> handleAction UnVertAtSelected
    _ -> pure unit

handleAction :: forall o m. (MonadEffect m) => GraphAction -> H.HalogenM AppState GraphAction AppSlots o m Unit
handleAction = case _ of
  NoOp -> log "NoOp"
  Init -> do
    doc <- H.liftEffect $ document =<< window
    H.subscribe' \sid ->
      eventListener KET.keyup (toEventTarget doc) (KE.fromEvent >>> map HandleKey)
  SwitchTab t -> H.modify_ \st -> st { tab = t }
  HandleKey ev -> case KE.key ev of
    "m" -> pr ev *> handleAction MergeAtSelected
    "M" -> pr ev *> handleAction UnMergeAtSelected
    "v" -> pr ev *> handleAction VertAtSelected
    "V" -> pr ev *> handleAction UnVertAtSelected
    "Enter" -> do
      pr ev
      combineAny
    "Backspace" -> do
      pr ev
      removeAny
    _ -> pure unit
  Select s -> H.modify_ \st -> st { selected = s }
  HandleImport i -> case i of
    ImportPiece piece -> H.modify_ \st -> st { model = Just $ loadPiece piece, selected = SelNone, tab = Nothing }
    ImportModel model -> H.modify_ \st -> st { model = Just model, selected = SelNone, tab = Nothing }
  MergeAtSelected -> tryModelAction getSelSlice mergeAtSlice true
  VertAtSelected -> tryModelAction getSelTrans vertAtMid true
  UnMergeAtSelected -> tryModelAction getSelTrans undoMergeAtTrans true
  UnVertAtSelected -> tryModelAction getSelSlice undoVertAtSlice true
  CombineAny -> combineAny
  RemoveAny -> removeAny
  SetNoteExplanation ne -> do
    tryModelAction
      (const $ Just ne)
      (\{ noteId, expl } -> noteSetExplanation noteId expl)
      false
    H.modify_ \st -> st { selected = updateSelection ne st.selected }
  where
  pr ev = H.liftEffect $ E.preventDefault $ KE.toEvent ev

  updateSelection { noteId, expl } sel
    | SelNote selnote <- sel
    , selnote.note.id == noteId = SelNote $ selnote { expl = expl }
    | otherwise = sel

appComponent :: forall query input output m. (MonadEffect m) => H.Component query input output m
appComponent = H.mkComponent { initialState, render, eval: H.mkEval $ H.defaultEval { handleAction = handleAction, initialize = Just Init } }
  where
  initialState :: input -> AppState
  initialState _ = { selected: SelNone, model: Nothing, tab: Just ImportTab }

  render st =
    HH.div_
      [ HH.div [ class_ "content" ]
          [ HH.h1_ [ HH.text "Proto-Voice Annotation Tool" ]
          , renderTabs st
          , HH.h2_ [ HH.text "Annotation" ]
          , HH.button
              [ class_ "pure-button"
              , HE.onClick $ \_ -> CombineAny
              , HP.enabled (outerSelected st.selected)
              ]
              [ HH.text "Combine (Enter)" ]
          , HH.button
              [ class_ "pure-button"
              , HE.onClick $ \_ -> RemoveAny
              , HP.enabled (outerSelected st.selected)
              ]
              [ HH.text "Remove (Backspace)" ]
          ]
      , case st.model of
          Nothing -> HH.text ""
          Just model -> do
            let
              graph = evalGraph model.reduction

              valid = validateReduction model.reduction
            HH.div_
              [ HH.p [ class_ "content" ] [ renderNoteExplanation graph st.selected ]
              , HH.div [ class_ "wide" ] [ renderReduction model.piece graph valid st.selected ]
              ]
      ]

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

renderTabs :: forall m. MonadEffect m => AppState -> HH.ComponentHTML GraphAction AppSlots m
renderTabs st =
  HH.div_
    [ HH.div [ class_ "pure-menu pure-menu-horizontal" ]
        [ HH.ul [ class_ "pure-menu-list" ]
            [ tabHandle st HelpTab "Help"
            , tabHandle st ImportTab "Import"
            , tabHandle st ExportTab "Export"
            , tabHandle st DebugTab "Debug"
            ]
        ]
    , case st.tab of
        Nothing -> HH.text ""
        Just HelpTab -> helpText
        Just ImportTab -> HH.slot (Proxy :: Proxy "importTab") 1 importComponent unit HandleImport
        Just ExportTab -> HH.slot_ (Proxy :: Proxy "exportTab") 0 exportComponent st.model
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

-- import component
-- ----------------
data ImportAction
  = ImportUpdateModelInput String
  | ImportUpdatePieceInput String
  | ImportLoadPiece Piece
  | ImportLoadModel Model

importComponent :: forall query input m. H.Component query input ImportOutput m
importComponent = H.mkComponent { initialState, render, eval: H.mkEval H.defaultEval { handleAction = handleImportAction } }
  where
  initialState _ = { modelText: "", pieceText: "" }

  render { modelText, pieceText } =
    HH.div [ class_ "tab" ]
      [ HH.button [ class_ "pure-button", HE.onClick $ \_ -> ImportLoadPiece examplePiece ] [ HH.text "Load Example" ]
      , HH.button [ class_ "pure-button", HE.onClick $ \_ -> ImportLoadPiece examplePieceLong ] [ HH.text "Load Example (Long)" ]
      , HH.div_
          [ HH.h3_ [ HH.text "Import Piece" ]
          , HH.textarea [ HP.value pieceText, HE.onValueInput ImportUpdatePieceInput ]
          , if pieceText == "" then
              HH.text ""
            else case pieceEither of
              Left err -> HH.p [ class_ "alert" ] [ HH.text $ "Invalid input:  ", HH.pre_ [ HH.text err ] ]
              Right model ->
                HH.div_
                  [ HH.p_ [ HH.text "Input valid!" ]
                  , HH.button [ class_ "pure-button pure-button-primary", HE.onClick $ const $ ImportLoadModel model ]
                      [ HH.text "Import Piece" ]
                  ]
          ]
      , HH.div_
          [ HH.h3_ [ HH.text "Import Analysis" ]
          , HH.textarea [ HP.value modelText, HE.onValueInput ImportUpdateModelInput ]
          , if modelText == "" then
              HH.text ""
            else case modelEither of
              Left err -> HH.p [ class_ "alert" ] [ HH.text $ "Invalid input:  ", HH.pre_ [ HH.text err ] ]
              Right model ->
                HH.div_
                  [ HH.p_ [ HH.text "Input valid!" ]
                  , HH.button [ class_ "pure-button pure-button-primary", HE.onClick $ const $ ImportLoadModel model ]
                      [ HH.text "Import Analysis" ]
                  ]
          ]
      ]
    where
    showErrors errs = Left $ "Errors parsing JSON:\n  " <> intercalate "\n  " (renderForeignError <$> errs)

    pieceEither = case readJSON pieceText of
      Right json -> case pieceFromJSON json of
        Just piece -> Right $ loadPiece piece
        Nothing -> Left "Invalid piece!"
      Left errs1 -> case readJSON pieceText of
        Right jsonNoIds -> case pieceFromJSON (addJSONIds jsonNoIds) of
          Just pieceNewIds -> Right $ loadPiece pieceNewIds
          Nothing -> Left "Invalid piece!"
        Left errs2 -> showErrors (errs1 <> errs2)

    modelEither = either showErrors reductionFromJSON $ readJSON modelText

  handleImportAction = case _ of
    ImportUpdateModelInput str -> H.modify_ \st -> st { modelText = str }
    ImportUpdatePieceInput str -> H.modify_ \st -> st { pieceText = str }
    ImportLoadPiece p -> H.raise $ ImportPiece p
    ImportLoadModel m -> H.raise $ ImportModel m

-- export component
-- ----------------
data ExportAction
  = CopyToClipboard String
  | DownloadJSON String String
  | TogglePretty
  | Receive (Maybe Model)

exportComponent :: forall query output m. MonadEffect m => H.Component query (Maybe Model) output m
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
  initialState input = { model: input, pretty: false }

  render { model: modelMaybe, pretty } = case modelMaybe of
    Nothing -> HH.text ""
    Just model ->
      let
        json = reductionToJSON model

        jsonStrOrErr = (if pretty then writeJSONPretty else JSON.writeJSON) <$> json

        val = validateReduction model.reduction

        reLoad = reductionFromJSON =<< json
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
                              , HP.name "prettyJSON"
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
                          , HE.onClick \_ -> DownloadJSON "analysis.json" jsonStr
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
    Receive model -> H.modify_ \st -> st { model = model }

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
    , HH.h3_ [ HH.text "Reduction Steps" ]
    , case st.model of
        Nothing -> HH.text "No active reduction."
        Just model -> case reductionToLeftmost model of
          Right steps -> HH.ol_ $ map (\step -> HH.li_ [ HH.text $ show step ]) steps
          Left err -> HH.p [ class_ "alert" ] [ HH.text $ "Warning: reduction cannot be turned into leftmost derivation: " <> err ]
    ]
