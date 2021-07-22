module Main where

import Prelude
import CommonApp (GraphAction(..), Selection(..), Tab(..), getSelSlice, getSelTrans, outerSelected)
import Control.Monad.State (class MonadState)
import Data.Either (Either(..))
import Data.Foldable (for_)
import Data.List as L
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.Class (class MonadEffect, liftEffect)
import Effect.Class.Console (log)
import Folding (evalGraph, reductionToLeftmost)
import Halogen as H
import Halogen.Aff as HA
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.Query.Event (eventListener)
import Halogen.VDom.Driver (runUI)
import JSONTransport (reductionFromJSON, reductionToJSON)
import Model (Model, loadPiece, mergeAtSlice, noteSetExplanation, undoMergeAtTrans, undoVertAtSlice, vertAtMid)
import Render (class_, renderNoteExplanation, renderReduction)
import Simple.JSON as JSON
import Type.Proxy (Proxy(..))
import Utils (copyToClipboard, examplePiece, examplePieceLong, writeJSONPretty)
import Validation (validateReduction, validationIsOk)
import Web.DOM.ParentNode (QuerySelector(..))
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
  = ( export :: forall query. H.Slot query Void Int )

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
  LoadPiece piece -> H.modify_ \st -> st { model = Just $ loadPiece piece, selected = SelNone, tab = Nothing }
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
    HH.div
      []
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
      , case st.model of
          Nothing -> HH.text ""
          Just model -> do
            let
              graph = evalGraph model.reduction

              valid = validateReduction model.reduction
            HH.div_
              [ HH.p_ [ renderNoteExplanation graph st.selected ]
              , renderReduction model.piece graph valid st.selected
              ]
      -- , HH.p_
      --     [ HH.text "Selection: "
      --     , HH.text $ show st.selected
      --     ]
      --, HH.pre_ [ HH.text $ maybe "No Piece Loaded" (_.reduction >>> showReduction) st.model ]
      ]

--------------------
-- sub-components --
--------------------
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

renderTabs :: forall m. MonadEffect m => AppState -> HH.ComponentHTML GraphAction AppSlots m
renderTabs st =
  HH.div_
    [ HH.div [ class_ "pure-menu pure-menu-horizontal" ]
        [ HH.ul [ class_ "pure-menu-list" ]
            [ HH.li [ class_ $ "pure-menu-item" <> if st.tab == Just HelpTab then " pure-menu-selected" else "" ]
                [ HH.a
                    [ class_ $ "pure-menu-link"
                    , HE.onClick \_ -> SwitchTab if st.tab == Just HelpTab then Nothing else Just HelpTab
                    , HP.href "javascript:;"
                    ]
                    [ HH.text "Help" ]
                ]
            , HH.li [ class_ $ "pure-menu-item" <> if st.tab == Just ImportTab then " pure-menu-selected" else "" ]
                [ HH.a
                    [ class_ $ "pure-menu-link"
                    , HE.onClick \_ -> SwitchTab if st.tab == Just ImportTab then Nothing else Just ImportTab
                    , HP.href "javascript:;"
                    ]
                    [ HH.text "Import" ]
                ]
            , HH.li [ class_ $ "pure-menu-item" <> if st.tab == Just ExportTab then " pure-menu-selected" else "" ]
                [ HH.a
                    [ class_ $ "pure-menu-link"
                    , HE.onClick \_ -> SwitchTab if st.tab == Just ExportTab then Nothing else Just ExportTab
                    , HP.href "javascript:;"
                    ]
                    [ HH.text "Export" ]
                ]
            ]
        ]
    , case st.tab of
        Nothing -> HH.text ""
        Just HelpTab -> helpText
        Just ImportTab ->
          HH.div [ class_ "tab" ]
            [ HH.button [ class_ "pure-button", HE.onClick $ \_ -> LoadPiece examplePiece ] [ HH.text "Load Example" ]
            , HH.button [ class_ "pure-button", HE.onClick $ \_ -> LoadPiece examplePieceLong ] [ HH.text "Load Example (Long)" ]
            ]
        Just ExportTab -> HH.slot_ (Proxy :: Proxy "export") 0 exportComponent st.model
    ]

-- import component
-- ----------------
data ImportAction
  = UpdateJSONInput String

-- export component
-- ----------------
data ExportAction
  = CopyToClipboard String
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
        json = reductionToJSON model.reduction

        jsonStr = (if pretty then writeJSONPretty else JSON.writeJSON) json

        val = validateReduction model.reduction

        reLoad = reductionFromJSON json
      in
        HH.div [ class_ "tab" ]
          [ if validationIsOk val then
              HH.text ""
            else
              HH.p [ class_ "alert" ] [ HH.text "Warning: reduction is incomplete and/or contains errors." ]
          , HH.h3_ [ HH.text "Reduction Steps" ]
          , HH.ol_ $ map (\step -> HH.li_ [ HH.text $ show step ]) $ reductionToLeftmost model.reduction
          , HH.h3_ [ HH.text "JSON String" ]
          , HH.div [ class_ "pure-g" ]
              [ HH.div [ class_ "pure-u-3-4" ]
                  [ HH.input
                      [ HP.type_ $ HP.InputCheckbox
                      , HP.checked pretty
                      , HE.onChange \_ -> TogglePretty
                      , HP.name "prettyJSON"
                      ]
                  , HH.label [ HP.for "prettyJSON" ] [ HH.text " pretty" ]
                  ]
              , HH.button
                  [ class_ "pure-button pure-button-primary pure-u-1-4"
                  , HE.onClick \_ -> CopyToClipboard jsonStr
                  ]
                  [ HH.text "Copy to Clipboard" ]
              ]
          , HH.pre_ [ HH.text $ jsonStr ]
          , HH.div_ case reLoad of
              Left err -> [ HH.text $ "Error re-reading reduction: " <> err ]
              Right re -> case L.find (\(Tuple a b) -> a /= b) $ L.zip model.reduction.segments re.segments of
                Nothing ->
                  if re == model.reduction then
                    [ HH.text "roundtrip ok" ]
                  else
                    [ HH.text "roundtrip not ok (but can't find wrong segment)! original:"
                    , HH.pre_ [ HH.text $ show model.reduction ]
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
    TogglePretty -> H.modify_ \st -> st { pretty = not st.pretty }
    Receive model -> H.modify_ \st -> st { model = model }
