module App.Main (main) where

import Prelude
import App.Common (AppSlots, AppState, GraphAction(..), ImportOutput(..), Selection(..), Tab(..), defaultSettings, getSelSlice, getSelTrans, outerSelected)
import App.Render (class_, noteSize, renderNoteExplanation, renderReduction, scalex, scoreScale)
import App.Tabs (renderTabs)
import App.Utils (insertScore, renderScore)
import Data.Array (length)
import Data.Array as A
import Data.Either (Either(..))
import Data.Foldable (for_)
import Data.Int (toNumber)
import Data.List as L
import Data.Maybe (Maybe(..), fromMaybe, maybe)
import Effect (Effect)
import Effect.Aff.Class (class MonadAff)
import Effect.Class (class MonadEffect, liftEffect)
import Effect.Class.Console (log)
import Halogen as H
import Halogen.Aff as HA
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.Query.Event (eventListener)
import Halogen.VDom.Driver (runUI)
import ProtoVoices.Folding (evalGraph)
import ProtoVoices.Model (Model, getInnerNotes, getParents, loadPiece, mergeAtSlice, noteSetExplanation, undoMergeAtTrans, undoVertAtSlice, vertAtMid)
import ProtoVoices.Validation (validateReduction)
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

--------------------
-- main component --
--------------------
-- setModel :: forall m. MonadEffect m => Maybe Model -> H.HalogenM AppState _ AppSlots _ m Unit
-- setModel = case _ of
--   Nothing -> do
--     H.modify_ \st -> st { model = Nothing }
--   Just model -> do
--     st <- H.get
--     H.put st { model = Just model }
tryModelAction ::
  forall o a m.
  (MonadEffect m) =>
  (Selection -> Maybe a) ->
  (a -> Model -> Either String Model) ->
  String ->
  Boolean ->
  H.HalogenM AppState GraphAction AppSlots o m Unit
tryModelAction selector action actionName clearSel = do
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
      Right model' -> do
        H.put
          st
            { model = Just model'
            , selected = (if clearSel then SelNone else st.selected)
            , undoStack = { m: model, name: actionName } L.: st.undoStack
            , redoStack = L.Nil
            }
        redrawScore

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

redrawScore :: forall o m. (MonadEffect m) => H.HalogenM AppState GraphAction AppSlots o m Unit
redrawScore = do
  st <- H.get
  when st.settings.showScore do
    let
      update = do
        model <- st.model
        scoreElt <- st.scoreElt
        let
          graph = evalGraph false false model.reduction

          mkSlice { slice } = { x: scalex st.settings slice.x - (noteSize / 2.0), notes: _.note <$> getInnerNotes slice }

          slices = case st.selected of
            SelNote { note, parents } -> A.filter (\s -> s.slice.id `A.elem` getParents parents || note `A.elem` (_.note <$> getInnerNotes s.slice)) $ A.fromFoldable graph.slices
            _ -> A.filter (\s -> s.depth == 0.0) $ A.fromFoldable graph.slices

          totalWidth = (1.0 / scoreScale) * scalex st.settings (toNumber $ length model.piece + 1)
        pure $ liftEffect $ insertScore scoreElt $ renderScore (mkSlice <$> slices) totalWidth scoreScale
    fromMaybe (pure unit) update

handleAction :: forall o m. (MonadEffect m) => GraphAction -> H.HalogenM AppState GraphAction AppSlots o m Unit
handleAction act = do
  case act of
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
      "z" -> pr ev *> handleAction Undo
      "Z" -> pr ev *> handleAction Redo
      "Enter" -> pr ev *> combineAny
      " " -> pr ev *> combineAny
      "Backspace" -> pr ev *> removeAny
      _ -> pure unit
    Select s -> do
      H.modify_ \st -> st { selected = s }
      redrawScore
    HandleImport i -> do
      case i of
        ImportPiece piece -> H.modify_ \st -> st { selected = SelNone, tab = Nothing, model = Just $ loadPiece piece }
        ImportModel model -> H.modify_ \st -> st { selected = SelNone, tab = Nothing, model = Just model }
      redrawScore
    HandleSettings s -> do
      H.modify_ \st -> st { settings = s }
      redrawScore
    MergeAtSelected -> tryModelAction getSelSlice mergeAtSlice "Merge Transitions" true
    VertAtSelected -> tryModelAction getSelTrans vertAtMid "Vert Slices" true
    UnMergeAtSelected -> tryModelAction getSelTrans undoMergeAtTrans "Unmerge Transition" true
    UnVertAtSelected -> tryModelAction getSelSlice undoVertAtSlice "Unvert Slice" true
    CombineAny -> combineAny
    RemoveAny -> removeAny
    SetNoteExplanation ne -> do
      tryModelAction
        (const $ Just ne)
        (\{ noteId, expl } -> noteSetExplanation noteId expl)
        "Set Note Explanation"
        false
      H.modify_ \st -> st { selected = updateSelection ne st.selected }
    Undo -> do
      st <- H.get
      let
        { model, s1, s2 } = swapTops st.model st.undoStack st.redoStack
      H.put $ st { undoStack = s1, redoStack = s2, model = model }
      redrawScore
    Redo -> do
      st <- H.get
      let
        { model, s1, s2 } = swapTops st.model st.redoStack st.undoStack
      H.put $ st { undoStack = s2, redoStack = s1, model = model }
      redrawScore
    -- RenderScore elt slice -> do
    --   log $ "score for slice " <> show slice.id
    --   -- log $ "found elt for slice " <> show slice.id
    --   liftEffect $ insertScore elt $ renderNotes $ _.note <$> getInnerNotes slice
    RegisterScoreElt elt -> do
      H.modify_ \st -> st { scoreElt = Just elt }
      redrawScore
  where
  pr ev = H.liftEffect $ E.preventDefault $ KE.toEvent ev

  swapTops model s1 s2 = case model of
    Nothing -> { model, s1, s2 }
    Just current -> case s1 of
      L.Nil -> { model, s1, s2 }
      L.Cons { m, name } rest -> { model: Just m, s1: rest, s2: { m: current, name } L.: s2 }

  updateSelection { noteId, expl } sel
    | SelNote selnote <- sel
    , selnote.note.id == noteId = SelNote $ selnote { expl = expl }
    | otherwise = sel

appComponent :: forall query input output m. MonadEffect m => MonadAff m => H.Component query input output m
appComponent = H.mkComponent { initialState, render, eval: H.mkEval $ H.defaultEval { handleAction = handleAction, initialize = Just Init } }
  where
  initialState :: input -> AppState
  initialState _ =
    { selected: SelNone
    , model: Nothing
    , undoStack: L.Nil
    , redoStack: L.Nil
    , tab: Just ImportTab
    , settings: defaultSettings
    , scoreElt: Nothing
    }

  render st =
    HH.div_
      [ HH.div [ class_ "content" ]
          [ HH.h1_ [ HH.text "Proto-Voice Annotation Tool" ]
          , renderTabs st
          , HH.h2_ [ HH.text "Annotation" ]
          , HH.div [ class_ "pure-g" ]
              [ HH.button
                  [ class_ "pure-button pure-u-1-4"
                  , HE.onClick $ \_ -> CombineAny
                  , HP.enabled (outerSelected st.selected)
                  ]
                  [ HH.text "Combine (Enter)" ]
              , HH.button
                  [ class_ "pure-button pure-u-1-4"
                  , HE.onClick $ \_ -> RemoveAny
                  , HP.enabled (outerSelected st.selected)
                  ]
                  [ HH.text "Remove (Backspace)" ]
              , HH.button
                  [ class_ "pure-button pure-u-1-4"
                  , HE.onClick $ \_ -> Undo
                  , HP.disabled $ L.null st.undoStack
                  ]
                  [ HH.text $ "Undo " <> maybe "" _.name (L.head st.undoStack) ]
              , HH.button
                  [ class_ "pure-button pure-u-1-4"
                  , HE.onClick $ \_ -> Redo
                  , HP.disabled $ L.null st.redoStack
                  ]
                  [ HH.text $ "Redo " <> maybe "" _.name (L.head st.redoStack) ]
              ]
          ]
      , case st.model of
          Nothing -> HH.text ""
          Just model -> do
            let
              graph = evalGraph st.settings.flatHori st.settings.showAllEdges model.reduction

              validation = validateReduction model.reduction
            HH.div_
              [ HH.p [ class_ "content" ] [ renderNoteExplanation graph st.selected ]
              , HH.div [ class_ "wide" ] [ renderReduction st.settings model.piece graph validation st.selected ]
              ]
      ]
