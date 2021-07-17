module Main where

import Prelude
import CommonApp (GraphAction(..), Selection(..), getSelSlice, getSelTrans, outerSelected)
import Control.Monad.State (class MonadState)
import Data.Either (Either(..))
import Data.Foldable (for_)
import Data.Maybe (Maybe(..), maybe)
import Effect (Effect)
import Effect.Class (class MonadEffect)
import Effect.Class.Console (log)
import Folding (evalGraph)
import Halogen as H
import Halogen.Aff as HA
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.Query.Event (eventListener)
import Halogen.VDom.Driver (runUI)
import Model (Model, loadPiece, mergeAtSlice, noteSetExplanation, showReduction, undoMergeAtTrans, undoVertAtSlice, vertAtMid)
import Render (renderLeftmost, renderNoteExplanation, renderReduction)
import Utils (examplePiece, examplePieceLong)
import Validation (validateReduction)
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

type AppState
  = { selected :: Selection
    , model :: Maybe Model
    }

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

combineAny :: forall o m. (MonadEffect m) => H.HalogenM AppState GraphAction () o m Unit
combineAny = do
  st <- H.get
  case st.selected of
    SelTrans _ -> handleAction VertAtSelected
    SelSlice _ -> handleAction MergeAtSelected
    _ -> pure unit

removeAny :: forall o m. (MonadEffect m) => H.HalogenM AppState GraphAction () o m Unit
removeAny = do
  st <- H.get
  case st.selected of
    SelTrans _ -> handleAction UnMergeAtSelected
    SelSlice _ -> handleAction UnVertAtSelected
    _ -> pure unit

handleAction :: forall o m. (MonadEffect m) => GraphAction -> H.HalogenM AppState GraphAction () o m Unit
handleAction = case _ of
  NoOp -> log "NoOp"
  Init -> do
    doc <- H.liftEffect $ document =<< window
    H.subscribe' \sid ->
      eventListener KET.keyup (toEventTarget doc) (KE.fromEvent >>> map HandleKey)
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
  LoadPiece piece -> H.modify_ \st -> st { model = Just $ loadPiece piece, selected = SelNone }
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
  initialState _ = { selected: SelNone, model: Nothing }

  render st =
    HH.div
      []
      [ HH.h1_ [ HH.text "Proto-Voice Annotation" ]
      , HH.button [ HE.onClick $ \_ -> LoadPiece examplePiece ] [ HH.text "Load Example" ]
      , HH.button [ HE.onClick $ \_ -> LoadPiece examplePieceLong ] [ HH.text "Load Example (Long)" ]
      , HH.button
          [ HE.onClick $ \_ -> CombineAny, HP.enabled (outerSelected st.selected) ]
          [ HH.text "Combine (Enter)" ]
      , HH.button
          [ HE.onClick $ \_ -> RemoveAny, HP.enabled (outerSelected st.selected) ]
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
              , renderLeftmost model.reduction
              ]
      , HH.p_
          [ HH.text "Selection: "
          , HH.text $ show st.selected
          ]
      --, HH.pre_ [ HH.text $ maybe "No Piece Loaded" (_.reduction >>> showReduction) st.model ]
      ]
