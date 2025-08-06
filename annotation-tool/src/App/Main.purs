module App.Main (main) where

import Prelude

import App.Common (AppSlots, AppState, GraphAction(..), ImportThing(..), Selection(..), Tab(..), defaultSettings, getSelSlice, getSelTrans, outerSelected)
import App.Render (renderNoteExplanation, renderReduction, scalex, scoreScale, sliceDistance)
import App.Tabs (renderTabs)
import App.Utils (class_, eventTargetIsBody)
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
import ProtoVoices.Folding (evalGraph, findSurface)
import ProtoVoices.JSONTransport (modelToJSON)
import ProtoVoices.Model (Model, getInnerNotes, getParents, loadPiece, mergeAtSlice, noteSetExplanation, undoMergeAtTrans, undoVertAtSlice, vertAtMid)
import ProtoVoices.RenderSVG (insertScore, renderScore)
import ProtoVoices.Validation (validateReduction)
import Simple.JSON as JSON
import Web.DOM.ParentNode (QuerySelector(..))
import Web.Event.Event as E
import Web.HTML (window)
import Web.HTML.HTMLDocument (toEventTarget)
import Web.HTML.Window (document, localStorage)
import Web.Storage.Storage as WStore
import Web.UIEvent.KeyboardEvent as KE
import Web.UIEvent.KeyboardEvent.EventTypes as KET
import Web.UIEvent.MouseEvent as ME
import Web.UIEvent.WheelEvent as WE

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
tryModelAction
  :: forall o a m
   . (MonadEffect m)
  => (Selection -> Maybe a)
  -> (a -> Model -> Either String Model)
  -> String
  -> Boolean
  -> H.HalogenM AppState GraphAction AppSlots o m Unit
tryModelAction selector action actionName clearSel = do
  st <- H.get
  let
    modelAndSel = do
      sel <- selector st.selected
      loaded <- st.loaded
      pure { sel, loaded }
  case modelAndSel of
    Nothing -> pure unit
    Just { sel, loaded } -> case action sel loaded.model of
      Left err -> log err
      Right model -> do
        H.put
          st
            { loaded = Just $ loaded { model = model }
            , selected = (if clearSel then SelNone else st.selected)
            , undoStack = { m: loaded.model, name: actionName } L.: st.undoStack
            , redoStack = L.Nil
            }
        autoSaveModel
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
      update = do -- Maybe
        loaded <- st.loaded
        let model = loaded.model
        scoreElt <- st.scoreElt
        let
          graph = evalGraph false false model.reduction

          -- mkSlice { slice } = { x: , notes: _.note <$> getInnerNotes slice }
          toX x = scalex st.settings x -- - (noteSize / 2.0)

          slices = case st.selected of
            SelNote { note, parents } -> A.filter (\s -> s.slice.id `A.elem` getParents parents || note `A.elem` (_.note <$> getInnerNotes s.slice)) $ A.fromFoldable graph.slices
            _ -> A.filter (\s -> s.depth == 0.0) $ A.fromFoldable graph.slices

          totalWidth = scalex st.settings (toNumber $ A.length model.piece) + sliceDistance / 2.0
        pure $ liftEffect $ insertScore scoreElt $ renderScore (_.slice <$> slices) toX model.styles.staff totalWidth scoreScale
    fromMaybe (pure unit) update

autoSaveModel :: forall o m. (MonadEffect m) => H.HalogenM AppState GraphAction AppSlots o m Unit
autoSaveModel = do
  loaded <- H.gets _.loaded
  name <- H.gets _.name
  case loaded of
    Just l -> case modelToJSON l.model of
      Right json ->
        liftEffect do
          w <- window
          s <- localStorage w
          WStore.setItem "autosave" (JSON.writeJSON { name, model: json }) s
      Left _err -> pure unit
    Nothing -> pure unit

handleAction :: forall o m. (MonadEffect m) => GraphAction -> H.HalogenM AppState GraphAction AppSlots o m Unit
handleAction act = do
  case act of
    NoOp -> log "NoOp"
    Init -> do
      doc <- H.liftEffect $ document =<< window
      H.subscribe' \_sid ->
        eventListener KET.keyup (toEventTarget doc) (KE.fromEvent >>> map HandleKey)
    SwitchTab t -> H.modify_ \st -> st { tab = t }
    HandleKey ev -> do
      if maybe false eventTargetIsBody $ E.target (KE.toEvent ev) then
        case KE.key ev of
          "z" -> pr ev *> handleAction Undo
          "Z" -> pr ev *> handleAction Redo
          "Enter" -> pr ev *> combineAny
          "Backspace" -> pr ev *> removeAny
          _ -> pure unit
      else pure unit
    HandleScroll ev -> do
      when (ME.ctrlKey $ WE.toMouseEvent ev) do
        H.liftEffect $ E.preventDefault $ WE.toEvent ev
        if ME.shiftKey $ WE.toMouseEvent ev then
          H.modify_ \st -> st { settings { yscale = min 2.0 (max (-2.0) $ st.settings.yscale - WE.deltaY ev / 1000.0) } }
        else
          H.modify_ \st -> st { settings { xscale = min 0.0 (max (-5.0) $ st.settings.xscale - WE.deltaY ev / 1000.0) } }
        redrawScore
    Select s -> do
      H.modify_ \st -> st { selected = s }
      autoSaveModel
      redrawScore
    HandleImport { name, thing } -> do
      case thing of
        ImportPiece piece ->
          let
            model = loadPiece piece
          in
            H.modify_ \st -> st
              { loaded = Just { model, surface: findSurface model.reduction }
              , name = name
              }
        ImportModel model -> H.modify_ \st -> st
          { loaded = Just { model, surface: findSurface model.reduction }
          , name = name
          }
      -- ImportCurrentSurface ->
      --   H.modify_ \st ->
      --     st
      --       { model = surfaceToModel <$> st.model
      --       , name = name
      --       }
      autoSaveModel
      redrawScore
      H.modify_ \st -> st { selected = SelNone, tab = Nothing, undoStack = L.Nil, redoStack = L.Nil }
    HandleSettings s -> do
      H.modify_ \st -> st { settings = s }
      redrawScore
    HandleStyle styles -> do
      H.modify_ \st -> case st.loaded of
        Nothing -> st
        Just loaded -> st { loaded = Just $ loaded { model { styles = styles } } }
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
        { loaded, s1, s2 } = swapTops st.loaded st.undoStack st.redoStack
      H.put $ st { undoStack = s1, redoStack = s2, loaded = loaded, selected = SelNone }
      autoSaveModel
      redrawScore
    Redo -> do
      st <- H.get
      let
        { loaded, s1, s2 } = swapTops st.loaded st.redoStack st.undoStack
      H.put $ st { undoStack = s2, redoStack = s1, loaded = loaded, selected = SelNone }
      autoSaveModel
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

  swapTops loaded s1 s2 = case loaded of
    Nothing -> { loaded, s1, s2 }
    Just current -> case s1 of
      L.Nil -> { loaded, s1, s2 }
      L.Cons { m, name } rest -> { loaded: Just $ current { model = m }, s1: rest, s2: { m: current.model, name } L.: s2 }

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
    , loaded: Nothing
    , name: "unnamed"
    , undoStack: L.Nil
    , redoStack: L.Nil
    , tab: Just ImportTab
    , settings: defaultSettings
    , scoreElt: Nothing
    }

  render st =
    HH.div_
      [ HH.h1 [ class_ "content" ] [ HH.text "Proto-Voice Annotation Tool" ]
      , renderTabs st modelInfo
      , HH.h2 [ class_ "content" ] [ HH.text $ "Annotations for " <> st.name ]
      , case modelInfo of
          Nothing -> HH.text ""
          Just { model, graph, validation } ->
            HH.div_
              [ HH.div [ class_ "content pure-g pure-form" ]
                  $
                    [ HH.button
                        [ class_ "pure-button pure-u-1-12"
                        , HE.onClick $ \_ -> Undo
                        , HP.disabled $ L.null st.undoStack
                        , HP.title $ "Undo " <> maybe "" _.name (L.head st.undoStack)
                        ]
                        [ HH.text "Undo"
                        ]
                    , HH.button
                        [ class_ "pure-button pure-u-1-12"
                        , HE.onClick $ \_ -> Redo
                        , HP.disabled $ L.null st.redoStack
                        , HP.title $ "Redo " <> maybe "" _.name (L.head st.redoStack)
                        ]
                        [ HH.text "Redo" ]
                    ]
                      <> case st.selected of
                        SelNote { note, expl, parents } ->
                          [ HH.div [ class_ "pure-u-1-12", HP.style "height:30px;" ] []
                          , HH.div [ class_ "pure-u-3-4" ]
                              [ renderNoteExplanation graph note expl parents ]
                          ]
                        SelNone ->
                          [ HH.div [ class_ "pure-u-1-12", HP.style "height:30px;" ] []
                          , HH.label [ class_ "pure-u-3-4" ] [ HH.text "Nothing selected." ]
                          ]
                        _ ->
                          [ HH.div [ class_ "pure-u-1-12 pure-g", HP.style "height:30px;" ] []
                          , HH.label [ class_ "pure-u-1-4" ] [ HH.text $ "Slice or transition selected." ]
                          , HH.button
                              [ class_ "pure-button pure-u-1-4"
                              , HE.onClick $ \_ -> CombineAny
                              , HP.enabled (outerSelected st.selected)
                              ]
                              [ HH.text "Reduce (Enter)" ]
                          , HH.button
                              [ class_ "pure-button pure-u-1-4"
                              , HE.onClick $ \_ -> RemoveAny
                              , HP.enabled (outerSelected st.selected)
                              ]
                              [ HH.text "Unreduce (Backspace)" ]
                          ]
              , HH.div
                  [ class_ "wide"
                  , HE.onWheel HandleScroll
                  ]
                  [ renderReduction st.settings model.piece graph validation model.styles st.selected ]
              ]
      ]
    where
    modelInfo = case st.loaded of
      Nothing -> Nothing
      Just loaded -> Just
        { model: loaded.model
        , graph: evalGraph st.settings.flatHori st.settings.showAllEdges loaded.model.reduction
        , validation: validateReduction loaded.model.reduction
        , surface: loaded.surface
        }
