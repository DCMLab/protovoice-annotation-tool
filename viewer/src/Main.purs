module Main where

import Prelude

import Common (AppSettings, Selection, ViewerAction(..), ViewerCache, cacheGetGraph, cacheGetPruned, cacheGetSurface, class_, emptyCache, fillCache, readOptions, showExplanation)
import Data.Array as A
import Data.Either (hush)
import Data.Foldable (for_)
import Data.Int (toNumber)
import Data.List as L
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Number (fromString)
import Effect (Effect)
import Effect.Aff.Class (class MonadAff)
import Effect.Class (class MonadEffect)
import Effect.Class.Console (log)
import Effect.Random (random)
import Effect.Uncurried (EffectFn3, mkEffectFn3)
import Foreign (Foreign)
import Halogen as H
import Halogen.Aff as HA
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.Subscription as HS
import Halogen.VDom.Driver (runUI)
import ProtoVoices.Folding (Graph)
import ProtoVoices.JSONTransport (ModelJSON, modelFromJSON)
import ProtoVoices.Model (Model)
import ProtoVoices.RenderSVG (insertScore, renderGraph)
import Pruning (Surface, countSteps)
import Render (noteSize, renderInner, renderReduction, renderScoreSVG, scalex, scoreScale, sliceWidth)
import Simple.JSON (readJSON)
import Web.DOM.Element (Element)
import Web.DOM.ParentNode (QuerySelector(..))

createViewer :: EffectFn3 String String Foreign Unit
createViewer = mkEffectFn3 createViewer'

createViewer' :: String -> String -> Foreign -> Effect Unit
createViewer' eltSelector json opts = do
  pfx <- show <$> random
  let
    settings = readOptions opts
  subscription <- HS.create
  HA.runHalogenAff
    $ do
        HA.awaitLoad
        elt <- HA.selectElement (QuerySelector eltSelector)
        for_ elt (runUI (viewerComponent pfx subscription) { json, settings })

type ViewerModel =
  { model :: Model
  , step :: Int
  , max :: Int
  , modelPruned :: Model
  , graph :: Graph
  , surface :: Surface
  }

type ViewerState =
  { model :: Maybe ViewerModel
  , cache :: ViewerCache
  , settings :: AppSettings
  , selected :: Selection
  , scoreElt :: Maybe Element
  , eventListener :: HS.Listener ViewerAction
  }

updateStepModel :: Int -> Int -> Model -> ViewerCache -> Maybe ViewerModel
updateStepModel step max model cache = do
  modelPruned <- hush $ cacheGetPruned model step cache
  let
    graph = cacheGetGraph modelPruned step cache

    surface = cacheGetSurface modelPruned step cache
  pure { model, step, max, modelPruned, graph, surface }

viewerComponent
  :: forall query output m
   . MonadEffect m
  => MonadAff m
  => String
  -> HS.SubscribeIO ViewerAction
  -> H.Component query { json :: String, settings :: AppSettings } output m
viewerComponent prefix { listener, emitter } =
  H.mkComponent
    { initialState
    , render
    , eval: H.mkEval $ H.defaultEval { handleAction = handleAction, initialize = Just Init }
    }
  where
  initialState :: { json :: String, settings :: AppSettings } -> ViewerState
  initialState { json, settings } =
    { model:
        case model of
          Just m -> updateStepModel m.step m.max m.model cache
          Nothing -> Nothing
    , cache
    , settings
    , selected: Nothing
    , scoreElt: Nothing
    , eventListener: listener
    }
    where
    model = do -- Maybe
      mjson :: ModelJSON <- hush (readJSON json)
      m <- hush $ modelFromJSON mjson
      let
        max = countSteps m.reduction
      pure { model: m, modelPruned: m, step: max, max: max }

    cache = case model of
      Just m -> fillCache m.model m.step emptyCache
      Nothing -> emptyCache

  render st = case st.model of
    Nothing -> HH.div [ class_ "pv-content" ] [ HH.text "No analysis loaded." ]
    Just { model, modelPruned, graph, surface, step, max } ->
      HH.div [ class_ "pv-widget" ]
        [ HH.header [ class_ "pv-content pv-controls" ]
            [ HH.div [ class_ "pv-toolbar" ]
                [ HH.button
                    [ class_ "pv-button pure-button"
                    , HE.onClick $ \_ -> ToFirst
                    , HP.disabled $ step <= 0
                    , HP.title "Move to first step"
                    ]
                    [ HH.text "<< First"
                    ]
                , HH.button
                    [ class_ "pv-button pure-button"
                    , HE.onClick $ \_ -> Backward
                    , HP.disabled $ step <= 0
                    , HP.title "Move one step back"
                    ]
                    [ HH.text "< Previous"
                    ]
                , HH.button
                    [ class_ "pv-button pure-button"
                    , HE.onClick $ \_ -> Forward
                    , HP.disabled $ step >= max
                    , HP.title "Move one step forward"
                    ]
                    [ HH.text "Next >" ]
                , HH.button
                    [ class_ "pv-button pure-button"
                    , HE.onClick $ \_ -> ToLast
                    , HP.disabled $ step >= max
                    , HP.title "Move to last step"
                    ]
                    [ HH.text "Last >>" ]
                , HH.span [ class_ "pv-step" ] [ HH.text $ " Step " <> show step <> " of " <> show max <> "." ]
                , HH.span [ class_ "pv-step" ]
                    [ case st.selected of
                        Just note -> HH.text $ "Note selected: " <> show note.note.pitch <> ", " <> showExplanation note.expl <> "."
                        Nothing -> HH.text "No note selected."
                    ]
                , HH.div [ class_ "pv-spacer" ] []
                , HH.button
                    [ class_ $ "pv-button pure-button" <> if st.settings.showSettings then " pv-button-pressed pure-button-active" else ""
                    , HE.onClick $ \_ -> ToggleSettings
                    , HP.title "Show or hide settings"
                    ]
                    [ HH.text "Settings" ]
                ]
            , if st.settings.showSettings then
                HH.div_
                  [ HH.div [ class_ "pv-control-box" ]
                      [ HH.label [ class_ "pv-control-label", HP.for "xscale" ] [ HH.text $ "horizontal zoom: " <> show st.settings.xscale ]
                      , HH.input
                          [ class_ "pv-control-range"
                          , HP.type_ $ HP.InputRange
                          , HP.min (-5.0)
                          , HP.max 0.0
                          , HP.step $ HP.Step 0.01
                          , HP.value $ show st.settings.xscale
                          , HE.onValueChange SetXScale
                          , HP.name "xscale"
                          ]
                      ]
                  , HH.div [ class_ "pv-control-box" ]
                      [ HH.label [ class_ "pv-control-label", HP.for "yscale" ] [ HH.text $ "vertical zoom: " <> show st.settings.yscale ]
                      , HH.input
                          [ class_ "pv-control-range"
                          , HP.type_ $ HP.InputRange
                          , HP.min (-2.0)
                          , HP.max 2.0
                          , HP.step $ HP.Step 0.01
                          , HP.value $ show st.settings.yscale
                          , HE.onValueChange SetYScale
                          , HP.name "yscale"
                          ]
                      ]
                  , HH.div [ class_ "pv-control-box" ]
                      [ HH.input
                          [ HP.type_ $ HP.InputCheckbox
                          , HP.checked st.settings.showInner
                          , HE.onChange \_ -> ToggleInner
                          , HP.id $ prefix <> "showInnerGraph"
                          ]
                      , HH.label [ HP.for $ prefix <> "showInnerGraph" ] [ HH.text "show inner graph" ]
                      ]
                  , HH.div [ class_ "pv-control-box" ]
                      [ HH.input
                          [ HP.type_ $ HP.InputCheckbox
                          , HP.checked st.settings.showScore
                          , HE.onChange \_ -> ToggleScore
                          , HP.id $ prefix <> "showScore"
                          ]
                      , HH.label [ HP.for $ prefix <> "showScore" ] [ HH.text "show score" ]
                      ]
                  , HH.div [ class_ "pv-control-box" ]
                      [ HH.input
                          [ HP.type_ $ HP.InputCheckbox
                          , HP.checked st.settings.grandStaff
                          , HE.onChange \_ -> ToggleGrandStaff
                          , HP.id $ prefix <> "useGrandStaff"
                          ]
                      , HH.label [ HP.for $ prefix <> "useGrandStaff" ] [ HH.text "use grand staff" ]
                      ]
                  , HH.div [ class_ "pv-control-box" ]
                      [ HH.input
                          [ HP.type_ $ HP.InputCheckbox
                          , HP.checked st.settings.showOuter
                          , HE.onChange \_ -> ToggleOuter
                          , HP.id $ prefix <> "showOuterGraph"
                          ]
                      , HH.label [ HP.for $ prefix <> "showOuterGraph" ] [ HH.text "show outer graph" ]
                      ]
                  ]
              else
                HH.text ""
            ]
        , HH.div
            [ class_ "pv-wide" ]
            [ HH.div [ class_ "pv-graph" ]
                [ if st.settings.showInner then
                    renderInner st.settings st.selected surface graph
                  else
                    HH.text ""
                , if st.settings.showScore then
                    renderScoreSVG st.settings modelPruned.piece graph (L.length model.reduction.segments == 1)
                  else
                    HH.text ""
                , if st.settings.showOuter then
                    renderReduction st.settings modelPruned.piece graph surface st.selected
                  else
                    HH.text ""
                ]
            ]
        ]

  setStep f st =
    fromMaybe st do
      { model, step, max: mx } <- st.model
      let
        step' = max 0 (min mx (f step))
      model' <- updateStepModel step' mx model st.cache
      pure $ st { model = Just model' }

  handleAction = case _ of
    NoOp -> pure unit
    Init -> do
      log "initializing."
      void $ H.subscribe emitter
    Select sel -> do
      H.modify_ \st -> st { selected = sel }
      redrawScore
    Forward -> do
      H.modify_ $ setStep (_ + 1)
      redrawScore
    Backward -> do
      H.modify_ $ setStep (_ - 1)
      redrawScore
    ToFirst -> do
      H.modify_ $ setStep (const 0)
      redrawScore
    ToLast -> do
      H.modify_ \st -> case st.model of
        Just { max } -> setStep (const max) st
        Nothing -> st
      redrawScore
    RegisterScoreElt elt -> do
      H.modify_ \st -> st { scoreElt = Just elt }
      redrawScore
    ToggleSettings -> H.modify_ \st -> st { settings { showSettings = not st.settings.showSettings } }
    ToggleInner -> H.modify_ \st -> st { settings { showInner = not st.settings.showInner } }
    ToggleOuter -> H.modify_ \st -> st { settings { showOuter = not st.settings.showOuter } }
    ToggleScore -> H.modify_ \st -> st { settings { showScore = not st.settings.showScore } }
    ToggleGrandStaff -> do
      H.modify_ \st -> st { settings { grandStaff = not st.settings.grandStaff } }
      redrawScore
    SetXScale s -> case fromString s of
      Nothing -> pure unit
      Just n -> do
        H.modify_ \st -> st { settings { xscale = n } }
        redrawScore
    SetYScale s -> case fromString s of
      Nothing -> pure unit
      Just n -> do
        H.modify_ \st -> st { settings { yscale = n } }
        redrawScore

redrawScore :: forall o m s. (MonadEffect m) => H.HalogenM ViewerState ViewerAction s o m Unit
redrawScore = do
  st <- H.get
  let
    update = do -- Maybe
      { model, surface: { slices }, graph } <- st.model
      scoreElt <- st.scoreElt
      let
        totalWidth = (1.0 / scoreScale) * (scalex st.settings (toNumber $ A.length model.piece) + sliceWidth / 2.0)
        select sel = do
          HS.notify st.eventListener $ Select sel
        toX x = scalex st.settings x - (noteSize / 2.0)
      pure $ H.liftEffect $ do
        insertScore scoreElt $ renderGraph graph slices st.selected select toX totalWidth scoreScale st.settings.grandStaff
  fromMaybe (pure unit) update
