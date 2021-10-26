module Main (mainJSON) where

import Prelude
import Common (AppSettings, Selection, ViewerAction(..), ViewerCache, cacheGetGraph, cacheGetPruned, cacheGetSurface, class_, defaultSettings, emptyCache, fillCache)
import Data.Array as A
import Data.Either (hush)
import Data.Foldable (for_)
import Data.Int (toNumber)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Number (fromString)
import Effect (Effect)
import Effect.Aff.Class (class MonadAff)
import Effect.Class (class MonadEffect)
import Effect.Class.Console (log)
import Halogen as H
import Halogen.Aff as HA
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.VDom.Driver (runUI)
import ProtoVoices.Folding (Graph)
import ProtoVoices.JSONTransport (ModelJSON, modelFromJSON)
import ProtoVoices.Model (Model, getInnerNotes)
import Pruning (Surface, countSteps)
import Render (noteSize, renderReduction, scalex, scoreScale)
import Simple.JSON (readJSON)
import Utils (insertScore, renderScore)
import Web.DOM.Element (Element)
import Web.DOM.ParentNode (QuerySelector(..))

mainJSON :: String -> String -> Effect Unit
mainJSON eltSelector json =
  HA.runHalogenAff
    $ do
        HA.awaitLoad
        elt <- HA.selectElement (QuerySelector eltSelector)
        for_ elt (runUI viewerComponent json)

type ViewerModel
  = { model :: Model
    , step :: Int
    , max :: Int
    , modelPruned :: Model
    , graph :: Graph
    , surface :: Surface
    }

type ViewerState
  = { model :: Maybe ViewerModel
    , cache :: ViewerCache
    , settings :: AppSettings
    , selected :: Selection
    , scoreElt :: Maybe Element
    }

updateStepModel :: Int -> Int -> Model -> ViewerCache -> Maybe ViewerModel
updateStepModel step max model cache = do
  modelPruned <- hush $ cacheGetPruned model step cache
  let
    graph = cacheGetGraph modelPruned step cache

    surface = cacheGetSurface modelPruned step cache
  pure { model, step, max, modelPruned, graph, surface }

viewerComponent :: forall query output m. MonadEffect m => MonadAff m => H.Component query String output m
viewerComponent = H.mkComponent { initialState, render, eval: H.mkEval $ H.defaultEval { handleAction = handleAction, initialize = Just Init } }
  where
  initialState :: String -> ViewerState
  initialState json =
    { model:
        case model of
          Just m -> updateStepModel m.step m.max m.model cache
          Nothing -> Nothing
    , cache
    , settings: defaultSettings
    , selected: Nothing
    , scoreElt: Nothing
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
      HH.div_
        [ HH.div [ class_ "pv-content" ]
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
                , HH.span [ class_ "pv-step" ] [ HH.text $ " Step " <> show step <> " of " <> show max ]
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
                  ]
              else
                HH.text ""
            ]
        , HH.div
            [ class_ "pv-wide" ]
            [ renderReduction st.settings modelPruned.piece graph surface st.selected ]
        , HH.p_ [ HH.text $ show surface ]
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
    Init -> log "initializing."
    Select sel -> H.modify_ \st -> st { selected = sel }
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
      { model, surface: { slices } } <- st.model
      scoreElt <- st.scoreElt
      let
        mkSlice slice = { x: scalex st.settings slice.x - (noteSize / 2.0), notes: _.note <$> getInnerNotes slice }

        totalWidth = (1.0 / scoreScale) * scalex st.settings (toNumber $ A.length model.piece + 1)
      pure $ H.liftEffect $ insertScore scoreElt $ renderScore (mkSlice <$> slices) totalWidth scoreScale
  fromMaybe (pure unit) update
