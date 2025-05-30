module App.Tabs.SVG where

import Prelude

import App.Common (AppSettings, ModelInfo)
import App.Render (offset, scalex, sliceDistance)
import Data.Array as A
import Data.Int (toNumber)
import Data.List as L
import Data.Maybe (Maybe(..))
import Effect.Class (class MonadEffect)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Core as HC
import Halogen.HTML.Properties as HP
import Halogen.Query.Input (Input(..))
import Halogen.Svg.Attributes as SA
import Halogen.Svg.Elements as SE
import ProtoVoices.Model (Staff(..))
import ProtoVoices.RenderSVG (insertScore, renderGraph)
import Web.DOM.Element (Element)

scoreHeightGrand :: Number
scoreHeightGrand = 150.0

scoreHeightSingle :: Number
scoreHeightSingle = 80.0

scoreScale :: Number
scoreScale = 0.9

axisHeight :: Number
axisHeight = offset 2

type SVGInput = { modelInfo :: ModelInfo, settings :: AppSettings }

data SVGAction
  = SVGRegisterElt Element
  | SVGReceive SVGInput

type SVGState =
  { scoreElt :: Maybe Element
  , modelInfo :: ModelInfo
  , settings :: AppSettings
  }

svgComponent
  :: forall query output m
   . MonadEffect m
  => H.Component query SVGInput output m
svgComponent = H.mkComponent
  { initialState
  , render
  , eval: H.mkEval H.defaultEval
      { handleAction = handleSVGAction
      , receive = Just <<< SVGReceive
      }
  }
  where
  initialState { modelInfo, settings } = { modelInfo, settings, scoreElt: Nothing }

  render { modelInfo, settings } = svgContainer settings modelInfo

  handleSVGAction action = do
    case action of
      SVGRegisterElt elt -> do
        H.modify_ \st -> st { scoreElt = Just elt }
      SVGReceive { modelInfo, settings } ->
        H.modify_ \st -> st { modelInfo = modelInfo, settings = settings }
    redrawGraph

-- TODO!

svgContainer
  :: forall p
   . AppSettings
  -> ModelInfo
  -> HH.HTML p SVGAction
svgContainer sett { model, graph } =
  let
    isComplete = L.length model.reduction.segments == 1
    width = scalex sett (graph.maxx + 1.0) + sliceDistance / 2.0
    staff = model.styles.staff
    systemHeight = if staff == GrandStaff then scoreHeightGrand else scoreHeightSingle
    extraRows = if isComplete then 0.0 else 1.0
    height = systemHeight * (graph.maxd + 1.0 + extraRows) + axisHeight
  in
    HH.div_
      [ SE.svg
          [ SA.width width
          , SA.height height
          , SA.viewBox (negate $ scalex sett 1.0) 0.0 width height
          , HP.IProp $ HC.ref $ map (Action <<< SVGRegisterElt)
          , HP.style "overflow: visible;"
          ]
          [ SE.element (HH.ElemName "svg")
              [ HP.style "overflow: visible;"
              , HP.ref $ H.RefLabel $ "svgRendering"
              ]
              []
          ]
      ]

redrawGraph :: forall o m s. (MonadEffect m) => H.HalogenM SVGState SVGAction s o m Unit
redrawGraph = do
  { modelInfo: { model, graph, surface }, settings, scoreElt } <- H.get
  case scoreElt of
    Nothing -> pure unit
    Just elt ->
      let
        totalWidth = scalex settings (toNumber $ A.length model.piece) + sliceDistance / 2.0
        toX x = scalex settings x -- - (noteSize / 2.0)
      in
        H.liftEffect $ do
          insertScore elt $ renderGraph graph model.piece surface model.styles Nothing Nothing toX totalWidth scoreScale
