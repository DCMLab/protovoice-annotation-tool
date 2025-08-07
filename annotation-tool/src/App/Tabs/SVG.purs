module App.Tabs.SVG where

import Prelude

import App.Common (ModelInfo, AppSettings)
import App.Render (offset, scalex, sliceDistance)
import App.Utils (class_, download, getElementHTML)
import Data.Array as A
import Data.Int (toNumber)
import Data.List as L
import Data.Maybe (Maybe(..))
import Effect.Class (class MonadEffect)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Core as HC
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.Query.Input (Input(..))
import Halogen.Svg.Attributes as SA
import Halogen.Svg.Elements as SE
import ProtoVoices.Model (Staff(..))
import ProtoVoices.RenderSVG (DOMScore, insertScore, renderGraph)
import Web.DOM.Element (Element)

scoreHeightGrand :: Number
scoreHeightGrand = 150.0

scoreHeightSingle :: Number
scoreHeightSingle = 80.0

scoreScale :: Number
scoreScale = 0.9

axisHeight :: Number
axisHeight = offset 2

type SVGInput = { modelInfo :: ModelInfo, settings :: AppSettings, name :: String }

data SVGAction
  = SVGRegisterElt Element
  | SVGReceive SVGInput
  | SVGToggleSurface
  | SVGDownload

type SVGState =
  { scoreElt :: Maybe Element
  , modelInfo :: ModelInfo
  , settings :: AppSettings
  , name :: String
  , showSurface :: Boolean
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
  initialState { modelInfo, settings, name } = { modelInfo, settings, scoreElt: Nothing, showSurface: true, name: name }

  render :: SVGState -> _
  render { modelInfo, settings, showSurface } = HH.div_
    [ HH.div [ class_ "content-np tab pure-form pure-g" ]
        [ HH.label [ class_ "pure-u-4-5" ]
            [ HH.input
                [ HP.type_ $ HP.InputCheckbox
                , HP.checked showSurface
                , HE.onChange \_ -> SVGToggleSurface
                , HP.id "showSurface"
                ]
            , HH.text " show full surface below graph"
            ]
        , HH.button
            [ class_ "pure-button pure-button-primary pure-u-1-5 center"
            , HE.onClick \_ -> SVGDownload
            ]
            [ HH.text "Download" ]

        ]
    , HH.div [ class_ "wide" ]
        [ svgContainer settings showSurface modelInfo ]
    ]

  handleSVGAction action = do
    case action of
      SVGRegisterElt elt ->
        H.modify_ \st -> st { scoreElt = Just elt }
      SVGReceive { modelInfo, settings } ->
        H.modify_ \st -> st { modelInfo = modelInfo, settings = settings }
      SVGToggleSurface ->
        H.modify_ \st -> st { showSurface = not st.showSurface }
      SVGDownload -> do
        { name } <- H.get
        svg <- H.liftEffect $ getElementHTML "svg-graph"
        _ <- H.liftEffect $
          download ("<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n" <> svg) (name <> ".svg") "image/svg+xml"
        pure unit
    redrawGraph

svgContainer
  :: forall p
   . AppSettings
  -> Boolean
  -> ModelInfo
  -> HH.HTML p SVGAction
svgContainer sett showSurface { model, graph } =
  let
    isComplete = L.length model.reduction.segments == 1
    width = scalex sett (graph.maxx + 1.0) + sliceDistance / 2.0
    staff = model.styles.staff
    systemHeight = if staff == GrandStaff then scoreHeightGrand else scoreHeightSingle
    extraRows = if isComplete then 0.0 else 1.0
    surfaceRows = if showSurface then 1.0 else 0.0
    height = systemHeight * (graph.maxd + surfaceRows + extraRows) + axisHeight
  in
    HH.div
      [ HP.style "overflow: scroll;"
      ]
      [ SE.svg
          [ SA.width width
          , SA.height height
          , SA.viewBox (negate $ scalex sett 1.0) 0.0 width height
          , HP.IProp $ HC.ref $ map (Action <<< SVGRegisterElt)
          , HP.style "overflow: visible;"
          , HP.id "svg-graph"
          , HP.attr (HH.AttrName "xmlns") "http://www.w3.org/2000/svg"
          ]
          []
      ]

renderSVG :: ModelInfo -> AppSettings -> Boolean -> DOMScore
renderSVG { model, graph, surface } settings showSurface = renderGraph graph model.piece surface model.styles Nothing Nothing toX totalWidth scoreScale showSurface
  where
  totalWidth = scalex settings (toNumber $ A.length model.piece) + sliceDistance / 2.0
  toX x = scalex settings x -- - (noteSize / 2.0)

redrawGraph :: forall o m s. (MonadEffect m) => H.HalogenM SVGState SVGAction s o m Unit
redrawGraph = do
  { modelInfo, settings, scoreElt, showSurface } <- H.get
  case scoreElt of
    Nothing -> pure unit
    Just elt ->
      H.liftEffect $ do
        insertScore elt $ renderSVG modelInfo settings showSurface
