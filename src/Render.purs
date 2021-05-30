module Render where

import Model
import Prelude
import Unfold
import Common (GraphActions(..))
import Data.Array (findIndex, fromFoldable, mapWithIndex)
import Data.Int (toNumber)
import Data.Map as M
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Tuple (Tuple(..))
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.Svg.Attributes as SA
import Halogen.Svg.Elements as SE

scalex :: Number -> Number
scalex x = x * 50.0

scaley :: Number -> Number
scaley y = y * 50.0

offset :: Int -> Number
offset i = toNumber i * 20.0

-- custom elements and attributes
cursor :: forall r i. String -> HH.IProp r i
cursor = SA.attr $ HH.AttrName "cursor"

tspan :: forall p r i. Array (HH.IProp r i) -> Array (HH.HTML p i) -> HH.HTML p i
tspan = SE.element $ HH.ElemName "tspan"

dy :: forall r i. String -> HH.IProp r i
dy = SA.attr $ HH.AttrName "dy"

svgFilter :: forall r i. String -> HH.IProp r i
svgFilter = SA.attr $ HH.AttrName "filter"

selColor :: SA.Color
selColor = SA.RGB 30 144 255

white :: SA.Color
white = SA.RGB 255 255 255

black :: SA.Color
black = SA.RGB 0 0 0

lightgray :: SA.Color
lightgray = SA.RGB 211 211 211

renderSlice :: forall p. Maybe Int -> GraphSlice -> HH.HTML p GraphActions
renderSlice selid { id, notes, x, depth: d } = case notes of
  Inner inotes ->
    SE.g
      [ cursor "pointer"
      , HE.onClick $ \_ -> SelectSlice (if selected then Nothing else Just id)
      ]
      ( [ SE.rect
            [ SA.x $ scalex x - scalex 0.4
            , SA.y $ scaley d - scaley 0.4
            , SA.width $ scalex 0.8
            , SA.height $ offset (M.size inotes - 1) + scaley 0.8
            , SA.fill $ if selected then (Just selColor) else (Just white)
            ]
        ]
          <> mapWithIndex mknote (getNotes inotes)
      )
  startstop -> mknode [ HH.text $ show startstop ] (scalex x) (scaley d) true
  where
  selected = Just id == selid

  mknode text x y fill =
    SE.text
      [ SA.x x
      , SA.y y
      , SA.text_anchor SA.AnchorMiddle
      , SA.dominant_baseline SA.BaselineMiddle
      , svgFilter (if fill then "url(#clear)" else "")
      ]
      text

  mknote i (Tuple p n) = mknode label (scalex x) (scaley d + offset i) false
    where
    label =
      [ HH.text $ show p
      , tspan
          [ dy "-7", SA.font_size SA.XSmall ]
          [ HH.text $ show n ]
      ]

renderTrans :: forall p. M.Map Int GraphSlice -> GraphTransition -> HH.HTML p GraphActions
renderTrans slices { id, left, right, edges } =
  fromMaybe (HH.text "")
    $ do
        { x: xl, depth: yl, notes: nl } <- M.lookup left slices
        { x: xr, depth: yr, notes: nr } <- M.lookup right slices
        let
          bar =
            [ SE.line
                [ SA.x1 $ scalex xl
                , SA.y1 $ scaley yl
                , SA.x2 $ scalex xr
                , SA.y2 $ scaley yr
                , SA.stroke (Just $ lightgray)
                , SA.strokeWidth 10.0
                ]
            ]

          mkedge { left: p1, right: p2 } =
            SE.line
              [ SA.x1 $ scalex xl
              , SA.y1 $ scaley yl + offset offl
              , SA.x2 $ scalex xr
              , SA.y2 $ scaley yr + offset offr
              , SA.stroke (Just $ black)
              , SA.strokeWidth 1.0
              ]
            where
            offl = findPitchIndex p1 nl

            offr = findPitchIndex p2 nr

          edgeLines = map mkedge (edges.regular <> edges.passing) -- TODO
        pure $ SE.g [] (bar <> edgeLines)
  where
  findPitchIndex (Inner pitch) (Inner notes) =
    fromMaybe 0
      $ findIndex
          (\(Tuple p n) -> p == pitch)
          (getNotes notes)

  findPitchIndex _ _ = 0

renderReduction :: forall p. Reduction -> Maybe Int -> HH.HTML p GraphActions
renderReduction reduction selected =
  HH.div
    [ HP.style "overflow-x: scroll;" ]
    [ SE.svg
        [ SA.width width
        , SA.height height
        , SA.viewBox (negate $ scalex 1.0) (negate $ scaley 1.0) width height
        ]
        (svgTranss <> svgSlices)
    ]
  where
  { slices, transitions, maxx, maxd } = evalGraph reduction

  width = scalex (maxx + 2.0)

  height = scaley (maxd + 4.0)

  svgSlices = map (renderSlice selected) $ fromFoldable $ M.values slices

  svgTranss = map (renderTrans slices) $ fromFoldable $ M.values transitions
