module Render where

import Prelude
import Model (Reduction, SliceId, StartStop(..), Note, getParents)
import Unfold (GraphSlice, GraphTransition, evalGraph, reductionToLeftmost)
import Common (GraphActions(..), Selection(..), noteIsSelected)
import Data.Array (findIndex, fromFoldable, length, mapWithIndex)
import Data.Int (toNumber)
import Data.Map as M
import Data.Maybe (Maybe(..), fromMaybe)
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.Svg.Attributes as SA
import Halogen.Svg.Elements as SE

scalex :: Number -> Number
scalex x = x * 60.0

scaley :: Number -> Number
scaley y = y * 60.0

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

selColor' :: SA.Color
selColor' = SA.RGB 100 200 255

-- noteSelColor :: SA.Color
-- noteSelColor = SA.RGB 255 0 0
white :: SA.Color
white = SA.RGB 255 255 255

black :: SA.Color
black = SA.RGB 0 0 0

lightgray :: SA.Color
lightgray = SA.RGB 211 211 211

renderSlice :: forall p. Selection -> GraphSlice -> HH.HTML p GraphActions
renderSlice selection { slice: { id, notes, x, parents }, depth: d } = case notes of
  Inner inotes ->
    SE.g (if selectable then selectionAttr else [])
      ( [ SE.rect
            [ SA.x $ scalex x - scalex 0.24
            , SA.y $ scaley d - scaley 0.24
            , SA.width $ scalex 0.48
            , SA.height $ offset (length inotes - 1) + scaley 0.48
            , SA.fill $ if selected then (Just selColor) else (Just white)
            ]
        ]
          <> mapWithIndex mknote inotes
      )
  startstop -> mknode [ HH.text $ show startstop ] (scalex x) (scaley d) false false true []
  where
  selected = selection == SelSlice id

  selectable = d == 0.0

  selectionAttr =
    [ cursor "pointer"
    , HE.onClick $ \_ -> Select (if selected then SelNone else SelSlice id)
    ]

  mknode text xcoord ycoord sel selable drawBg attr =
    SE.g (if selable then attr else [])
      $ (if drawBg then [ bg ] else [])
      <> [ label ]
    where
    bg =
      SE.rect
        [ SA.x $ xcoord - scalex 0.24
        , SA.y $ ycoord - (0.5 * offset 1)
        , SA.width $ scalex 0.48
        , SA.height $ offset 1
        , SA.fill $ if sel then (Just selColor) else (Just white)
        ]

    label =
      SE.text
        [ SA.x xcoord
        , SA.y ycoord
        , SA.text_anchor SA.AnchorMiddle
        , SA.dominant_baseline SA.BaselineMiddle
        ]
        text

  mknote :: Int -> Note -> HH.HTML p GraphActions
  mknote i note =
    mknode
      label
      (scalex x)
      (scaley d + offset i)
      nselected
      nselectable
      nselectable
      attrs
    where
    nselected = noteIsSelected selection (Inner note)

    nselectable = d /= 0.0

    label =
      [ HH.text $ show note.pitch
      , SE.title [] [ HH.text note.id ]
      ]

    attrs =
      [ cursor "pointer"
      , HE.onClick $ \_ -> Select $ if nselected then SelNone else SelNote { note: note.id, parentSlices: getParents parents }
      ]

renderTrans :: forall p. Selection -> M.Map SliceId GraphSlice -> GraphTransition -> HH.HTML p GraphActions
renderTrans selection slices { id, left, right, edges } =
  fromMaybe (HH.text "")
    $ do
        { depth: yl, slice: { x: xl, notes: nl } } <- M.lookup left slices
        { depth: yr, slice: { x: xr, notes: nr } } <- M.lookup right slices
        let
          selectable = yl == 0.0 && yr == 0.0

          bar =
            [ SE.line
                $ [ SA.x1 $ scalex xl
                  , SA.y1 $ scaley yl
                  , SA.x2 $ scalex xr
                  , SA.y2 $ scaley yr
                  , SA.stroke (Just $ if transSelected then selColor else lightgray)
                  , SA.strokeWidth 15.0
                  ]
                <> if selectable then selectionAttr else []
            ]

          mkedge { left: p1, right: p2 } =
            SE.line
              [ SA.x1 $ scalex xl
              , SA.y1 $ scaley yl + offset offl
              , SA.x2 $ scalex xr
              , SA.y2 $ scaley yr + offset offr
              , SA.stroke (Just $ if edgeSelected then selColor else black)
              , SA.strokeWidth 1.0
              ]
            where
            offl = findPitchIndex p1 nl

            offr = findPitchIndex p2 nr

            edgeSelected = noteIsSelected selection p1 || noteIsSelected selection p2

          edgeLines = map mkedge (edges.regular <> edges.passing) -- TODO
        pure $ SE.g [] (bar <> edgeLines)
  where
  transSelected = selection == SelTrans id

  selectionAttr =
    [ cursor "pointer"
    , HE.onClick $ \_ -> Select (if transSelected then SelNone else SelTrans id)
    ]

  findPitchIndex (Inner note) (Inner notes) =
    fromMaybe 0
      $ findIndex
          ((==) note)
          notes

  findPitchIndex _ _ = 0

renderReduction :: forall p. Reduction -> Selection -> HH.HTML p GraphActions
renderReduction reduction selection =
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

  svgSlices = map (renderSlice selection) $ fromFoldable $ M.values slices

  svgTranss = map (renderTrans selection slices) $ fromFoldable $ M.values transitions

renderLeftmost :: forall p. Reduction -> HH.HTML p GraphActions
renderLeftmost red = HH.ol_ $ map (\step -> HH.li_ [ HH.text $ show step ]) steps
  where
  steps = reductionToLeftmost red
