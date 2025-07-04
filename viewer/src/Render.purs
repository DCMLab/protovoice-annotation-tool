module Render where

import Prelude

import Common (AppSettings, Selection, ViewerAction(..), noteIsSelected, showExplanation)
import Data.Array as A
import Data.Foldable (maximum, minimum)
import Data.Int (toNumber)
import Data.List as L
import Data.Map as M
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Number (exp)
import Data.Pitches (diasteps)
import Data.Set as S
import Data.Tuple (Tuple(..))
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Core as HC
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.Query.Input (Input(..))
import Halogen.Svg.Attributes as SA
import Halogen.Svg.Elements as SE
import ProtoVoices.Folding (GraphSlice, GraphTransition, Graph)
import ProtoVoices.Model (BottomSurface, Edge, Note, NoteExplanation(..), Notes, Piece, SliceId, Staff(..), StartStop(..), explHasParent, getInnerNotes)

sliceWidth :: Number
sliceWidth = 70.0

scalex :: AppSettings -> Number -> Number
scalex { xscale } x = x * sliceWidth * exp xscale

scaley :: AppSettings -> Number -> Number
scaley { yscale } y = y * 100.0 * exp yscale

offset :: Int -> Number
offset i = toNumber i * 20.0

noteSize :: Number
noteSize = 29.0

innerFactor :: Number
innerFactor = 1.6

scoreHeightGrand :: Number
scoreHeightGrand = 150.0

scoreHeightSingle :: Number
scoreHeightSingle = 80.0

scoreScale :: Number
scoreScale = 0.9

axisHeight :: Number
axisHeight = offset 2

findPitchIndex :: StartStop Note -> StartStop Notes -> Int
findPitchIndex (Inner note) (Inner notes) =
  fromMaybe (-1)
    $ A.findIndex
        (\n -> n.note == note)
        notes

findPitchIndex _ _ = 0

-- custom elements and attributes
cursor :: forall r i. String -> HH.IProp r i
cursor = HP.attr $ HH.AttrName "cursor"

tspan :: forall p r i. Array (HH.IProp r i) -> Array (HH.HTML p i) -> HH.HTML p i
tspan = SE.element $ HH.ElemName "tspan"

dy :: forall r i. String -> HH.IProp r i
dy = HP.attr $ HH.AttrName "dy"

svgFilter :: forall r i. String -> HH.IProp r i
svgFilter = HP.attr $ HH.AttrName "filter"

selColorOuter :: SA.Color
selColorOuter = SA.RGB 30 144 255

selColorOuter' :: SA.Color
selColorOuter' = SA.RGB 135 206 250

selColorInner :: SA.Color
selColorInner = selColorOuter -- SA.RGB 51 160 44

selColorInner' :: SA.Color
selColorInner' = selColorOuter' -- SA.RGB 178 223 138

parentColor :: SA.Color
parentColor = selColorInner'

warnColor :: SA.Color
warnColor = SA.RGB 255 165 0

errColor :: SA.Color
errColor = SA.RGB 255 0 0

white :: SA.Color
white = SA.RGB 255 255 255

black :: SA.Color
black = SA.RGB 0 0 0

lightgray :: SA.Color
lightgray = SA.RGB 211 211 211

data SelectionStatus
  = NotSelected
  | Selected
  | Related

derive instance eqSelectionStatus :: Eq SelectionStatus

noteSelectionStatus :: Selection -> Note -> SelectionStatus
noteSelectionStatus sel note = if selected then Selected else if parentSelected then Related else NotSelected
  where
  selected = noteIsSelected sel (Inner note)

  parentSelected = case sel of
    Just { expl } -> explHasParent note.id expl
    Nothing -> false

renderSlice :: forall p. AppSettings -> Selection -> GraphSlice -> HH.HTML p ViewerAction
renderSlice sett selection { slice: slice@{ id, notes, x, parents }, depth: d } = case notes of
  Inner inotes ->
    SE.g []
      $
        [ SE.g []
            $
              [ SE.rect
                  [ SA.x svgx
                  , SA.y $ scaley sett d - (noteSize / 2.0)
                  , SA.width noteSize
                  , SA.height $ offset (A.length inotes - 1) + noteSize
                  , SA.fill white
                  ]
              ]
                <> A.mapWithIndex mknote inotes
        ]
  startstop -> mknode (show startstop) (show startstop) (scalex sett x) (scaley sett d) (if selIsRoot then Related else NotSelected) []
  where
  svgx = scalex sett x - (noteSize / 2.0)

  selIsRoot = case selection of
    Just { expl } -> expl == RootExpl
    Nothing -> false

  isTopLevel = d == 0.0

  mknode text title xcoord ycoord selStatus attr =
    SE.g attr
      $ [ bg, label, SE.title [] [ HH.text title ] ]
    where
    bg =
      SE.rect
        [ SA.x $ xcoord - (noteSize / 2.0)
        , SA.y $ ycoord - (offset 1 / 2.0)
        , SA.width noteSize
        , SA.height $ offset 1
        , SA.fill
            $ case selStatus of
                NotSelected -> white
                Selected -> selColorInner
                Related -> selColorInner'
        ]

    label =
      SE.element (HH.ElemName "text")
        [ SA.x xcoord
        , SA.y ycoord
        , SA.textAnchor SA.AnchorMiddle
        , SA.dominantBaseline SA.BaselineMiddle
        , HP.style "pointer-events: none;"
        , SA.fill if selStatus == NotSelected then black else white
        ]
        [ HH.text $ text ]

  mknote :: Int -> { note :: Note, expl :: NoteExplanation } -> HH.HTML p ViewerAction
  mknote i { note, expl } =
    mknode
      (show note.pitch)
      (showExplanation expl)
      (scalex sett x)
      (scaley sett d + offset i)
      nodeselected
      (if clickable then attrsSel else [])
    where
    nodeselected = noteSelectionStatus selection note

    nselectable = d /= 0.0

    clickable = nselectable

    attrsSel =
      [ cursor "pointer"
      , HE.onClick
          $ \ev ->
              if nselectable then
                Select if nodeselected == Selected then Nothing else Just { note, expl }
              else
                NoOp
      ]

renderTrans :: forall p. AppSettings -> Selection -> M.Map SliceId GraphSlice -> GraphTransition -> HH.HTML p ViewerAction
renderTrans sett selection slices { id, left, right, edges } =
  fromMaybe (HH.text "")
    $ do
        { depth: yl, slice: { x: xl, notes: nl } } <- M.lookup left slices
        { depth: yr, slice: { x: xr, notes: nr } } <- M.lookup right slices
        let
          selectable = yl == 0.0 && yr == 0.0

          bar =
            [ SE.line
                $
                  [ SA.x1 $ scalex sett xl
                  , SA.y1 $ scaley sett yl
                  , SA.x2 $ scalex sett xr
                  , SA.y2 $ scaley sett yr
                  , SA.stroke lightgray
                  , SA.strokeWidth $ if selectable then (noteSize / 2.0) else 5.0
                  ]
            ]

          mkedge :: Boolean -> Edge -> HH.HTML p ViewerAction
          mkedge isPassing edge@{ left: p1, right: p2 } =
            SE.line
              [ SA.x1 $ scalex sett xl
              , SA.y1 $ scaley sett yl + offset offl
              , SA.x2 $ scalex sett xr
              , SA.y2 $ scaley sett yr + offset offr
              , SA.stroke
                  if edgeSelected then
                    selColorInner
                  else
                    black
              , SA.strokeWidth 1.0
              , HP.attr (HH.AttrName "stroke-dasharray") (if isPassing then "6,3" else "")
              ]
            where
            offl = findPitchIndex p1 nl

            offr = findPitchIndex p2 nr

            edgeSelected = noteIsSelected selection p1 || noteIsSelected selection p2

          edgeLines = map (mkedge false) (A.fromFoldable edges.regular) <> map (mkedge true) (A.fromFoldable edges.passing)
        pure $ SE.g [] (bar <> edgeLines)

renderHori
  :: forall p
   . AppSettings
  -> Selection
  -> M.Map SliceId GraphSlice
  -> { child :: SliceId, parent :: SliceId }
  -> HH.HTML p ViewerAction
renderHori sett selection slices { child, parent } =
  fromMaybe (HH.text "")
    $ do
        { depth: yc, slice: slicec@{ x: xc, notes: notesc } } <- M.lookup child slices
        { depth: yp, slice: { x: xp, notes: notesp } } <- M.lookup parent slices
        let
          bar =
            [ SE.line
                $
                  [ SA.x1 $ scalex sett xp
                  , SA.y1 $ scaley sett yp
                  , SA.x2 $ scalex sett xc
                  , SA.y2 $ scaley sett yc
                  , SA.stroke lightgray
                  , SA.strokeWidth 5.0
                  , HP.attr (HH.AttrName "stroke-dasharray") "10,5"
                  ]
            ]

          mkedge :: { parentNote :: Note, childNote :: Note } -> HH.HTML p ViewerAction
          mkedge { parentNote, childNote } =
            SE.line
              [ SA.x1 $ scalex sett xp
              , SA.y1 $ scaley sett yp + offset offp
              , SA.x2 $ scalex sett xc
              , SA.y2 $ scaley sett yc + offset offc
              , SA.stroke if edgeSelected then selColorInner else black
              , SA.strokeWidth 1.0
              ]
            where
            offp = findPitchIndex (Inner parentNote) notesp

            offc = findPitchIndex (Inner childNote) notesc

            edgeSelected = noteIsSelected selection (Inner parentNote) || noteIsSelected selection (Inner childNote)

          edges = map mkedge $ A.catMaybes $ map explToHori $ getInnerNotes slicec
        pure $ SE.g [] $ bar <> edges
  where
  explToHori note = case note.expl of
    HoriExpl parentNote -> Just { childNote: note.note, parentNote }
    _ -> Nothing

renderInner :: forall p. AppSettings -> Selection -> BottomSurface -> Graph -> HH.HTML p ViewerAction
renderInner sett sel { slices, transs } graph = svg
  where
  extractNotes slice = M.fromFoldable $ (\n -> Tuple n.note.id { note: n, x: slice.x }) <$> getInnerNotes slice

  notes = M.unions $ extractNotes <$> slices

  surfaceEdges =
    S.fromFoldable
      $ do
          trans <- transs
          A.fromFoldable trans.regular <> trans.passing

  miny = fromMaybe 0 $ minimum $ map (_.note.note.pitch >>> diasteps) $ M.values notes

  maxy = fromMaybe 0 $ maximum $ map (_.note.note.pitch >>> diasteps) $ M.values notes

  width = scalex sett (graph.maxx + 2.0)

  height = innerFactor * (offset $ maxy - miny + 2)

  selIsRoot = case sel of
    Just { expl } -> expl == RootExpl
    Nothing -> false

  notePosition :: StartStop Note -> { x :: Number, y :: Number }
  notePosition = case _ of
    Inner note ->
      { x: scalex sett $ fromMaybe 0.0 $ _.x <$> M.lookup note.id notes
      , y: innerFactor * (offset $ maxy - diasteps note.pitch)
      }
    Start -> { x: 0.0, y: (offset $ maxy - miny) * innerFactor / 2.0 }
    Stop -> { x: scalex sett graph.maxx, y: (offset $ maxy - miny) * innerFactor / 2.0 }

  surfaceNote :: StartStop Note -> Boolean
  surfaceNote = case _ of
    Inner note -> M.member note.id notes
    _ -> true

  mkNode { x, y } label title selected selAttr =
    SE.g []
      [ SE.circle
          $
            [ SA.cx x
            , SA.cy y
            , SA.r 15.0
            , SA.stroke black
            , SA.fill case selected of
                NotSelected -> white
                Selected -> selColorInner
                Related -> selColorInner'
            , cursor "pointer"
            ]
              <> selAttr
      , SE.element (H.ElemName "text")
          [ SA.x x
          , SA.y y
          , SA.textAnchor SA.AnchorMiddle
          , SA.dominantBaseline SA.BaselineMiddle
          , HP.style "pointer-events: none;"
          , SA.fill $ if selected == NotSelected then black else white
          ]
          [ HH.text $ label ]
      , SE.title [] [ HH.text title ]
      ]

  mkNote { note: { note, expl }, x: notex } = mkNode (notePosition $ Inner note) (show note.pitch) (showExplanation expl) selected selAttr
    where
    selected = noteSelectionStatus sel note

    selAttr =
      [ HE.onClick \ev ->
          Select if selected == Selected then Nothing else Just { note, expl }
      ]

  mkStartStop s = mkNode (notePosition s) (show s) (show s) (if selIsRoot then Related else NotSelected) []

  mkEdge isRegular { left, right }
    | surfaceNote left && surfaceNote right =
        SE.line
          $
            [ SA.x1 x1
            , SA.x2 x2
            , SA.y1 y1
            , SA.y2 y2
            , SA.stroke
                if edgeSelected then
                  selColorInner
                else if S.member { left, right } surfaceEdges then black
                else lightgray
            ]
              <> if isRegular then [] else [ HP.attr (HH.AttrName "stroke-dasharray") "6,3" ]
        where
        { x: x1, y: y1 } = notePosition left

        { x: x2, y: y2 } = notePosition right

        edgeSelected = noteIsSelected sel left || noteIsSelected sel right
    | otherwise = HH.text ""

  svgNotes = A.fromFoldable $ mkNote <$> M.values notes

  svgArrows = do -- Array
    trans <- A.fromFoldable $ M.values graph.transitions
    (mkEdge true <$> A.fromFoldable trans.edges.regular) <> (mkEdge false <$> trans.edges.passing)

  svg =
    HH.div_
      [ SE.svg
          [ SA.width width
          , SA.height height
          , SA.viewBox (negate $ scalex sett 1.0) (negate $ innerFactor * offset 1) width height
          ]
          (svgArrows <> svgNotes <> [ mkStartStop Start, mkStartStop Stop ])
      ]

-- renderTime :: forall r p. AppSettings -> Number -> Int -> { time :: Either String MBS | r } -> HH.HTML p ViewerAction
-- renderTime sett yoff i { time } =
--   SE.text
--     [ SA.x $ scalex sett $ toNumber (i + 1)
--     , SA.y $ (axisHeight - offset 1) + yoff
--     , SA.textAnchor SA.AnchorMiddle
--     , SA.dominantBaseline SA.BaselineMiddle
--     ]
--     [ HH.text label ]
--   where
--   label = case time of
--     Right (MBS { m, b, s }) -> if s == 0 % 1 then show m <> "." <> show b else ""
--     Left str -> str

renderScoreSVG
  :: forall p
   . AppSettings
  -> Piece
  -> Graph
  -> Boolean
  -> Staff
  -> HH.HTML p ViewerAction
renderScoreSVG sett piece graph isComplete staffType =
  HH.div_
    [ SE.svg
        [ SA.width width
        , SA.height height
        , SA.viewBox (negate $ scalex sett 1.0) 0.0 width height
        , HP.IProp $ HC.ref $ map (Action <<< RegisterScoreElt)
        , HP.style "overflow: visible;"
        ]
        $
          [ SE.element (H.ElemName "svg")
              [ HP.style "overflow: visible;"
              , HP.ref $ H.RefLabel $ "scoreStaff"
              ]
              []
          ]
    -- <> (A.mapWithIndex (renderTime sett ((graph.maxd + 1.0 + extraRows) * systemHeight)) piece)
    ]
  where

  width = scalex sett (graph.maxx + 1.0) + sliceWidth / 2.0

  systemHeight = if staffType == GrandStaff then scoreHeightGrand else scoreHeightSingle

  extraRows = if isComplete then 0.0 else 1.0

  height = systemHeight * (graph.maxd + 1.0 + extraRows) + axisHeight

renderReduction :: forall p. AppSettings -> Piece -> Graph -> BottomSurface -> Selection -> HH.HTML p ViewerAction
renderReduction sett piece graph surface selection =
  HH.div_
    [ SE.svg
        [ SA.width width
        , SA.height height
        , SA.viewBox (negate $ scalex sett 1.0) (-offset 1) width height
        ]
        (svgTranss <> svgHoris <> svgSlices)
    ]
  where
  { slices, transitions, horis, maxx, maxd } = graph

  width = scalex sett (maxx + 2.0)

  -- { height: innerHeight, svg: svgInner } = renderInner sett selection maxx surface
  -- extraHeight = axisHeight + scoreHeight -- + innerHeight
  height = scaley sett maxd + offset (deepestSize + 1) -- + extraHeight

  deepestSize = fromMaybe 1 $ maximum $ map (_.slice >>> getInnerNotes >>> \ns -> max (A.length ns) 1) $ L.filter (\s -> s.depth == maxd) $ M.values graph.slices

  svgSlices = map (renderSlice sett selection) $ A.fromFoldable $ M.values slices

  svgTranss = map (renderTrans sett selection slices) $ A.fromFoldable $ M.values transitions

  svgHoris = map (renderHori sett selection slices) $ A.fromFoldable horis

-- svgAxis = A.mapWithIndex (renderTime sett 0.0) piece
-- svgScore = [ renderScore ]
