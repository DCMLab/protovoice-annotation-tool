module App.Render where

import Prelude
import App.Common (AppSettings, GraphAction(..), Selection(..), addParentToNote, noteIsSelected, removeParent)
import Data.Array (catMaybes, elem, findIndex, fromFoldable, length, mapWithIndex)
import Data.Array as A
import Data.Either (Either(..))
import Data.Int (toNumber)
import Data.Map as M
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Ratio ((%))
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Core as HC
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HA
import Halogen.HTML.Properties as HP
import Halogen.Query.Input (Input(..))
import Halogen.Svg.Attributes as SA
import Halogen.Svg.Elements as SE
import ProtoVoices.Common (MBS(..))
import ProtoVoices.Folding (Graph, GraphSlice, GraphTransition)
import ProtoVoices.Model (DoubleOrnament(..), Edge, LeftOrnament(..), Note, NoteExplanation(..), Notes, Parents, Piece, RightOrnament(..), SliceId, StartStop(..), explHasParent, getInnerNotes, getParents, setHoriExplParent, setLeftExplParent, setRightExplParent)
import ProtoVoices.Validation (EdgeError(..), NoteError(..), SliceError(..), Validation)
import Web.UIEvent.MouseEvent (ctrlKey)

scalex :: AppSettings -> Number -> Number
scalex { xscale } x = x * xscale

scaley :: AppSettings -> Number -> Number
scaley { yscale } y = y * yscale

offset :: Int -> Number
offset i = toNumber i * 20.0

noteSize :: Number
noteSize = 29.0

scoreHeight :: Number
scoreHeight = 150.0

scoreScale :: Number
scoreScale = 0.9

axisHeight :: Number
axisHeight = 60.0

findPitchIndex :: StartStop Note -> StartStop Notes -> Int
findPitchIndex (Inner note) (Inner notes) =
  fromMaybe (-1)
    $ findIndex
        (\n -> n.note == note)
        notes

findPitchIndex _ _ = 0

-- custom elements and attributes
cursor :: forall r i. String -> HH.IProp r i
cursor = SA.attr $ HH.AttrName "cursor"

tspan :: forall p r i. Array (HH.IProp r i) -> Array (HH.HTML p i) -> HH.HTML p i
tspan = SE.element $ HH.ElemName "tspan"

dy :: forall r i. String -> HH.IProp r i
dy = SA.attr $ HH.AttrName "dy"

svgFilter :: forall r i. String -> HH.IProp r i
svgFilter = SA.attr $ HH.AttrName "filter"

selColorOuter :: Maybe SA.Color
selColorOuter = Just $ SA.RGB 30 144 255

selColorOuter' :: Maybe SA.Color
selColorOuter' = Just $ SA.RGB 135 206 250

selColorInner :: Maybe SA.Color
selColorInner = selColorOuter -- Just $ SA.RGB 51 160 44

selColorInner' :: Maybe SA.Color
selColorInner' = selColorOuter' -- Just $ SA.RGB 178 223 138

parentColor :: Maybe SA.Color
parentColor = selColorInner'

warnColor :: Maybe SA.Color
warnColor = Just $ SA.RGB 255 165 0

errColor :: Maybe SA.Color
errColor = Just $ SA.RGB 255 0 0

white :: Maybe SA.Color
white = Just $ SA.RGB 255 255 255

black :: Maybe SA.Color
black = Just $ SA.RGB 0 0 0

lightgray :: Maybe SA.Color
lightgray = Just $ SA.RGB 211 211 211

data SelectionStatus
  = NotSelected
  | Selected
  | Related

derive instance eqSelectionStatus :: Eq SelectionStatus

renderSlice :: forall p. AppSettings -> Selection -> Validation -> GraphSlice -> HH.HTML p GraphAction
renderSlice sett selection validation { slice: slice@{ id, notes, x, parents }, depth: d } = case notes of
  Inner inotes ->
    SE.g []
      $ [ SE.g (if isTopLevel then selectionAttr else [])
            $ [ SE.rect
                  [ SA.x svgx
                  , SA.y $ scaley sett d - (noteSize / 2.0)
                  , SA.width noteSize
                  , SA.height $ offset (length inotes - 1) + noteSize
                  , SA.fill white
                  , SA.stroke $ if selected then selColorOuter else if activeParent then parentColor else if sliceInvalid then errColor else white
                  ]
              ]
            <> mapWithIndex mknote inotes
        ]
  startstop -> mknode [ HH.text $ show startstop ] (scalex sett x) (scaley sett d) (if activeParent then Related else NotSelected) Nothing []
  where
  svgx = scalex sett x - (noteSize / 2.0)

  selected = selection == SelSlice id

  activeParent = case selection of
    SelNote { parents: noteParents } -> elem id $ getParents noteParents
    _ -> false

  sliceInvalid = M.lookup id validation.slices == Just SSInvalidReduction

  isTopLevel = d == 0.0

  selectionAttr =
    [ cursor "pointer"
    , HE.onClick $ \ev -> if ctrlKey ev then NoOp else Select (if selected then SelNone else SelSlice id)
    ]

  mknode text xcoord ycoord selStatus valid attr =
    SE.g attr
      $ [ bg, label ]
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
      SE.text
        [ SA.x xcoord
        , SA.y ycoord
        , SA.text_anchor SA.AnchorMiddle
        , SA.dominant_baseline SA.BaselineMiddle
        , SA.fill case valid of
            Nothing -> if selStatus == Related then white else lightgray
            Just NSNoExpl -> case selStatus of
              Selected -> white
              _ -> black
            Just NSInvalidExplanation -> warnColor
        ]
        text

  mknote :: Int -> { note :: Note, expl :: NoteExplanation } -> HH.HTML p GraphAction
  mknote i { note, expl } =
    mknode
      label
      (scalex sett x)
      (scaley sett d + offset i)
      nodeselected
      (M.lookup note.id validation.notes)
      (if clickable then attrsSel else [])
    where
    nselected = noteIsSelected selection (Inner note)

    nselectedParent = case selection of
      SelNote { expl: selExpl } -> explHasParent note.id selExpl
      _ -> false

    nodeselected = if nselected then Selected else if nselectedParent then Related else NotSelected

    nselectable = d /= 0.0

    clickable = nselectable || activeParent

    label =
      [ HH.text $ show note.pitch
      , SE.title [] [ HH.text note.id ]
      ]

    attrsSel =
      [ cursor "pointer"
      , HE.onClick
          $ \ev ->
              if ctrlKey ev then
                if activeParent then
                  addParentToNote selection id note
                else
                  NoOp
              else if nselectable then
                Select if nselected then SelNone else SelNote { note, expl, parents }
              else
                NoOp
      ]

renderTrans :: forall p. AppSettings -> Selection -> Validation -> M.Map SliceId GraphSlice -> GraphTransition -> HH.HTML p GraphAction
renderTrans sett selection validation slices { id, left, right, edges } =
  fromMaybe (HH.text "")
    $ do
        { depth: yl, slice: { x: xl, notes: nl } } <- M.lookup left slices
        { depth: yr, slice: { x: xr, notes: nr } } <- M.lookup right slices
        let
          selectable = yl == 0.0 && yr == 0.0

          bar =
            [ SE.line
                $ [ SA.x1 $ scalex sett xl
                  , SA.y1 $ scaley sett yl
                  , SA.x2 $ scalex sett xr
                  , SA.y2 $ scaley sett yr
                  , SA.stroke if transSelected then selColorOuter else lightgray
                  , SA.strokeWidth $ if selectable then (noteSize / 2.0) else 5.0
                  ]
                <> if selectable then selectionAttr else []
            ]

          mkedge :: Boolean -> Edge -> HH.HTML p GraphAction
          mkedge isPassing edge@{ left: p1, right: p2 } =
            SE.line
              [ SA.x1 $ scalex sett xl
              , SA.y1 $ scaley sett yl + offset offl
              , SA.x2 $ scalex sett xr
              , SA.y2 $ scaley sett yr + offset offr
              , SA.stroke
                  if edgeSelected then
                    selColorInner
                  else case M.lookup edge validation.edges of
                    Just ESNotUsed -> warnColor
                    Just ESNotStepwise -> errColor
                    Just ESNotRepetition -> errColor
                    _ -> black
              , SA.strokeWidth 1.0
              , SA.attr (HH.AttrName "stroke-dasharray") (if isPassing then "6,3" else "")
              ]
            where
            offl = findPitchIndex p1 nl

            offr = findPitchIndex p2 nr

            edgeSelected = noteIsSelected selection p1 || noteIsSelected selection p2

          edgeLines = map (mkedge false) (A.fromFoldable edges.regular) <> map (mkedge true) (A.fromFoldable edges.passing)
        pure $ SE.g [] (bar <> edgeLines)
  where
  transSelected = selection == SelTrans id

  selectionAttr =
    [ cursor "pointer"
    , HE.onClick $ \_ -> Select (if transSelected then SelNone else SelTrans id)
    ]

renderHori ::
  forall p.
  AppSettings ->
  Selection ->
  M.Map SliceId GraphSlice ->
  { child :: SliceId, parent :: SliceId } ->
  HH.HTML p GraphAction
renderHori sett selection slices { child, parent } =
  fromMaybe (HH.text "")
    $ do
        { depth: yc, slice: slicec@{ x: xc, notes: notesc } } <- M.lookup child slices
        { depth: yp, slice: { x: xp, notes: notesp } } <- M.lookup parent slices
        let
          bar =
            [ SE.line
                $ [ SA.x1 $ scalex sett xp
                  , SA.y1 $ scaley sett yp
                  , SA.x2 $ scalex sett xc
                  , SA.y2 $ scaley sett yc
                  , SA.stroke lightgray
                  , SA.strokeWidth 5.0
                  , SA.attr (HH.AttrName "stroke-dasharray") "10,5"
                  ]
            ]

          mkedge :: { parentNote :: Note, childNote :: Note } -> HH.HTML p GraphAction
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

          edges = map mkedge $ catMaybes $ map explToHori $ getInnerNotes slicec
        pure $ SE.g [] $ bar <> edges
  where
  explToHori note = case note.expl of
    HoriExpl parentNote -> Just { childNote: note.note, parentNote }
    _ -> Nothing

renderTime :: forall r p. AppSettings -> Int -> { time :: Either String MBS | r } -> HH.HTML p GraphAction
renderTime sett i { time } =
  SE.text
    [ SA.x $ scalex sett $ toNumber (i + 1)
    , SA.y $ negate (axisHeight / 2.0)
    , SA.text_anchor SA.AnchorMiddle
    , SA.dominant_baseline SA.BaselineMiddle
    ]
    [ HH.text label ]
  where
  label = case time of
    Right (MBS { m, b, s }) -> if s == 0 % 1 then show m <> "." <> show b else ""
    Left str -> str

renderScore :: forall p. HH.HTML p GraphAction
renderScore =
  SE.element (H.ElemName "svg")
    [ SA.x 0.0
    , SA.y (negate $ scoreHeight + axisHeight)
    , HP.style "overflow: visible;"
    , HP.ref $ H.RefLabel $ "scoreStaff"
    , HP.IProp $ HC.ref $ map (Action <<< RegisterScoreElt)
    ]
    []

renderReduction :: forall p. AppSettings -> Piece -> Graph -> Validation -> Selection -> HH.HTML p GraphAction
renderReduction sett piece graph validation selection =
  HH.div
    [ HP.style "overflow-x: scroll; max-width: max-content;" ]
    [ SE.svg
        [ SA.width width
        , SA.height height
        , SA.viewBox (negate $ scalex sett 1.0) (negate extraHeight) width height
        ]
        (svgScore <> svgTranss <> svgHoris <> svgSlices <> svgAxis)
    ]
  where
  { slices, transitions, horis, maxx, maxd } = graph

  width = scalex sett (maxx + 2.0)

  extraHeight = axisHeight + if sett.showScore then scoreHeight else 0.0

  height = scaley sett (maxd + 2.0) + extraHeight

  svgSlices = map (renderSlice sett selection validation) $ fromFoldable $ M.values slices

  svgTranss = map (renderTrans sett selection validation slices) $ fromFoldable $ M.values transitions

  svgHoris = map (renderHori sett selection slices) $ fromFoldable horis

  svgAxis = mapWithIndex (renderTime sett) piece

  svgScore = if sett.showScore then [ renderScore ] else []

class_ :: forall r i. String -> HH.IProp ( class :: String | r ) i
class_ str = HA.class_ $ HH.ClassName str

mkOption :: forall o p. (Maybe o -> GraphAction) -> { k :: Maybe o, v :: String, s :: Boolean } -> HH.HTML p GraphAction
mkOption updateAction { k, v, s } =
  HH.option
    [ HA.value v
    , HA.selected s
    , HE.onClick \_ -> updateAction k
    ]
    [ HH.text v ]

mkSelect :: forall o p. Show o => Eq o => (Maybe o -> GraphAction) -> Maybe o -> Array o -> HH.HTML p GraphAction
mkSelect updateAction orn opts = HH.select_ options
  where
  options = mkOption updateAction <$> [ { k: Nothing, v: "Nothing", s: false } ] <> map (\o -> { k: Just o, v: show o, s: Just o == orn }) opts

doubleOrnaments :: Array DoubleOrnament
doubleOrnaments =
  [ FullNeighbor
  , FullRepeat
  , LeftRepeatOfRight
  , RightRepeatOfLeft
  , PassingMid
  , PassingLeft
  , PassingRight
  ]

renderNoteExplanation :: forall p. Graph -> Note -> NoteExplanation -> Parents SliceId -> HH.HTML p GraphAction
renderNoteExplanation graph note expl parents =
  HH.div [ class_ "pure-g", HA.style "height:30px;" ]
    $ [ HH.label [ class_ "pure-u-1-4" ] [ HH.text $ "Note selected: " <> show note.pitch ] ]
    <> case expl of
        NoExpl -> [ HH.label [ class_ "pure-u-1-4" ] [ HH.text "no parents" ] ]
        RootExpl -> [ HH.label [ class_ "pure-u-1-4" ] [ HH.text "root note" ] ]
        HoriExpl n ->
          [ HH.button [ class_ "pure-u-1-24", HE.onClick $ \_ -> removeParent note expl setHoriExplParent ] [ HH.text "x" ]
          , HH.label [ class_ "pure-u-5-24" ] [ HH.text $ " parent: " <> show n.pitch ]
          ]
        LeftExpl lxpl@{ orn, rightParent } ->
          [ HH.div [ class_ "pure-u-1-4" ] []
          , HH.div [ class_ "pure-u-1-4" ]
              [ mkSelect (\orn' -> SetNoteExplanation { noteId: note.id, expl: LeftExpl lxpl { orn = orn' } })
                  orn
                  [ LeftRepeat, LeftNeighbor ]
              ]
          , HH.button [ class_ "pure-u-1-24", HE.onClick $ \_ -> removeParent note expl setRightExplParent ] [ HH.text "x" ]
          , HH.label [ class_ "pure-u-5-24" ] [ HH.text $ " right parent: " <> show rightParent.pitch ]
          ]
        RightExpl rxpl@{ orn, leftParent } ->
          [ HH.button [ class_ "pure-u-1-24", HE.onClick $ \_ -> removeParent note expl setLeftExplParent ] [ HH.text "x" ]
          , HH.label [ class_ "pure-u-5-24" ] [ HH.text $ " left parent: " <> show leftParent.pitch ]
          , HH.div [ class_ "pure-u-1-4" ]
              [ mkSelect (\orn' -> SetNoteExplanation { noteId: note.id, expl: RightExpl rxpl { orn = orn' } })
                  orn
                  [ RightRepeat, RightNeighbor ]
              ]
          ]
        DoubleExpl dxpl@{ orn, rightParent, leftParent } ->
          [ HH.button [ class_ "pure-u-1-24", HE.onClick $ \_ -> removeParent note expl setLeftExplParent ] [ HH.text "x" ]
          , HH.label [ class_ "pure-u-5-24" ] [ HH.text $ " left parent: " <> show leftParent.pitch ]
          , HH.div [ class_ "pure-u-1-4" ]
              [ mkSelect (\orn' -> SetNoteExplanation { noteId: note.id, expl: DoubleExpl dxpl { orn = orn' } })
                  orn
                  doubleOrnaments
              ]
          , HH.button [ class_ "pure-u-1-24", HE.onClick $ \_ -> removeParent note expl setRightExplParent ] [ HH.text "x" ]
          , HH.label [ class_ "pure-u-5-24" ] [ HH.text $ " right parent: " <> show rightParent.pitch ]
          ]
