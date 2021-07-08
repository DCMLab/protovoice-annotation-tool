module Render where

import Prelude
import Common (MBS)
import CommonApp (GraphAction(..), Selection(..), addParentToNote, noteIsSelected)
import Data.Array (catMaybes, elem, findIndex, fromFoldable, length, mapWithIndex)
import Data.Int (toNumber)
import Data.Map as M
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Ratio ((%))
import Folding (Graph, GraphTransition, GraphSlice, reductionToLeftmost)
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HA
import Halogen.HTML.Properties as HP
import Halogen.Svg.Attributes as SA
import Halogen.Svg.Elements as SE
import Model (LeftOrnament(..), RightOrnament(..), DoubleOrnament(..), Edge, Note, NoteExplanation(..), Notes, Piece, Reduction, SliceId, StartStop(..), explHasParent, getInnerNotes, getParents)
import Validation (EdgeStatus(..), NoteStatus(..), SliceStatus(..), Validation)
import Web.UIEvent.MouseEvent (ctrlKey)

scalex :: Number -> Number
scalex x = x * 60.0

scaley :: Number -> Number
scaley y = y * 60.0

offset :: Int -> Number
offset i = toNumber i * 20.0

findPitchIndex :: StartStop Note -> StartStop Notes -> Int
findPitchIndex (Inner note) (Inner notes) =
  fromMaybe 0
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

selColor :: Maybe SA.Color
selColor = Just $ SA.RGB 30 144 255

selColor' :: Maybe SA.Color
selColor' = Just $ SA.RGB 135 206 250

parentColor :: Maybe SA.Color
parentColor = selColor'

warnColor :: Maybe SA.Color
warnColor = Just $ SA.RGB 255 165 0

errColor :: Maybe SA.Color
errColor = Just $ SA.RGB 255 0 0

-- noteSelColor :: SA.Color
-- noteSelColor = SA.RGB 255 0 0
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

renderSlice :: forall p. Selection -> Validation -> GraphSlice -> HH.HTML p GraphAction
renderSlice selection validation { slice: { id, notes, x, parents }, depth: d } = case notes of
  Inner inotes ->
    SE.g (if selectable then selectionAttr else [])
      ( [ SE.rect
            [ SA.x $ scalex x - scalex 0.24
            , SA.y $ scaley d - scaley 0.24
            , SA.width $ scalex 0.48
            , SA.height $ offset (length inotes - 1) + scaley 0.48
            , SA.fill white
            , SA.stroke $ if selected then selColor else if activeParent then parentColor else if sliceInvalid then errColor else white
            ]
        ]
          <> mapWithIndex mknote inotes
      )
  startstop -> mknode [ HH.text $ show startstop ] (scalex x) (scaley d) NotSelected NSOk []
  where
  selected = selection == SelSlice id

  activeParent = case selection of
    SelNote { parents: noteParents } -> elem id $ getParents noteParents
    _ -> false

  sliceInvalid = M.lookup id validation.slices == Just SSInvalidReduction

  selectable = d == 0.0

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
        [ SA.x $ xcoord - scalex 0.24
        , SA.y $ ycoord - (0.5 * offset 1)
        , SA.width $ scalex 0.48
        , SA.height $ offset 1
        , SA.fill
            $ case selStatus of
                NotSelected -> white
                Selected -> selColor
                Related -> selColor'
        ]

    label =
      SE.text
        [ SA.x xcoord
        , SA.y ycoord
        , SA.text_anchor SA.AnchorMiddle
        , SA.dominant_baseline SA.BaselineMiddle
        , SA.fill case valid of
            NSOk -> lightgray
            NSNoExpl -> case selStatus of
              Selected -> white
              _ -> black
            NSInvalidExplanation -> warnColor
        ]
        text

  mknote :: Int -> { note :: Note, expl :: NoteExplanation } -> HH.HTML p GraphAction
  mknote i { note, expl } =
    mknode
      label
      (scalex x)
      (scaley d + offset i)
      nodeselected
      (fromMaybe NSOk $ M.lookup note.id validation.notes)
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

renderTrans :: forall p. Selection -> Validation -> M.Map SliceId GraphSlice -> GraphTransition -> HH.HTML p GraphAction
renderTrans selection validation slices { id, left, right, edges } =
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
                  , SA.stroke if transSelected then selColor else lightgray
                  , SA.strokeWidth 15.0
                  ]
                <> if selectable then selectionAttr else []
            ]

          mkedge :: Boolean -> Edge -> HH.HTML p GraphAction
          mkedge isPassing edge@{ left: p1, right: p2 } =
            SE.line
              [ SA.x1 $ scalex xl
              , SA.y1 $ scaley yl + offset offl
              , SA.x2 $ scalex xr
              , SA.y2 $ scaley yr + offset offr
              , SA.stroke
                  if edgeSelected then
                    selColor
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

          edgeLines = map (mkedge false) edges.regular <> map (mkedge true) edges.passing
        pure $ SE.g [] (bar <> edgeLines)
  where
  transSelected = selection == SelTrans id

  selectionAttr =
    [ cursor "pointer"
    , HE.onClick $ \_ -> Select (if transSelected then SelNone else SelTrans id)
    ]

renderHori ::
  forall p.
  Selection ->
  M.Map SliceId GraphSlice ->
  { child :: SliceId, parent :: SliceId } ->
  HH.HTML p GraphAction
renderHori selection slices { child, parent } =
  fromMaybe (HH.text "")
    $ do
        { depth: yc, slice: slicec@{ x: xc, notes: notesc } } <- M.lookup child slices
        { depth: yp, slice: { x: xp, notes: notesp } } <- M.lookup parent slices
        let
          bar =
            [ SE.line
                $ [ SA.x1 $ scalex xp
                  , SA.y1 $ scaley yp
                  , SA.x2 $ scalex xc
                  , SA.y2 $ scaley yc
                  , SA.stroke lightgray
                  , SA.strokeWidth 3.0
                  , SA.attr (HH.AttrName "stroke-dasharray") "10,5"
                  ]
            ]

          mkedge :: { parentNote :: Note, childNote :: Note } -> HH.HTML p GraphAction
          mkedge { parentNote, childNote } =
            SE.line
              [ SA.x1 $ scalex xp
              , SA.y1 $ scaley yp + offset offp
              , SA.x2 $ scalex xc
              , SA.y2 $ scaley yc + offset offc
              , SA.stroke if edgeSelected then selColor else black
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

renderTime :: forall r p. Int -> { time :: MBS | r } -> HH.HTML p GraphAction
renderTime i { time }
  | time.s == 0 % 1 =
    SE.text
      [ SA.x $ scalex $ toNumber (i + 1)
      , SA.y $ scaley (-0.5)
      , SA.text_anchor SA.AnchorMiddle
      , SA.dominant_baseline SA.BaselineMiddle
      ]
      [ HH.text $ show time.m <> "." <> show time.b ]
  | otherwise = HH.text ""

renderReduction :: forall p. Piece -> Graph -> Validation -> Selection -> HH.HTML p GraphAction
renderReduction piece graph validation selection =
  HH.div
    [ HP.style "overflow-x: scroll;" ]
    [ SE.svg
        [ SA.width width
        , SA.height height
        , SA.viewBox (negate $ scalex 1.0) (negate $ scaley 1.0) width height
        ]
        (svgTranss <> svgHoris <> svgSlices <> svgAxis)
    ]
  where
  { slices, transitions, horis, maxx, maxd } = graph

  width = scalex (maxx + 2.0)

  height = scaley (maxd + 4.0)

  svgSlices = map (renderSlice selection validation) $ fromFoldable $ M.values slices

  svgTranss = map (renderTrans selection validation slices) $ fromFoldable $ M.values transitions

  svgHoris = map (renderHori selection slices) $ fromFoldable horis

  svgAxis = mapWithIndex renderTime piece

renderLeftmost :: forall p. Reduction -> HH.HTML p GraphAction
renderLeftmost red = HH.ol_ $ map (\step -> HH.li_ [ HH.text $ show step ]) steps
  where
  steps = reductionToLeftmost red

gridDiv :: forall p. String -> Array (HH.HTML p GraphAction) -> HH.HTML p GraphAction
gridDiv classes = HH.div [ HA.class_ $ HH.ClassName classes ]

mkOption :: forall o p. (Maybe o -> GraphAction) -> { k :: Maybe o, v :: String, s :: Boolean } -> HH.HTML p GraphAction
mkOption updateAction { k, v, s } = HH.option [ HA.value v, HA.selected s, HE.onClick \_ -> updateAction k ] [ HH.text v ]

mkSelect :: forall o p. Show o => Eq o => (Maybe o -> GraphAction) -> Maybe o -> Array o -> HH.HTML p GraphAction
mkSelect updateAction orn opts = HH.select_ options
  where
  options = mkOption updateAction <$> [ { k: Nothing, v: "Nothing", s: false } ] <> map (\o -> { k: Just o, v: show o, s: Just o == orn }) opts

doubleOrnaments :: Array DoubleOrnament
doubleOrnaments =
  [ RootNote
  , FullNeighbor
  , FullRepeat
  , LeftRepeatOfRight
  , RightRepeatOfLeft
  , PassingMid
  , PassingLeft
  , PassingRight
  ]

renderNoteExplanation :: forall p. Graph -> Selection -> HH.HTML p GraphAction
renderNoteExplanation graph sel =
  HH.div
    [ HA.style "height:30px;", HA.class_ $ HH.ClassName "pure-g" ] case sel of
    SelNote { note, parents, expl } ->
      [ gridDiv "pure-u-1-4" [ HH.text $ show note.pitch <> " (" <> note.id <> ") " ] ]
        <> case expl of
            NoExpl -> [ gridDiv "pure-u-1-4" [ HH.text "(no parents)" ] ]
            HoriExpl n ->
              [ gridDiv "pure-u-1-4"
                  [ HH.text $ "parent: " <> show n.pitch <> " (" <> n.id <> ")"
                  , HH.button [ HE.onClick $ \_ -> SetNoteExplanation { noteId: note.id, expl: NoExpl } ] [ HH.text "x" ]
                  ]
              ]
            LeftExpl lxpl@{ orn, rightParent } ->
              [ gridDiv "pure-u-1-4" []
              , gridDiv "pure-u-1-4"
                  [ mkSelect (\orn' -> SetNoteExplanation { noteId: note.id, expl: LeftExpl lxpl { orn = orn' } })
                      orn
                      [ LeftRepeat, LeftNeighbor ]
                  ]
              , gridDiv "pure-u-1-4"
                  [ HH.text $ "right parent: " <> show rightParent.pitch <> " (" <> rightParent.id <> ")"
                  , HH.button [ HE.onClick $ \_ -> SetNoteExplanation { noteId: note.id, expl: NoExpl } ] [ HH.text "x" ]
                  ]
              ]
            RightExpl rxpl@{ orn, leftParent } ->
              [ gridDiv "pure-u-1-4"
                  [ HH.text $ "left parent: " <> show leftParent.pitch <> " (" <> leftParent.id <> ")"
                  , HH.button [ HE.onClick $ \_ -> SetNoteExplanation { noteId: note.id, expl: NoExpl } ] [ HH.text "x" ]
                  ]
              , gridDiv "pure-u-1-4"
                  [ mkSelect (\orn' -> SetNoteExplanation { noteId: note.id, expl: RightExpl rxpl { orn = orn' } })
                      orn
                      [ RightRepeat, RightNeighbor ]
                  ]
              ]
            DoubleExpl dxpl@{ orn, rightParent, leftParent } ->
              [ gridDiv "pure-u-1-4"
                  [ HH.text $ "left parent: " <> show leftParent.pitch <> " (" <> leftParent.id <> ")"
                  , HH.button
                      [ HE.onClick
                          $ \_ ->
                              SetNoteExplanation
                                { noteId: note.id
                                , expl: LeftExpl { orn: Nothing, rightParent }
                                }
                      ]
                      [ HH.text "x" ]
                  ]
              , gridDiv "pure-u-1-4"
                  [ mkSelect (\orn' -> SetNoteExplanation { noteId: note.id, expl: DoubleExpl dxpl { orn = orn' } })
                      orn
                      doubleOrnaments
                  ]
              , gridDiv "pure-u-1-4"
                  [ HH.text $ "right parent: " <> show rightParent.pitch <> " (" <> rightParent.id <> ")"
                  , HH.button
                      [ HE.onClick
                          $ \_ ->
                              SetNoteExplanation
                                { noteId: note.id
                                , expl: RightExpl { orn: Nothing, leftParent }
                                }
                      ]
                      [ HH.text "x" ]
                  ]
              ]
    _ -> [ gridDiv "pure-u-1" [ HH.text "No note selected." ] ]
