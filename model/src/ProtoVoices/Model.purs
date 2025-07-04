module ProtoVoices.Model where

import Prelude hiding (degree)

import Control.Alt ((<|>))
import Data.Array (filter, sortBy)
import Data.Array as A
import Data.Either (Either(..))
import Data.Foldable (all, find, foldMap, foldl, for_, intercalate)
import Data.FunctorWithIndex (mapWithIndex)
import Data.Generic.Rep (class Generic)
import Data.Int (toNumber)
import Data.List (List(..), (:))
import Data.List as L
import Data.Map as M
import Data.Maybe (Maybe(..), fromMaybe, maybe)
import Data.Monoid (power)
import Data.Ordering (invert)
import Data.Pitches (class Interval, Pitch, SPitch, degree, direction, ic, isStep, pc, pto, unison)
import Data.Set as S
import Data.Show.Generic (genericShow)
import Data.String as Str
import Data.Traversable (scanl)
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.Console (log, logShow)
import Foreign as F
import ProtoVoices.Common (MBS)
import Simple.JSON (readImpl, writeImpl)
import Simple.JSON as JSON

-- types
-- -----
--
type Time = Either String MBS

type Note = { pitch :: SPitch, id :: String }

type Piece = Array { time :: Time, notes :: Array { hold :: Boolean, note :: Note } }

data RightOrnament
  = RightRepeat
  | RightNeighbor

derive instance eqRightOrnament :: Eq RightOrnament

derive instance genericRightOrnament :: Generic RightOrnament _

instance showRightOrnament :: Show RightOrnament where
  show ro = genericShow ro

data LeftOrnament
  = LeftRepeat
  | LeftNeighbor

derive instance eqLeftOrnament :: Eq LeftOrnament

derive instance genericLeftOrnament :: Generic LeftOrnament _

instance showLeftOrnament :: Show LeftOrnament where
  show lo = genericShow lo

data DoubleOrnament
  = FullNeighbor
  | FullRepeat
  | LeftRepeatOfRight
  | RightRepeatOfLeft
  | PassingMid
  | PassingLeft
  | PassingRight

derive instance eqDoubleOrnament :: Eq DoubleOrnament

derive instance genericDoubleOrnament :: Generic DoubleOrnament _

instance showDoubleOrnament :: Show DoubleOrnament where
  show o = genericShow o

data NoteExplanation
  = NoExpl
  | RootExpl
  | HoriExpl Note
  | RightExpl { orn :: Maybe RightOrnament, leftParent :: Note }
  | LeftExpl { orn :: Maybe LeftOrnament, rightParent :: Note }
  | DoubleExpl { orn :: Maybe DoubleOrnament, leftParent :: Note, rightParent :: Note }

derive instance eqNoteExplanation :: Eq NoteExplanation

derive instance genericNoteExplanation :: Generic NoteExplanation _

instance showNoteExplanation :: Show NoteExplanation where
  show o = genericShow o

explParentEdge :: NoteExplanation -> Edges
explParentEdge = case _ of
  DoubleExpl { orn, leftParent: l, rightParent: r } ->
    let
      parentEdge = { left: Inner l, right: Inner r }
    in
      case orn of
        Just PassingMid -> emptyEdges { passing = [ parentEdge ] }
        Just PassingLeft -> emptyEdges { passing = [ parentEdge ] }
        Just PassingRight -> emptyEdges { passing = [ parentEdge ] }
        _ -> emptyEdges { regular = S.singleton parentEdge }
  RootExpl -> emptyEdges { regular = S.singleton { left: Start, right: Stop } }
  _ -> emptyEdges

explLeftEdge :: { note :: Note, expl :: NoteExplanation } -> Edges
explLeftEdge { note: m, expl } = case expl of
  DoubleExpl { orn, leftParent } -> case orn of
    Just PassingRight -> emptyEdges { passing = [ { left: Inner leftParent, right: Inner m } ] }
    _ -> emptyEdges { regular = edge leftParent }
  RightExpl { leftParent } -> emptyEdges { regular = edge leftParent }
  RootExpl -> emptyEdges { regular = S.singleton { left: Start, right: Inner m } }
  _ -> emptyEdges
  where
  edge l = S.singleton { left: Inner l, right: Inner m }

explRightEdge :: { note :: Note, expl :: NoteExplanation } -> Edges
explRightEdge { note: m, expl } = case expl of
  DoubleExpl { orn, rightParent } -> case orn of
    Just PassingLeft -> emptyEdges { passing = [ { left: Inner m, right: Inner rightParent } ] }
    _ -> emptyEdges { regular = edge rightParent }
  LeftExpl { rightParent } -> emptyEdges { regular = edge rightParent }
  RootExpl -> emptyEdges { regular = S.singleton { left: Inner m, right: Stop } }
  _ -> emptyEdges
  where
  edge r = S.singleton { left: Inner m, right: Inner r }

parentEdges :: Slice -> Edges
parentEdges = case _ of
  { notes: Inner notes } -> foldMap explParentEdge (_.expl <$> notes)
  _ -> emptyEdges

explIsSplit :: NoteExplanation -> Boolean
explIsSplit = case _ of
  HoriExpl _ -> false
  _ -> true

explIsHori :: NoteExplanation -> Boolean
explIsHori = case _ of
  NoExpl -> true
  HoriExpl _ -> true
  _ -> false

findLeftOrn :: SPitch -> Note -> Maybe LeftOrnament
findLeftOrn child { pitch: parent }
  | pc child == pc parent = Just LeftRepeat
  | isStep $ ic (child `pto` parent) = Just LeftNeighbor
  | otherwise = Nothing

findRightOrn :: SPitch -> Note -> Maybe RightOrnament
findRightOrn child { pitch: parent }
  | pc child == pc parent = Just RightRepeat
  | isStep $ ic (child `pto` parent) = Just RightNeighbor
  | otherwise = Nothing

pbetween :: forall i. Interval i => Eq i => Pitch i -> Pitch i -> Pitch i -> Boolean
pbetween l m r = l /= m && m /= r && l /= r && dir1 == odir && dir2 == odir
  where
  odir = direction $ l `pto` r

  dir1 = direction $ l `pto` m

  dir2 = direction $ m `pto` r

findDoubleOrn :: SPitch -> Note -> Note -> Maybe DoubleOrnament
findDoubleOrn child { pitch: left } { pitch: right }
  | pc child == pc left, pc child == pc right = Just FullRepeat
  | pc child == pc left, isStep (ic (child `pto` right)) = Just RightRepeatOfLeft
  | pc child == pc right, isStep (ic (child `pto` left)) = Just LeftRepeatOfRight
  | degree (pc child) == degree (pc left)
  , i1 <- ic $ left `pto` child
  , i2 <- ic $ child `pto` right
  , isStep i2
  , compare i1 unison /= direction i2 = Just RightRepeatOfLeft
  | degree (pc child) == degree (pc right)
  , i1 <- ic $ left `pto` child
  , i2 <- ic $ child `pto` right
  , isStep i1
  , direction i1 /= compare i2 unison = Just LeftRepeatOfRight
  | degree (pc left) == degree (pc right), isStep (ic (child `pto` left)) = Just FullNeighbor
  | otherwise =
      if isStep (ic (child `pto` left)) then
        if isStep (ic (child `pto` right)) then Just PassingMid else Just PassingLeft
      else if isStep (ic (child `pto` right)) then Just PassingRight
      else Nothing

setLeftExplParent :: SPitch -> Maybe Note -> NoteExplanation -> Maybe NoteExplanation
setLeftExplParent child leftParentMaybe expl = case leftParentMaybe of
  Just leftParent -> case expl of
    NoExpl -> Just $ RightExpl { orn: findRightOrn child leftParent, leftParent }
    RightExpl _ -> Just $ RightExpl { orn: findRightOrn child leftParent, leftParent }
    LeftExpl { rightParent } ->
      Just
        $ DoubleExpl
            { orn: findDoubleOrn child leftParent rightParent
            , leftParent
            , rightParent
            }
    DoubleExpl { rightParent } ->
      Just
        $ DoubleExpl
            { orn: findDoubleOrn child leftParent rightParent
            , leftParent
            , rightParent
            }
    _ -> Nothing
  Nothing -> case expl of
    LeftExpl l -> Just $ LeftExpl l
    DoubleExpl { rightParent } -> Just $ LeftExpl { orn: findLeftOrn child rightParent, rightParent }
    HoriExpl _ -> Nothing
    _ -> Just NoExpl

setRightExplParent :: SPitch -> Maybe Note -> NoteExplanation -> Maybe NoteExplanation
setRightExplParent child rightParentMaybe expl = case rightParentMaybe of
  Just rightParent -> case expl of
    NoExpl -> Just $ LeftExpl { orn: findLeftOrn child rightParent, rightParent }
    LeftExpl _ -> Just $ LeftExpl { orn: findLeftOrn child rightParent, rightParent }
    RightExpl { leftParent } ->
      Just
        $ DoubleExpl
            { orn: findDoubleOrn child leftParent rightParent
            , leftParent
            , rightParent
            }
    DoubleExpl { leftParent } ->
      Just
        $ DoubleExpl
            { orn: findDoubleOrn child leftParent rightParent
            , leftParent
            , rightParent
            }
    _ -> Nothing
  Nothing -> case expl of
    RightExpl r -> Just $ RightExpl r
    DoubleExpl { leftParent } -> Just $ RightExpl { orn: findRightOrn child leftParent, leftParent }
    HoriExpl _ -> Nothing
    _ -> Just NoExpl

setHoriExplParent :: SPitch -> Maybe Note -> NoteExplanation -> Maybe NoteExplanation
setHoriExplParent _child parentMaybe expl = case parentMaybe of
  Just parent -> case expl of
    NoExpl -> Just $ HoriExpl parent
    HoriExpl _ -> Just $ HoriExpl parent
    _ -> Nothing
  Nothing -> case expl of
    HoriExpl _ -> Just NoExpl
    NoExpl -> Just NoExpl
    _ -> Nothing

explHasParent :: String -> NoteExplanation -> Boolean
explHasParent id = case _ of
  NoExpl -> false
  RootExpl -> false
  HoriExpl n -> n.id == id
  RightExpl { leftParent } -> leftParent.id == id
  LeftExpl { rightParent } -> rightParent.id == id
  DoubleExpl { leftParent, rightParent } -> leftParent.id == id || rightParent.id == id

explParents :: NoteExplanation -> Array Note
explParents = case _ of
  NoExpl -> []
  RootExpl -> []
  HoriExpl parent -> [ parent ]
  RightExpl { leftParent } -> [ leftParent ]
  LeftExpl { rightParent } -> [ rightParent ]
  DoubleExpl { leftParent, rightParent } -> [ leftParent, rightParent ]

data StartStop a
  = Start
  | Inner a
  | Stop

getInner :: forall a. StartStop a -> Maybe a
getInner (Inner a) = Just a

getInner _ = Nothing

derive instance eqStartStop :: (Eq a) => Eq (StartStop a)

derive instance functorStartStop :: Functor StartStop

derive instance ordStartStop :: (Ord a) => Ord (StartStop a)

instance showStartStop :: (Show a) => Show (StartStop a) where
  show Start = "⋊"
  show Stop = "⋉"
  show (Inner a) = show a

instance writeForeignStartStop :: (JSON.WriteForeign a) => JSON.WriteForeign (StartStop a) where
  writeImpl = case _ of
    Start -> writeImpl "start"
    Stop -> writeImpl "stop"
    Inner a -> writeImpl a

instance readForeignStartStop :: (JSON.ReadForeign a) => JSON.ReadForeign (StartStop a) where
  readImpl frgn = do
    let
      tryOuter = do
        str <- readImpl frgn
        case str of
          "start" -> pure Start
          "stop" -> pure Stop
          _ -> F.fail $ F.ForeignError "StartStop is neither Start nor Stop!"
    tryOuter <|> (Inner <$> readImpl frgn)

type Edge = { left :: StartStop Note, right :: StartStop Note }

isRepeatingEdge :: Edge -> Boolean
isRepeatingEdge = case _ of
  { left: Inner left, right: Inner right } -> pc left.pitch == pc right.pitch
  _ -> false

type Notes = Array { note :: Note, expl :: NoteExplanation }

sortNotes :: forall r. Array { note :: Note | r } -> Array { note :: Note | r }
sortNotes = sortBy (\a b -> invert $ compare a.note.pitch b.note.pitch)

type Edges =
  { regular :: S.Set Edge
  , passing :: Array Edge
  }

emptyEdges :: Edges
emptyEdges = mempty

regularEdge :: Edge -> Edges
regularEdge edge = emptyEdges { regular = S.singleton edge }

passingEdge :: Edge -> Edges
passingEdge edge = emptyEdges { passing = [ edge ] }

newtype SliceId = SliceId Int

instance showSliceId :: Show SliceId where
  show (SliceId i) = "s" <> show i

derive newtype instance eqSliceId :: Eq SliceId

derive newtype instance ordSliceId :: Ord SliceId

derive newtype instance readForeignSliceId :: JSON.ReadForeign SliceId

derive newtype instance writeForeignSliceId :: JSON.WriteForeign SliceId

incS :: SliceId -> SliceId
incS (SliceId i) = SliceId $ i + 1

newtype TransId = TransId Int

instance showTransId :: Show TransId where
  show (TransId i) = "t" <> show i

derive newtype instance eqTransId :: Eq TransId

derive newtype instance ordTransId :: Ord TransId

derive newtype instance readForeignTransId :: JSON.ReadForeign TransId

derive newtype instance writeForeignTransId :: JSON.WriteForeign TransId

incT :: TransId -> TransId
incT (TransId i) = TransId $ i + 1

data Parents s
  = NoParents
  | VertParent s
  | MergeParents { left :: s, right :: s }

derive instance eqParents :: (Eq s) => Eq (Parents s)

derive instance genericParents :: Generic (Parents s) _

instance showParents :: (Show s) => Show (Parents s) where
  show p = genericShow p

getParents :: forall s. Parents s -> Array s
getParents = case _ of
  NoParents -> []
  VertParent p -> [ p ]
  MergeParents { left, right } -> [ left, right ]

type Slice =
  { id :: SliceId
  , notes :: StartStop Notes
  , x :: Number
  , parents :: Parents SliceId
  }

getInnerNotes :: Slice -> Notes
getInnerNotes slice = case slice.notes of
  Start -> []
  Stop -> []
  Inner notes -> notes

type Transition =
  { id :: TransId
  , edges :: Edges
  , is2nd :: Boolean
  }

emptyTrans :: TransId -> Boolean -> Transition
emptyTrans id is2nd = { id, is2nd, edges: emptyEdges }

transEdges :: Transition -> Array Edge
transEdges trans = S.toUnfoldable trans.edges.regular <> trans.edges.passing

data Op
  = Freeze
  | Split { childl :: Segment, childr :: EndSegment }
  | Hori { childl :: Segment, childm :: Segment, childr :: EndSegment }

derive instance eqOp :: Eq Op

derive instance genericOp :: Generic Op _

instance showOp :: Show Op where
  show x = genericShow x

type Segment =
  { trans :: Transition
  , rslice :: Slice
  , op :: Op
  }

resetExpls :: Segment -> Segment
resetExpls seg = seg { rslice { notes = map (\note -> note { expl = NoExpl }) <$> seg.rslice.notes } }

type EndSegment =
  { trans :: Transition
  , op :: Op
  }

attachSegment :: EndSegment -> Slice -> Segment
attachSegment { trans, op } rslice = { trans, op, rslice }

detachSegment :: Segment -> EndSegment
detachSegment { trans, op } = { trans, op }

type BottomSurface = { slices :: Array Slice, transs :: Array Edges }

type Reduction =
  { start :: Slice
  , segments :: List Segment
  , nextTransId :: TransId
  , nextSliceId :: SliceId
  }

type Style =
  { classes :: String
  , label :: String
  }

emptyStyle :: Style
emptyStyle = { classes: "", label: "" }

styleSetClasses :: String -> Maybe Style -> Maybe Style
styleSetClasses classes styleMaybe =
  if Str.null classes && Str.null style.label then Nothing
  else Just style { classes = classes }
  where
  style = fromMaybe emptyStyle styleMaybe

styleSetLabel :: String -> Maybe Style -> Maybe Style
styleSetLabel label styleMaybe =
  if Str.null label && Str.null style.classes then Nothing
  else Just style { label = label }
  where
  style = fromMaybe emptyStyle styleMaybe

data Staff = TrebleStaff | BassStaff | GrandStaff

derive instance eqStaff :: Eq Staff

derive instance genericStaff :: Generic Staff _

instance showStaff :: Show Staff where
  show = genericShow

-- | Translates a Staff value to a string that represents the value in JS code.
staffType2JS :: Staff -> String
staffType2JS = case _ of
  GrandStaff -> "grand"
  TrebleStaff -> "treble"
  BassStaff -> "bass"

type Styles =
  { notes :: M.Map String Style
  , edges :: M.Map (Tuple String String) Style
  , slices :: M.Map SliceId Style
  , transitions :: M.Map TransId Style
  , css :: String
  , staff :: Staff
  }

stylesUpdateNote :: (Maybe Style -> Maybe Style) -> String -> Styles -> Styles
stylesUpdateNote f id styles = styles { notes = M.alter f id styles.notes }

stylesUpdateEdge :: (Maybe Style -> Maybe Style) -> Tuple String String -> Styles -> Styles
stylesUpdateEdge f id styles = styles { edges = M.alter f id styles.edges }

stylesUpdateSlice :: (Maybe Style -> Maybe Style) -> SliceId -> Styles -> Styles
stylesUpdateSlice f id styles = styles { slices = M.alter f id styles.slices }

stylesUpdateTrans :: (Maybe Style -> Maybe Style) -> TransId -> Styles -> Styles
stylesUpdateTrans f id styles = styles { transitions = M.alter f id styles.transitions }

emptyStyles :: Styles
emptyStyles =
  { notes: M.empty
  , edges: M.empty
  , transitions: M.empty
  , slices: M.empty
  , css: defaultCSS
  , staff: GrandStaff
  }

defaultCSS :: String
defaultCSS =
  """text {
    font-size: 16px;
    fill: black;
}
.pv-op-marker {
    stroke: black;
    stroke-width: 1.5;
    fill: none;
}
.pv-edge {
    stroke: black;
}

.cat1, .cat1 svg { fill: #4C72B0; }
.cat2, .cat2 svg { fill: #DD8452; }
.cat3, .cat3 svg { fill: #55A868; }
.cat4, .cat4 svg { fill: #C44E52; }
.cat5, .cat5 svg { fill: #8172B3; }
.cat6, .cat6 svg { fill: #937860; }
.cat7, .cat7 svg { fill: #DA8BC3; }
.cat8, .cat8 svg { fill: #8C8C8C; }
.cat9, .cat9 svg { fill: #CCB974; }
.cat10, .cat10 svg { fill: #64B5CD; }

.cat1b, .cat1b svg { fill: #A1C9F4; }
.cat2b, .cat2b svg { fill: #FFB482; }
.cat3b, .cat3b svg { fill: #8DE5A1; }
.cat4b, .cat4b svg { fill: #FF9F9B; }
.cat5b, .cat5b svg { fill: #D0BBFF; }
.cat6b, .cat6b svg { fill: #DEBB9B; }
.cat7b, .cat7b svg { fill: #FAB0E4; }
.cat8b, .cat8b svg { fill: #CFCFCF; }
.cat9b, .cat9b svg { fill: #FFFEA3; }
.cat10b, .cat10b svg { fill: #B9F2F0; }

.cat1c, .cat10c svg { fill: #001C7F; }
.cat2c, .cat10c svg { fill: #B1400D; }
.cat3c, .cat10c svg { fill: #12711C; }
.cat4c, .cat10c svg { fill: #8C0800; }
.cat5c, .cat10c svg { fill: #591E71; }
.cat6c, .cat10c svg { fill: #592F0D; }
.cat7c, .cat10c svg { fill: #A23582; }
.cat8c, .cat10c svg { fill: #3C3C3C; }
.cat9c, .cat10c svg { fill: #B8850A; }
.cat10c, .cat10c svg { fill: #006374; }

.visible { display: inline; }
.hidden { display: none; }
.strong { font-weight: bold; }
.emph { font-style: italic; }
"""

type Model =
  { piece :: Piece
  , reduction :: Reduction
  , styles :: Styles
  }

printSurface :: Model -> Effect Unit
printSurface model = do
  log "⋊"
  for_ model.reduction.segments
    $ \seg -> do
        logShow seg.trans
        logShow seg.rslice

showReduction :: Reduction -> String
showReduction { segments } = intercalate "\n" $ showSegment 0 <$> segments
  where
  showChildren indent = case _ of
    Freeze -> "Freeze"
    Split { childl, childr } ->
      "Split\n"
        <> showSegment (indent + 1) childl
        <> "\n"
        <> showSegment' (indent + 1) childr
    Hori { childl, childm, childr } ->
      "Hori\n"
        <> showSegment (indent + 1) childl
        <> "\n"
        <> showSegment (indent + 1) childm
        <> "\n"
        <> showSegment' (indent + 1) childr

  showSegment' indent { trans, op } =
    power " " indent
      <> "trans: "
      <> show trans
      <> ", op:"
      <> showChildren indent op

  showSegment indent { trans, rslice, op } =
    power " " indent
      <> "trans: "
      <> show trans
      <> ", rslice: "
      <> show rslice
      <> ", op:"
      <> showChildren indent op

-- loading a piece
-- ===============
--
startSlice :: Slice
startSlice = { id: SliceId 0, notes: Start, x: 0.0, parents: NoParents }

thawTrans :: Array Note -> Int -> Array { note :: Note, hold :: Boolean } -> Transition
thawTrans ties id slice =
  { id: TransId id
  , is2nd: false
  , edges:
      { regular: S.fromFoldable $ A.catMaybes $ map findSecond ties
      , passing: []
      }
  }
  where
  notes = map _.note slice

  findSecond fst = (\snd -> { left: Inner fst, right: Inner snd }) <$> find (\snd -> snd.pitch == fst.pitch) notes

thawSlice :: Array { note :: Note, hold :: Boolean } -> Int -> Slice
thawSlice slice id =
  { id: SliceId id
  , notes: Inner $ map (\n -> { note: n.note, expl: NoExpl }) $ sortNotes slice
  , x: toNumber id
  , parents: NoParents
  }

thawPiece :: Piece -> Reduction
thawPiece piece =
  { start: startSlice
  , segments: L.fromFoldable $ segs
  , nextSliceId: SliceId $ imax + 2
  , nextTransId: TransId $ imax + 1
  }
  where
  thaw st { notes } = { seg, ties, i: st.i + 1 }
    where
    seg =
      Just
        { trans: thawTrans st.ties st.i notes
        , rslice: thawSlice notes (st.i + 1)
        , op: Freeze
        }

    ties = map _.note $ A.filter _.hold notes

  init = { seg: Nothing, ties: [], i: 0 }

  states = scanl thaw init piece

  imax = maybe 0 _.i $ (A.last states)

  lastSeg :: Segment
  lastSeg =
    { trans: thawTrans [] imax []
    , rslice:
        { id: SliceId (imax + 1)
        , notes: Stop
        , x: toNumber (imax + 1)
        , parents: NoParents
        }
    , op: Freeze
    }

  segs = A.catMaybes (map _.seg states) <> [ lastSeg ]

loadPiece :: Piece -> Model
loadPiece piece = { piece, reduction: thawPiece piece, styles: emptyStyles }

surfaceToModel :: Model -> Model
surfaceToModel model = { piece, reduction: model.reduction { segments = segs }, styles: model.styles }
  where
  toSlice seg =
    { time: Left ""
    , notes: (\n -> { note: n.note, hold: false }) <$> getInnerNotes seg.rslice
    }

  convertSegment i seg = seg { op = Freeze, rslice = seg.rslice { x = toNumber i + 1.0 } }

  piece = A.fromFoldable $ toSlice <$> L.dropEnd 1 model.reduction.segments

  segs = mapWithIndex convertSegment model.reduction.segments

-- outer structure operations
-- ==========================
--
-- | A helper function that traverses the surface of a reduction (a list of segments)
-- and applies an operation 'f' (which might fail)
-- to segments starting with slice 'sliceId'.
doAt :: forall a. (List Segment -> Maybe (Tuple a (List Segment))) -> (Slice -> a -> Either String (List Segment)) -> Reduction -> Either String (List Segment)
doAt match f red = go red.start red.segments
  where
  go :: Slice -> List Segment -> Either String (List Segment)
  go _ Nil = Left "not found"

  go leftSlice segs@(Cons seg rest) = case match segs of
    Just (Tuple m remainder) -> (\result -> result <> remainder) <$> f leftSlice m
    Nothing -> Cons seg <$> go seg.rslice rest

setParents :: Parents Slice -> Segment -> Segment
setParents p seg = case p of
  NoParents -> seg { rslice { parents = NoParents } }
  VertParent slice ->
    seg
      { rslice
          { parents = VertParent slice.id
          , notes = map (findUniqueVertExpl slice) <$> seg.rslice.notes
          }
      }
  MergeParents { left, right } ->
    seg
      { rslice
          { parents = MergeParents { left: left.id, right: right.id }
          , notes = map (findUniqueMergeExpl left right) <$> seg.rslice.notes
          }
      }
  where
  findUniqueVertExpl parentSlice child = case filter (\pnote -> pnote.note.pitch == child.note.pitch) $ getInnerNotes parentSlice of
    [ parentNote ] -> child { expl = HoriExpl parentNote.note }
    _ -> child

  findUniqueMergeExpl :: Slice -> Slice -> { note :: Note, expl :: NoteExplanation } -> { note :: Note, expl :: NoteExplanation }
  findUniqueMergeExpl leftSlice rightSlice child
    | leftSlice.notes == Start
    , rightSlice.notes == Stop = child { expl = RootExpl }
    | Inner lnotes <- leftSlice.notes
    , Inner rnotes <- rightSlice.notes =
        let
          rightExpls =
            A.catMaybes do
              { note: ln } <- lnotes
              pure do
                orn <- findRightOrn child.note.pitch ln
                pure $ RightExpl { orn: Just orn, leftParent: ln }

          leftExpls =
            A.catMaybes do
              { note: rn } <- rnotes
              pure do
                orn <- findLeftOrn child.note.pitch rn
                pure $ LeftExpl { orn: Just orn, rightParent: rn }

          doubleExpls =
            A.catMaybes do
              { note: ln } <- lnotes
              { note: rn } <- rnotes
              pure do
                orn <- findDoubleOrn child.note.pitch ln rn
                pure $ DoubleExpl { orn: Just orn, leftParent: ln, rightParent: rn }
        in
          case doubleExpls of
            [ uniqueDoubleExpl ] -> child { expl = uniqueDoubleExpl }
            _ -> case leftExpls <> rightExpls of
              [ uniqueSingleExpl ] -> child { expl = uniqueSingleExpl }
              _ -> child
    | otherwise = child

-- | Replace the edges in the left child of a hori with the corresponding left parent edges.
-- The mapping is obtained from the notes in the left child slice.
vertEdgesLeft :: Edges -> Slice -> Either String Edges
vertEdgesLeft edges slice
  | Inner notes <- slice.notes =
      Right
        $
          { regular: S.catMaybes $ S.map replaceRight edges.regular
          , passing: A.catMaybes $ map replaceRight edges.passing
          }
      where
      replaceRight { left, right }
        | Inner rightNote <- right
        , Just sliceNote <- A.find (\n -> n.note.id == rightNote.id) notes
        , HoriExpl parent <- sliceNote.expl = Just { left, right: Inner parent }
        | otherwise = Nothing
  | otherwise = Left "The current reduction is invalid: Trying to vert a Start or Stop slice."

-- | Replace the edges in the right child of a hori with the corresponding right parent edges.
-- The mapping is obtained from the notes in the right child slice.
vertEdgesRight :: Edges -> Slice -> Either String Edges
vertEdgesRight edges slice
  | Inner notes <- slice.notes =
      Right
        $
          { regular: S.catMaybes $ S.map replaceLeft edges.regular
          , passing: A.catMaybes $ map replaceLeft edges.passing
          }
      where
      replaceLeft { left, right }
        | Inner leftNote <- left
        , Just sliceNote <- A.find (\n -> n.note.id == leftNote.id) notes
        , HoriExpl parent <- sliceNote.expl = Just { left: Inner parent, right }
        | otherwise = Nothing
  | otherwise = Left "The current reduction is invalid: Trying to vert a Start or Stop slice."

-- | Replace the edges in the left parent of a hori with the corresponding left child edges.
-- The mapping is obtained from the notes in the left child slice.
horiEdgesLeft :: Edges -> Slice -> Edges
horiEdgesLeft edgesl slicel
  | Inner notes <- slicel.notes =
      { regular: S.catMaybes $ S.map replaceRight edgesl.regular
      , passing: A.catMaybes $ map replaceRight edgesl.passing
      }
      where
      replaceRight { left, right }
        | Inner rightNote <- right
        , Just sliceNote <-
            A.find
              ( \n -> case n.expl of
                  HoriExpl parent -> parent.id == rightNote.id
                  _ -> false
              )
              notes = Just { left, right: Inner sliceNote.note }
        | otherwise = Nothing
  | otherwise = edgesl

-- | Replace the edges in the right parent of a hori with the corresponding right child edges.
-- The mapping is obtained from the notes in the left child slice.
horiEdgesRight :: Slice -> Edges -> Edges
horiEdgesRight slicer edgesr
  | Inner notes <- slicer.notes =
      { regular: S.catMaybes $ S.map replaceLeft edgesr.regular
      , passing: A.catMaybes $ map replaceLeft edgesr.passing
      }
      where
      replaceLeft { left, right }
        | Inner leftNote <- left
        , Just sliceNote <-
            A.find
              ( \n -> case n.expl of
                  HoriExpl parent -> parent.id == leftNote.id
                  _ -> false
              )
              notes = Just { left: Inner sliceNote.note, right }
        | otherwise = Nothing
  | otherwise = edgesr

-- | Add all middle edges at a horizontalization between notes that are distributed to both sides
horiEdgesMid :: Slice -> Edges -> Slice -> Edges
horiEdgesMid slicel edges slicer = edges <> emptyEdges { regular = toBoth }
  where
  toLeft =
    foldl
      ( \acc left -> case left.expl of
          HoriExpl parent -> M.insert parent.id left.note acc
          _ -> acc
      )
      M.empty
      $ getInnerNotes slicel

  toBoth =
    foldl
      ( \acc right -> case right.expl of
          HoriExpl parent -> case M.lookup parent.id toLeft of
            Just left -> S.insert { left: Inner left, right: Inner right.note } acc
            Nothing -> acc
          _ -> acc
      )
      S.empty
      $ getInnerNotes slicer

-- | Merges two segment into a new segment with a 'Split' operation.
mkMerge :: TransId -> Slice -> Segment -> Segment -> Segment
mkMerge tid leftSlice seg1 seg2 =
  { trans: { id: tid, edges: parentEdges seg1'.rslice, is2nd: seg1.trans.is2nd }
  , rslice: seg2.rslice
  , op
  }
  where
  seg1' = setParents pars seg1

  pars = MergeParents { left: leftSlice, right: seg2.rslice }

  op = Split { childl: seg1', childr: detachSegment seg2 }

-- | Applies a merge at slice 'sliceId', if it exists on the reduction surface.
mergeAtSlice :: SliceId -> Model -> Either String Model
mergeAtSlice sliceId model = case doAt matchSlice tryMerge model.reduction of
  Right segments' -> Right model { reduction { segments = segments', nextTransId = incT nextTId } }
  Left err -> Left err
  where
  nextTId = model.reduction.nextTransId

  matchSlice :: List Segment -> Maybe (Tuple { seg1 :: Segment, seg2 :: Segment } (List Segment))
  matchSlice = case _ of
    Cons seg1 (Cons seg2 rest) -> if seg1.rslice.id == sliceId then Just (Tuple { seg1, seg2 } rest) else Nothing
    _ -> Nothing

  tryMerge :: Slice -> { seg1 :: Segment, seg2 :: Segment } -> Either String (List Segment)
  tryMerge leftSlice { seg1, seg2 } = Right $ mkMerge nextTId leftSlice seg1 seg2 : Nil

-- | Verticalizes three segments into two new segments,
-- the first of witch gets a 'Hori' operation.
mkVert :: TransId -> SliceId -> Segment -> Segment -> Segment -> Either String { mkLeftParent :: EndSegment -> Either String EndSegment, rightParent :: Segment, topSlice :: Slice }
mkVert tid sid l@{ rslice: { notes: Inner notesl } } m@{ rslice: { notes: Inner notesr } } r
  | not $ all isRepeatingEdge m.trans.edges.regular = Left "Middle transition of a vert must only contain repetition and passing edges!"
  | otherwise =
      do
        rightEdges <- vertEdgesRight r.trans.edges childm.rslice
        let
          rightParent = { trans: { id: incT tid, is2nd: true, edges: rightEdges }, rslice: r.rslice, op: Freeze }
        pure { mkLeftParent, rightParent, topSlice }
      where
      countPitches notes = foldl (\counts n -> M.insertWith (+) n.note.pitch 1 counts) M.empty notes

      newNotes counts = A.mapWithIndex mkNote pitches
        where
        mkNote i p = { note: { pitch: p, id: "note" <> show sid <> "." <> show i }, expl: NoExpl }

        pitches = A.reverse $ A.concatMap (\(Tuple p c) -> A.replicate c p) $ M.toUnfoldable counts

      topSlice =
        { notes: Inner $ newNotes $ M.unionWith max (countPitches notesl) (countPitches notesr)
        , id: sid
        , x: (l.rslice.x + m.rslice.x) / 2.0
        , parents: NoParents
        }

      childm = setParents (VertParent topSlice) m

      mkLeftParent lfound = do
        let
          childl = setParents (VertParent topSlice) (attachSegment lfound l.rslice)
        leftEdges <- vertEdgesLeft lfound.trans.edges childl.rslice
        pure
          { trans: { id: tid, is2nd: false, edges: leftEdges }
          , op: Hori { childl, childm, childr: detachSegment r }
          }

mkVert _ _ _ _ _ = Left "Cannot vert an outer slice!"

-- | Applies a verticalization at transition 'transId', if it exists on the reduction surface.
vertAtMid :: TransId -> Model -> Either String Model
vertAtMid transId model = case doVert model.reduction.start model.reduction.segments of
  Right { seg', tail', dangling } -> case dangling of
    Nothing ->
      Right
        model
          { reduction
              { segments = Cons seg' tail'
              , nextTransId = incT $ incT nextTId
              , nextSliceId = incS nextSId
              }
          }
    Just _dangling -> Left "Failed to insert hori!"
  Left err -> Left err
  where
  nextTId = model.reduction.nextTransId

  nextSId = model.reduction.nextSliceId

  -- Walk through the top-level segments and try to find the matching ID.
  -- Create the hori and try to insert is,
  -- alternatively passing it to the front if it can't be inserted directly
  doVert
    :: Slice
    -> List Segment
    -> Either String { seg' :: Segment, tail' :: List Segment, dangling :: Maybe { dist :: Int, mkLeftParent :: EndSegment -> Either String EndSegment, topSlice :: Slice } }
  doVert leftSlice (Cons l tail@(Cons m (Cons r rest)))
    | m.trans.id == transId = do
        { mkLeftParent, rightParent, topSlice } <- mkVert nextTId nextSId l m r
        tryInsert 0 topSlice mkLeftParent leftSlice (l { rslice = topSlice }) rightParent rest
    | otherwise = do
        { seg': m', tail': tailm', dangling } <- doVert l.rslice tail
        case dangling of
          Nothing -> Right { seg': l, tail': Cons m' tailm', dangling: Nothing }
          Just { dist, mkLeftParent, topSlice } -> tryInsert dist topSlice mkLeftParent leftSlice l m' tailm'

  doVert _ _ = Left $ "Cannot hori at " <> show transId

  -- Try to insert the operation starting from a given top-level segment.
  -- If the insertion is not completed (leftover == Just n), increase distance by n+1.
  tryInsert
    :: Int
    -> Slice
    -> (EndSegment -> Either String EndSegment)
    -> Slice
    -> Segment
    -> Segment
    -> List Segment
    -> Either String { seg' :: Segment, tail' :: List Segment, dangling :: Maybe { dist :: Int, mkLeftParent :: EndSegment -> Either String EndSegment, topSlice :: Slice } }
  tryInsert dist topSlice mkLeftParent leftSlice segl segr tail = do
    { segl', segr', tail', leftover } <-
      insertDangling dist topSlice mkLeftParent leftSlice segl segr tail
    pure case leftover of
      Nothing -> { seg': segl', tail': Cons segr' tail', dangling: Nothing }
      Just n -> { seg': segl', tail': Cons segr' tail', dangling: Just { dist: dist + 1 + n, mkLeftParent, topSlice } }

  -- Insert the operation as deep as possible into the graph, until a suitable place is found.
  -- If the place is not found, return the leftovers,
  -- i.e. the number of additional segments that need to be traversed
  -- to reach the point of failure from a previous top-level segment.
  insertDangling
    :: Int
    -> Slice
    -> (EndSegment -> Either String EndSegment)
    -> Slice
    -> Segment
    -> Segment
    -> List Segment
    -> Either String { segl' :: Segment, segr' :: Segment, tail' :: List Segment, leftover :: Maybe Int }
  insertDangling dist topSlice mkLeftParent leftSlice segl segr tail
    -- can be inserted here: return update segment (no leftovers)
    | dist == 0, not segl.trans.is2nd = do
        segl' <- mkLeftParent (detachSegment segl)
        pure { segl': attachSegment segl' segl.rslice, segr': segr, tail': tail, leftover: Nothing }
    -- cannot be inserted here: descend and return updated segment + leftovers
    | otherwise =
        case segl.op of
          -- freeze: cannot descend -> introduce leftovers
          Freeze -> Right { segl': segl, segr': segr, tail': tail, leftover: Just 0 }
          -- split: first try to descend on childr, then on childl; pass on remaining leftovers
          Split { childl, childr } -> do
            -- try inserting under right child
            { segl': childr', segr', tail', leftover: lor } <-
              insertDangling dist topSlice mkLeftParent childl.rslice (attachSegment childr segl.rslice) segr tail
            -- fix the parent of childl after inserting under childr
            childlFixed <-
              if dist == 0 then case childl.rslice.parents of
                MergeParents _ -> Right $ setParents (MergeParents { left: leftSlice, right: topSlice }) $ resetExpls childl
                _ -> Left "invalid derivation structure: non-merge parents on merge slice"
              else
                pure childl
            case lor of -- check if inserting under right child was complete
              Nothing -> -- yes? return updated segments

                pure
                  $
                    { segl':
                        segl
                          { op = Split { childl: childlFixed, childr: detachSegment childr' }
                          , trans { edges = parentEdges childlFixed.rslice }
                          }
                    , segr'
                    , tail'
                    , leftover: Nothing
                    }
              Just nlor -> do -- no? try inserting under left child
                { segl': childl', segr': childr'', tail': ltail, leftover: lol } <-
                  insertDangling (dist + nlor + 1) topSlice mkLeftParent leftSlice childlFixed childr' (Cons segr' tail')
                case ltail of
                  Cons segr'' tail'' -> do
                    let
                      segl' =
                        segl
                          { op = Split { childl: childl', childr: detachSegment childr'' }
                          , trans { edges = parentEdges childl'.rslice }
                          }
                    pure $ { segl', segr': segr'', tail': tail'', leftover: (_ + 1) <$> lol }
                  _ -> Left "Returned tail too short (splitLeft). This is a bug!"
          -- hori: first try to descend on childr, then childm, then childl; pass on remaining leftovers
          Hori { childl, childm, childr } -> case segr.op of -- right split before hori?
            Hori _ -> Left "Not a leftmost derivation (Hori right of Hori)!"
            -- right split: recur using left child, then update it
            Split s -> do
              { segl', segr': splitChildl', tail': remTail, leftover } <-
                insertDangling dist topSlice mkLeftParent leftSlice segl s.childl (Cons (attachSegment s.childr segr.rslice) tail)
              case remTail of
                Cons splitChildr' tail' -> do
                  let
                    splitOp' = Split { childl: splitChildl', childr: detachSegment splitChildr' }
                  pure { segl', segr': segr { op = splitOp', trans { edges = parentEdges splitChildl'.rslice } }, tail', leftover }
                _ -> Left "Returned tail too short (splitRight). This is a bug!"
            -- no right split: process hori itself
            Freeze -> case tail of
              Nil -> Left "Tail too short (hori). This is a bug!"
              Cons nextsegr rtail -> do
                -- try inserting under right child
                { segl': childr'r, segr': nextsegr', tail': rtail', leftover: lor } <-
                  insertDangling (dist - 1) topSlice mkLeftParent childm.rslice (attachSegment childr segl.rslice) nextsegr rtail
                let
                  tail'r = Cons nextsegr' rtail'
                case lor of -- insertion under right child complete?
                  Nothing -> do
                    let
                      op' = Hori { childl, childm, childr: detachSegment childr'r }
                    segr'r <- fixRight childm childr'r
                    pure { segl': segl { op = op' }, segr': segr'r, tail': tail'r, leftover: Nothing }
                  Just nlor -> do
                    -- try inserting under middle child
                    { segl': childm'm, segr': childr'm, tail': tail'm, leftover: lom } <-
                      insertDangling (dist + nlor) topSlice mkLeftParent childl.rslice childm childr'r tail'r
                    case lom of -- insertion under middle child complete?
                      Nothing -> do
                        let
                          op' = Hori { childl, childm: childm'm, childr: detachSegment childr'm }
                        segr'm <- fixRight childm'm childr'm
                        pure { segl': segl { op = op' }, segr': segr'm, tail': tail'm, leftover: Nothing }
                      Just nlom -> do
                        -- try inserting under left child
                        { segl': childl'l, segr': childm'l, tail': tailAndChildr'l, leftover: lol } <-
                          insertDangling (dist + 1 + nlom) topSlice mkLeftParent leftSlice childl childm'm (Cons childr'm tail'm)
                        case tailAndChildr'l of
                          Nil -> Left "Returned tail too short (hori). This is a bug!"
                          Cons childr'l tail'l -> do
                            let
                              op' = Hori { childl: childl'l, childm: childm'l, childr: detachSegment childr'l }
                            segr'l <- fixRight childm'l childr'l
                            pure { segl': segl { op = op' }, segr': segr'l, tail': tail'l, leftover: (_ + 2) <$> lol }
        where
        fixRight childm childr = do
          edges' <- vertEdgesRight childr.trans.edges childm.rslice
          pure segr { trans { edges = edges' } }

-- | Undoes a merge at transition 'transId', if it exists, restoring its children.
undoMergeAtTrans :: TransId -> Model -> Either String Model
undoMergeAtTrans transId model = case doAt matchTrans tryUnmerge model.reduction of
  Right segments' -> Right model { reduction { segments = segments' } }
  Left err -> Left err
  where
  matchTrans = case _ of
    Cons seg rest -> if seg.trans.id == transId then Just (Tuple seg rest) else Nothing
    _ -> Nothing

  tryUnmerge _ seg = case seg.op of
    Split { childl, childr } ->
      Right
        $ resetExpls (setParents NoParents childl)
            : attachSegment childr seg.rslice
            : Nil
    _ -> Left "Operation is not a split!"

-- | Undoes a verticalization at slice 'sliceId', if it exists, restoring its children.
undoVertAtSlice :: SliceId -> Model -> Either String Model
undoVertAtSlice sliceId model = do
  { tail: segments', result } <- extractVert model.reduction.segments
  case result of
    Right children -> do
      segments'' <- doAt matchSlice (tryReInsert children) $ model.reduction { segments = segments' }
      pure $ model { reduction { segments = segments'' } }
    Left _ -> Left "Could not find left parent edge. This is a bug!"
  where
  extractVert = case _ of
    Nil -> Left "Could not find hori!"
    Cons _ Nil -> Left "Could not find hori!"
    Cons pl (Cons pr rest) ->
      if pl.rslice.id == sliceId then
        if pr.op == Freeze then case pl.op of
          Hori { childl, childr, childm } ->
            Right
              { tail: childl { rslice = pl.rslice } : pr : rest
              , result:
                  Right
                    { slicel: childl.rslice
                    , childm
                    , childr
                    , pl': childl.trans.edges
                    }
              , surfaceCount: 0
              }
          Freeze ->
            Right
              { tail: pl : pr : rest
              , result: Left 1
              , surfaceCount: 1
              }
          -- TODO: maybe allow split too?
          _ -> Left "Not a top-level hori!"
        else
          Left "Not a top-level hori!"
      else do
        next <- extractVert (Cons pr rest)
        case next.result of
          Right _res -> pure $ { tail: Cons pl next.tail, result: next.result, surfaceCount: 0 }
          Left count -> do
            pl'Maybe <- tryExtract count (detachSegment pl)
            case pl'Maybe of
              Left count' -> pure { tail: pl : next.tail, result: Left (count' + 1), surfaceCount: next.surfaceCount + 1 }
              Right { seg', result } -> do
                fixedTail <- fixPl next.surfaceCount result.pl' $ attachSegment seg' pl.rslice : next.tail
                pure { tail: fixedTail, result: Right result, surfaceCount: 0 }

  tryExtract
    :: Int
    -> EndSegment
    -> Either String (Either Int { seg' :: EndSegment, result :: { slicel :: Slice, childm :: Segment, childr :: EndSegment, pl' :: Edges } })
  tryExtract count seg
    | count <= 0 = case seg.op of
        Hori { childl, childm, childr } ->
          pure $ Right
            $
              { seg': detachSegment childl
              , result: { slicel: childl.rslice, childm, childr, pl': childl.trans.edges }
              }
        Freeze -> pure $ Left count
        _ -> Left "Could not find left parent edge. This is a bug!"
    | otherwise = case seg.op of
        Freeze -> pure $ Left count
        Split { childl, childr } -> do
          -- check right child
          resR <- tryExtract count childr
          case resR of
            Right { seg', result } -> pure $ Right { seg': seg { op = Split { childl, childr: seg' } }, result }
            Left countR -> do
              -- check left child
              resL <- tryExtract (countR + 1) $ detachSegment childl
              case resL of
                Right { seg', result } -> pure $ Right { seg': seg { op = Split { childl: attachSegment seg' childl.rslice, childr } }, result }
                Left countL -> pure $ Left countL
        Hori { childl, childm, childr } -> do
          -- check right child
          resR <- tryExtract (count - 1) childr
          case resR of
            -- TODO: fix changed segments, if necessary!
            Right { seg', result } -> do
              pure $ Right { seg': seg { op = Hori { childl, childm, childr: seg' } }, result }
            Left countR -> do
              resM <- tryExtract (countR + 1) $ detachSegment childm
              case resM of
                Right { seg', result } -> pure $ Right { seg': seg { op = Hori { childl, childm: attachSegment seg' childm.rslice, childr } }, result }
                Left countM -> do
                  resL <- tryExtract (countM + 1) $ detachSegment childl
                  case resL of
                    Right { seg', result } -> pure $ Right { seg': seg { op = Hori { childl: attachSegment seg' childl.rslice, childm, childr } }, result }
                    Left countL -> pure $ Left countL

  fixPl :: Int -> Edges -> List Segment -> Either String (List Segment)
  fixPl count pl
    | count >= 0 = case _ of
        Nil -> pure Nil
        Cons seg tail -> do
          { seg', pl' } <- insertPl count pl $ detachSegment seg
          tail' <- fixPl (count - 1) pl' tail
          pure $ attachSegment seg' seg.rslice : tail'
    | otherwise = pure

  insertPl :: Int -> Edges -> EndSegment -> Either String { seg' :: EndSegment, pl' :: Edges }
  insertPl count pl seg
    | count <= 0 = case seg.op of
        Freeze -> Right { seg': seg { trans { edges = pl } }, pl': pl }
        _ -> Left "Invalid structure. This is a bug!"
    | otherwise = case seg.op of
        Freeze -> Right { seg': seg, pl': pl }
        Split { childl, childr } -> do
          resL <- insertPl (count + 1) pl $ detachSegment childl
          resR <- insertPl count resL.pl' childr
          pure { seg': seg { op = Split { childl: attachSegment resL.seg' childl.rslice, childr: resR.seg' } }, pl': resR.pl' }
        Hori { childl, childm, childr } -> do
          resL <- insertPl (count + 1) pl $ detachSegment childl
          resM <- insertPl count resL.pl' $ detachSegment childm
          if count == 1 then do
            pl' <- vertEdgesRight resM.pl' childm.rslice
            Right
              { seg':
                  seg
                    { op =
                        Hori
                          { childl: attachSegment resL.seg' childl.rslice
                          , childm: attachSegment resM.seg' childm.rslice
                          , childr: childr { trans { edges = resM.pl' } }
                          }
                    }
              , pl'
              }
          else do
            resR <- insertPl (count - 1) resM.pl' childr
            Right
              { seg':
                  seg
                    { op =
                        Hori
                          { childl: attachSegment resL.seg' childl.rslice
                          , childm: attachSegment resM.seg' childm.rslice
                          , childr: resR.seg'
                          }
                    }
              , pl': resR.pl'
              }

  matchSlice = case _ of
    Cons pl (Cons pr rest) -> if pl.rslice.id == sliceId then Just (Tuple { pl, pr } rest) else Nothing
    _ -> Nothing

  tryReInsert { slicel, childm, childr } _ { pl, pr } =
    Right
      $ resetExpls (setParents NoParents $ pl { rslice = slicel })
          : resetExpls (setParents NoParents childm)
          : attachSegment childr pr.rslice
          : Nil

noteSetExplanation :: String -> NoteExplanation -> Model -> Either String Model
noteSetExplanation noteId expl model = case traverseTop Nil model.reduction.segments of
  Right segments' -> Right model { reduction { segments = segments' } }
  Left err -> Left err
  where
  noteInSlice :: Slice -> Boolean
  noteInSlice slice = A.any (\n -> n.note.id == noteId) $ getInnerNotes slice

  setNoteExplInSlice :: Slice -> Slice
  setNoteExplInSlice slice = slice { notes = map (\n -> if n.note.id == noteId then n { expl = expl } else n) <$> slice.notes }

  traverseTop :: List Edges -> List Segment -> Either String (List Segment)
  traverseTop upFromLeft = case _ of
    Nil -> Right Nil
    Cons seg rest -> do
      { rUp, seg' } <- doSegment upFromLeft seg
      rest' <- traverseTop rUp rest
      pure $ Cons seg' rest'

  doSegment :: List Edges -> Segment -> Either String { seg' :: Segment, rUp :: List Edges }
  doSegment upFromLeft seg = _ { seg' { rslice = rslice' } } <$> doSegment' upFromLeft seg
    where
    rslice' = if noteInSlice seg.rslice then setNoteExplInSlice seg.rslice else seg.rslice

  doSegment' :: forall r. List Edges -> { trans :: Transition, op :: Op | r } -> Either String { seg' :: { trans :: Transition, op :: Op | r }, rUp :: List Edges }
  doSegment' upFromLeft seg = case seg.op of
    Freeze -> case upFromLeft of
      Nil -> Right $ { seg': seg, rUp: Nil }
      Cons edges rest -> Right $ { seg': seg { trans { edges = edges } }, rUp: rest }
    Split { childl, childr } -> do
      { seg': childl', rUp: rUpl } <- doSegment upFromLeft childl
      { seg': childr', rUp: rUpr } <- doSegment' rUpl childr
      let
        segNewOp = seg { op = Split { childl: childl', childr: childr' } }
      seg' <-
        if noteInSlice childl.rslice then
          if explIsSplit expl then
            pure segNewOp { trans { edges = parentEdges childl'.rslice } }
          else
            Left "Cannot explain a split note with a hori."
        else
          pure segNewOp
      pure $ { seg', rUp: rUpr }
    Hori { childl, childm, childr } -> do
      { seg': childl', rUp: rUpl } <- doSegment upFromLeft childl
      { seg': childm', rUp: rUpm } <- doSegment rUpl childm
      { seg': childr', rUp: rUpr } <- doSegment' rUpm childr
      leftParentEdges <- vertEdgesLeft childl'.trans.edges childl'.rslice
      rightParentEdges <- vertEdgesRight childr'.trans.edges childm'.rslice
      if (noteInSlice childl.rslice || noteInSlice childm.rslice) && not (explIsHori expl) then
        Left
          $ "Cannot explain a hori note with a split. noteId = "
              <> show noteId
              <> ", childl.rslice = "
              <> show childl.rslice
              <> ", childm.rslice = "
              <> show childm.rslice
      else
        pure unit
      pure
        $
          { seg':
              seg
                { op = Hori { childl: childl', childm: childm', childr: childr' }
                , trans { edges = leftParentEdges }
                }
          , rUp: Cons rightParentEdges rUpr
          }

-- Style Operations
-- ================

setNoteStyle :: String -> Style -> Model -> Model
setNoteStyle id style model = model { styles { notes = notes' } }
  where
  notes' = M.insert id style model.styles.notes

setEdgeStyle :: Tuple String String -> Style -> Model -> Model
setEdgeStyle ids style model = model { styles { edges = edges' } }
  where
  edges' = M.insert ids style model.styles.edges

setSliceStyle :: SliceId -> Style -> Model -> Model
setSliceStyle id style model = model { styles { slices = slices' } }
  where
  slices' = M.insert id style model.styles.slices

setTransStyle :: TransId -> Style -> Model -> Model
setTransStyle id style model = model { styles { transitions = transitions' } }
  where
  transitions' = M.insert id style model.styles.transitions
