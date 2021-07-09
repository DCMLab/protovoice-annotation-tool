module Model where

import Prelude
import Common (MBS)
import Data.Array as A
import Data.Either (Either(..))
import Data.Foldable (find, foldl, for_, intercalate)
import Data.Generic.Rep (class Generic)
import Data.Int (toNumber)
import Data.List (List(..), (:))
import Data.List as L
import Data.Map as M
import Data.Maybe (Maybe(..), maybe)
import Data.Monoid (power)
import Data.Show.Generic (genericShow)
import Data.Traversable (scanl)
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.Console (log, logShow)
import SimplePitch (SimplePitch)

-- types
-- -----
--
type Piece
  = Array { time :: MBS, notes :: Array { hold :: Boolean, note :: Note } }

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
  = RootNote
  | FullNeighbor
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
        _ -> emptyEdges { regular = [ parentEdge ] }
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

setLeftExplParent :: Note -> NoteExplanation -> Maybe NoteExplanation
setLeftExplParent leftParent = case _ of
  NoExpl -> Just $ RightExpl { orn: Nothing, leftParent }
  RightExpl { orn } -> Just $ RightExpl { orn, leftParent }
  LeftExpl { rightParent } -> Just $ DoubleExpl { orn: Nothing, leftParent, rightParent }
  DoubleExpl { orn, rightParent } -> Just $ DoubleExpl { orn, leftParent, rightParent }
  HoriExpl _ -> Nothing

setRightExplParent :: Note -> NoteExplanation -> Maybe NoteExplanation
setRightExplParent rightParent = case _ of
  NoExpl -> Just $ LeftExpl { orn: Nothing, rightParent }
  LeftExpl { orn } -> Just $ LeftExpl { orn, rightParent }
  RightExpl { leftParent } -> Just $ DoubleExpl { orn: Nothing, leftParent, rightParent }
  DoubleExpl { orn, leftParent } -> Just $ DoubleExpl { orn, leftParent, rightParent }
  HoriExpl _ -> Nothing

setHoriExplParent :: Note -> NoteExplanation -> Maybe NoteExplanation
setHoriExplParent parent = case _ of
  NoExpl -> Just $ HoriExpl parent
  HoriExpl _ -> Just $ HoriExpl parent
  _ -> Nothing

explHasParent :: String -> NoteExplanation -> Boolean
explHasParent id = case _ of
  NoExpl -> false
  HoriExpl n -> n.id == id
  RightExpl { leftParent } -> leftParent.id == id
  LeftExpl { rightParent } -> rightParent.id == id
  DoubleExpl { leftParent, rightParent } -> leftParent.id == id || rightParent.id == id

type Note
  = { pitch :: SimplePitch, id :: String }

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

type Edge
  = { left :: StartStop Note, right :: StartStop Note }

isRepeatingEdge :: Edge -> Boolean
isRepeatingEdge = case _ of
  { left: Inner left, right: Inner right } -> left.pitch == right.pitch
  _ -> false

type Notes
  = Array { note :: Note, expl :: NoteExplanation }

type Edges
  = { regular :: Array Edge
    , passing :: Array Edge
    }

emptyEdges :: Edges
emptyEdges = { regular: [], passing: [] }

newtype SliceId
  = SliceId Int

instance showSliceId :: Show SliceId where
  show (SliceId i) = "s" <> show i

derive instance eqSliceId :: Eq SliceId

derive instance ordSliceId :: Ord SliceId

incS :: SliceId -> SliceId
incS (SliceId i) = SliceId $ i + 1

newtype TransId
  = TransId Int

instance showTransId :: Show TransId where
  show (TransId i) = "t" <> show i

derive instance eqTransId :: Eq TransId

derive instance ordTransId :: Ord TransId

incT :: TransId -> TransId
incT (TransId i) = TransId $ i + 1

data Parents
  = NoParents
  | VertParent SliceId
  | MergeParents { left :: SliceId, right :: SliceId }

derive instance eqParents :: Eq Parents

derive instance genericParents :: Generic Parents _

instance showParents :: Show Parents where
  show p = genericShow p

getParents :: Parents -> Array SliceId
getParents = case _ of
  NoParents -> []
  VertParent p -> [ p ]
  MergeParents { left, right } -> [ left, right ]

type Slice
  = { id :: SliceId
    , notes :: StartStop Notes
    , x :: Number
    , parents :: Parents
    }

getInnerNotes :: Slice -> Notes
getInnerNotes slice = case slice.notes of
  Start -> []
  Stop -> []
  Inner notes -> notes

type Transition
  = { id :: TransId
    , edges :: Edges
    , is2nd :: Boolean
    }

emptyTrans :: TransId -> Boolean -> Transition
emptyTrans id is2nd = { id, is2nd, edges: emptyEdges }

transEdges :: Transition -> Array Edge
transEdges trans = trans.edges.regular <> trans.edges.passing

data Op
  = Freeze
  | Split { childl :: Segment, childr :: EndSegment }
  | Hori { childl :: Segment, childm :: Segment, childr :: EndSegment }

derive instance genericOp :: Generic Op _

instance showOp :: Show Op where
  show x = genericShow x

type Segment
  = { trans :: Transition
    , rslice :: Slice
    , op :: Op
    }

resetExpls :: Segment -> Segment
resetExpls seg = seg { rslice { notes = map (\note -> note { expl = NoExpl }) <$> seg.rslice.notes } }

type EndSegment
  = { trans :: Transition
    , op :: Op
    }

attachSegment :: EndSegment -> Slice -> Segment
attachSegment { trans, op } rslice = { trans, op, rslice }

detachSegment :: Segment -> EndSegment
detachSegment { trans, op } = { trans, op }

type Reduction
  = { start :: Slice
    , segments :: List Segment
    , nextTransId :: TransId
    , nextSliceId :: SliceId
    }

type Model
  = { piece :: Piece
    , reduction :: Reduction
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
      { regular: A.catMaybes $ map findSecond ties
      , passing: []
      }
  }
  where
  notes = map _.note slice

  findSecond fst = (\snd -> { left: Inner fst, right: Inner snd }) <$> find (\snd -> snd.pitch == fst.pitch) notes

thawSlice :: Array { note :: Note, hold :: Boolean } -> Int -> Slice
thawSlice slice id =
  { id: SliceId id
  , notes: Inner $ map (\n -> { note: n.note, expl: NoExpl }) slice
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
loadPiece piece = { piece, reduction: thawPiece piece }

-- outer structure operations
-- ==========================
--
-- | A helper function that traverses the surface of a reduction (a list of segments)
-- and applies an operation 'f' (which might fail)
-- to segments starting with slice 'sliceId'.
doAt :: forall a. (List Segment -> Maybe (Tuple a (List Segment))) -> (SliceId -> a -> Either String (List Segment)) -> Reduction -> Either String (List Segment)
doAt match f red = go red.start.id red.segments
  where
  go :: SliceId -> List Segment -> Either String (List Segment)
  go _ Nil = Left "not found"

  go leftId segs@(Cons seg rest) = case match segs of
    Just (Tuple m remainder) -> (\result -> result <> remainder) <$> f leftId m
    Nothing -> Cons seg <$> go seg.rslice.id rest

setParents :: Parents -> Segment -> Segment
setParents p seg = seg { rslice { parents = p } }

-- | Merges two segment into a new segment with a 'Split' operation.
mkMerge :: TransId -> SliceId -> Segment -> Segment -> Segment
mkMerge tid leftId seg1 seg2 =
  { trans: emptyTrans tid seg1.trans.is2nd
  , rslice: seg2.rslice
  , op
  }
  where
  pars = MergeParents { left: leftId, right: seg2.rslice.id }

  op = Split { childl: setParents pars seg1, childr: detachSegment seg2 }

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

  tryMerge :: SliceId -> { seg1 :: Segment, seg2 :: Segment } -> Either String (List Segment)
  tryMerge leftId { seg1, seg2 } = Right $ mkMerge nextTId leftId seg1 seg2 : Nil

-- | Verticalizes three segments into two new segments,
-- the first of witch gets a 'Hori' operation.
mkVert :: TransId -> SliceId -> Segment -> Segment -> Segment -> Either String (List Segment)
mkVert tid sid l@{ rslice: { notes: Inner notesl } } m@{ rslice: { notes: Inner notesr } } r
  | l.trans.is2nd = Left "Cannot vert right of a vert (not a leftmost derivation)!"
  | not $ A.all isRepeatingEdge m.trans.edges.regular = Left "Middle transition of a vert must only contain repetition and passing edges!"
  | otherwise = Right $ pleft : pright : Nil
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

    pleft =
      { trans: emptyTrans tid false
      , rslice: topSlice
      , op:
          Hori
            { childl: setParents (VertParent sid) l
            , childm: setParents (VertParent sid) m
            , childr: detachSegment r
            }
      }

    pright = { trans: emptyTrans (incT tid) true, rslice: r.rslice, op: Freeze }

mkVert _ _ _ _ _ = Left "Cannot vert an outer slice!"

-- | Applies a verticalization at transition 'transId', if it exists on the reduction surface.
vertAtMid :: TransId -> Model -> Either String Model
vertAtMid transId model = case doAt matchMid tryVert model.reduction of
  Right segments' ->
    Right
      model
        { reduction
          { segments = segments'
          , nextTransId = incT $ incT nextTId
          , nextSliceId = incS nextSId
          }
        }
  Left err -> Left err
  where
  nextTId = model.reduction.nextTransId

  nextSId = model.reduction.nextSliceId

  matchMid = case _ of
    Cons l (Cons m (Cons r rest)) -> if m.trans.id == transId then Just (Tuple { l, m, r } rest) else Nothing
    _ -> Nothing

  tryVert _ { l, m, r } = mkVert nextTId nextSId l m r

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
undoVertAtSlice sliceId model = case doAt matchSlice tryUnvert model.reduction of
  Right segments' -> Right model { reduction { segments = segments' } }
  Left err -> Left err
  where
  matchSlice = case _ of
    Cons pl (Cons pr rest) -> if pl.rslice.id == sliceId then Just (Tuple { pl, pr } rest) else Nothing
    _ -> Nothing

  tryUnvert _ { pl, pr } = case pl.op of
    Hori { childl, childm, childr } ->
      Right
        $ resetExpls (setParents NoParents childl)
        : resetExpls (setParents NoParents childm)
        : attachSegment childr pr.rslice
        : Nil
    _ -> Left "Operation is not a hori!"

noteSetExplanation :: String -> NoteExplanation -> Model -> Either String Model
noteSetExplanation noteId expl model = case traverseTop Nil model.reduction.segments of
  Right segments' -> Right model { reduction { segments = segments' } }
  Left err -> Left err
  where
  noteInSlice :: Slice -> Boolean
  noteInSlice slice = A.any (\n -> n.note.id == noteId) $ getInnerNotes slice

  setNoteExplInSlice :: Slice -> Slice
  setNoteExplInSlice slice = slice { notes = map (\n -> if n.note.id == noteId then n { expl = expl } else n) <$> slice.notes }

  parentEdges :: Slice -> Edges
  parentEdges = case _ of
    { notes: Inner notes } -> foldl collectParents { regular: [], passing: [] } (_.expl <$> notes)
    _ -> emptyEdges
    where
    collectParents acc e = acc <> explParentEdge e

  notFound = Left $ "Note " <> noteId <> " not found"

  vertEdgesLeft :: Edges -> Slice -> Either String Edges
  vertEdgesLeft edges slice
    | Inner notes <- slice.notes =
      Right
        $ { regular: A.concatMap replaceRight edges.regular
          , passing: A.concatMap replaceRight edges.passing
          }
      where
      replaceRight { left, right }
        | Inner rightNote <- right
        , Just sliceNote <- A.find (\n -> n.note.id == rightNote.id) notes
        , HoriExpl parent <- sliceNote.expl = [ { left, right: Inner parent } ]
        | otherwise = []
    | otherwise = Left "The current reduction is invalid: Trying to vert a Start or Stop slice."

  vertEdgesRight :: Edges -> Slice -> Either String Edges
  vertEdgesRight edges slice
    | Inner notes <- slice.notes =
      Right
        $ { regular: A.concatMap replaceLeft edges.regular
          , passing: A.concatMap replaceLeft edges.passing
          }
      where
      replaceLeft { left, right }
        | Inner leftNote <- left
        , Just sliceNote <- A.find (\n -> n.note.id == leftNote.id) notes
        , HoriExpl parent <- sliceNote.expl = [ { left: Inner parent, right } ]
        | otherwise = []
    | otherwise = Left "The current reduction is invalid: Trying to vert a Start or Stop slice."

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
        $ { seg':
              seg
                { op = Hori { childl: childl', childm: childm', childr: childr' }
                , trans { edges = leftParentEdges }
                }
          , rUp: Cons rightParentEdges rUpr
          }
