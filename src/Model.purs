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
import Data.Pitches (class Interval, Pitch, SPitch, direction, ic, isStep, pc, pto)
import Data.Show.Generic (genericShow)
import Data.Traversable (scanl)
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.Console (log, logShow)

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
        _ -> emptyEdges { regular = [ parentEdge ] }
  RootExpl -> emptyEdges { regular = [ { left: Start, right: Stop } ] }
  _ -> emptyEdges

parentEdges :: Slice -> Edges
parentEdges = case _ of
  { notes: Inner notes } -> foldl collectParents { regular: [], passing: [] } (_.expl <$> notes)
  _ -> emptyEdges
  where
  collectParents acc e = acc <> explParentEdge e

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
  | pc left == pc right, isStep (ic (child `pto` left)) = Just FullNeighbor
  | pbetween (pc left) (pc child) (pc right) =
    if isStep (ic (child `pto` left)) then
      if isStep (ic (child `pto` right)) then Just PassingMid else Just PassingLeft
    else if isStep (ic (child `pto` right)) then Just PassingRight else Nothing
  | otherwise = Nothing

setLeftExplParent :: SPitch -> Maybe Note -> NoteExplanation -> Maybe NoteExplanation
setLeftExplParent child leftParentMaybe expl = case leftParentMaybe of
  Just leftParent -> case expl of
    NoExpl -> Just $ RightExpl { orn: findRightOrn child leftParent, leftParent }
    RightExpl { orn } -> Just $ RightExpl { orn: findRightOrn child leftParent, leftParent }
    LeftExpl { rightParent } ->
      Just
        $ DoubleExpl
            { orn: findDoubleOrn child leftParent rightParent
            , leftParent
            , rightParent
            }
    DoubleExpl { orn, rightParent } ->
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
    LeftExpl { orn } -> Just $ LeftExpl { orn: findLeftOrn child rightParent, rightParent }
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
    DoubleExpl { orn, leftParent } -> Just $ RightExpl { orn: findRightOrn child leftParent, leftParent }
    HoriExpl _ -> Nothing
    _ -> Just NoExpl

setHoriExplParent :: SPitch -> Maybe Note -> NoteExplanation -> Maybe NoteExplanation
setHoriExplParent child parentMaybe expl = case parentMaybe of
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

type Note
  = { pitch :: SPitch, id :: String }

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

type Slice
  = { id :: SliceId
    , notes :: StartStop Notes
    , x :: Number
    , parents :: Parents SliceId
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

derive instance eqOp :: Eq Op

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
doAt :: forall a. (List Segment -> Maybe (Tuple a (List Segment))) -> (Slice -> a -> Either String (List Segment)) -> Reduction -> Either String (List Segment)
doAt match f red = go red.start red.segments
  where
  go :: Slice -> List Segment -> Either String (List Segment)
  go _ Nil = Left "not found"

  go leftSlice segs@(Cons seg rest) = case match segs of
    Just (Tuple m remainder) -> (\result -> result <> remainder) <$> f leftSlice m
    Nothing -> Cons seg <$> go seg.rslice rest

-- TODO: automatically add unique explanations
setParents :: Parents Slice -> Segment -> Segment
setParents p seg = case p of
  NoParents -> seg { rslice { parents = NoParents } }
  VertParent slice -> seg { rslice { parents = VertParent slice.id } }
  MergeParents { left, right } ->
    let
      notes' =
        if left.notes == Start && right.notes == Stop then
          map (_ { expl = RootExpl }) <$> seg.rslice.notes
        else
          seg.rslice.notes
    in
      seg
        { rslice
          { parents = MergeParents { left: left.id, right: right.id }
          , notes = notes'
          }
        }

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
mkVert :: TransId -> SliceId -> Segment -> Segment -> Segment -> Either String { mkLeftParent :: EndSegment -> EndSegment, rightParent :: Segment, topSlice :: Slice }
mkVert tid sid l@{ rslice: { notes: Inner notesl } } m@{ rslice: { notes: Inner notesr } } r
  | not $ A.all isRepeatingEdge m.trans.edges.regular = Left "Middle transition of a vert must only contain repetition and passing edges!"
  | otherwise = Right { mkLeftParent, rightParent, topSlice }
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

    mkLeftParent lchild =
      { trans: emptyTrans tid false
      , op:
          Hori
            { childl: setParents (VertParent topSlice) (attachSegment lchild l.rslice)
            , childm: setParents (VertParent topSlice) m
            , childr: detachSegment r
            }
      }

    rightParent = { trans: emptyTrans (incT tid) true, rslice: r.rslice, op: Freeze }

mkVert _ _ _ _ _ = Left "Cannot vert an outer slice!"

-- | Applies a verticalization at transition 'transId', if it exists on the reduction surface.
vertAtMid :: TransId -> Model -> Either String Model
vertAtMid transId model = case doVert model.reduction.start model.reduction.segments of
  Right { tail': segments', dangling } -> case dangling of
    Nothing ->
      Right
        model
          { reduction
            { segments = segments'
            , nextTransId = incT $ incT nextTId
            , nextSliceId = incS nextSId
            }
          }
    Just dang -> Left "Failed to insert hori!"
  Left err -> Left err
  where
  nextTId = model.reduction.nextTransId

  nextSId = model.reduction.nextSliceId

  -- Walk through the top-level segments and try to find the matching ID.
  -- Create the hori and try to insert is,
  -- alternatively passing it to the front if it can't be inserted directly
  doVert ::
    Slice ->
    List Segment ->
    Either String { tail' :: List Segment, dangling :: Maybe { dist :: Int, mkLeftParent :: EndSegment -> EndSegment, topSlice :: Slice } }
  doVert leftSlice (Cons l tail@(Cons m (Cons r rest)))
    | m.trans.id == transId = do
      { mkLeftParent, rightParent, topSlice } <- mkVert nextTId nextSId l m r
      tryInsert 0 topSlice mkLeftParent leftSlice (l { rslice = topSlice }) $ Cons rightParent rest
    | otherwise = do
      { tail', dangling } <- doVert l.rslice tail
      case dangling of
        Nothing -> Right { tail': Cons l tail', dangling: Nothing }
        Just { dist, mkLeftParent, topSlice } -> tryInsert dist topSlice mkLeftParent leftSlice l tail'

  doVert _ _ = Left $ "Cannot hori at " <> show transId

  -- Try to insert the operation starting from a given top-level segment.
  -- If the insertion is not completed (leftover == Just n), increase distance by n+1.
  tryInsert ::
    Int ->
    Slice ->
    (EndSegment -> EndSegment) ->
    Slice ->
    Segment ->
    List Segment ->
    Either String { tail' :: List Segment, dangling :: Maybe { dist :: Int, mkLeftParent :: EndSegment -> EndSegment, topSlice :: Slice } }
  tryInsert dist topSlice mkLeftParent leftSlice l t' = do
    { seg': l', leftover } <- insertDangling dist topSlice mkLeftParent leftSlice (detachSegment l)
    let
      tail' = Cons (attachSegment l' l.rslice) t'
    pure case leftover of
      Nothing -> { tail', dangling: Nothing }
      Just n -> { tail', dangling: Just { dist: dist + 1 + n, mkLeftParent, topSlice } }

  -- Insert the operation as deep as possible into the graph, until a suitable place is found.
  -- If the place is not found, return the leftovers,
  -- i.e. the number of additional segments that need to be traversed
  -- to reach the point of failure from a previous top-level segment.
  insertDangling ::
    Int ->
    Slice ->
    (EndSegment -> EndSegment) ->
    Slice ->
    EndSegment ->
    Either String { seg' :: EndSegment, leftover :: Maybe Int }
  insertDangling dist topSlice mkLeftParent leftSlice seg
    -- can be inserted here: return update segment (no leftovers)
    | dist == 0, not seg.trans.is2nd = Right $ { seg': mkLeftParent seg, leftover: Nothing }
    -- cannot be inserted here: descend and return updated segment + leftovers
    | otherwise = case seg.op of
      -- freeze: cannot descend -> introduce leftovers
      Freeze -> Right { seg': seg, leftover: Just 0 }
      -- split: first try to descend on childr, then on childl; pass on remaining leftovers
      Split { childl, childr } -> do
        { seg': childr', leftover: lor } <- insertDangling dist topSlice mkLeftParent childl.rslice childr
        childlFixed <-
          if dist == 0 then case childl.rslice.parents of
            MergeParents { left } -> Right $ resetExpls $ setParents (MergeParents { left: leftSlice, right: topSlice }) childl
            _ -> Left "invalid derivation structure: non-merge parents on merge slice"
          else
            pure childl
        case lor of
          Nothing -> pure $ { seg': seg { op = Split { childl: childlFixed, childr: childr' } }, leftover: Nothing }
          Just nlor -> do
            { seg': childl'End, leftover: lol } <- insertDangling (dist + nlor + 1) topSlice mkLeftParent leftSlice (detachSegment childlFixed)
            let
              childl' = attachSegment childl'End childlFixed.rslice

              seg' = seg { op = Split { childl: childl', childr: childr' } }
            pure $ { seg', leftover: (_ + 1) <$> lol }
      -- hori: first try to descend on childr, then childm, then childl; pass on remaining leftovers
      Hori { childl, childm, childr } -> do
        { seg': childr', leftover: lor } <- insertDangling (dist - 1) topSlice mkLeftParent childm.rslice childr
        case lor of
          Nothing -> pure { seg': seg { op = Hori { childl, childm, childr: childr' } }, leftover: Nothing }
          Just nlor -> do
            { seg': childm'End, leftover: lom } <- insertDangling (dist + nlor) topSlice mkLeftParent childl.rslice (detachSegment childm)
            let
              childm' = attachSegment childm'End childm.rslice
            case lom of
              Nothing -> pure { seg': seg { op = Hori { childl, childm: childm', childr: childr' } }, leftover: Nothing }
              Just nlom -> do
                { seg': childl'End, leftover: lol } <- insertDangling (dist + 1 + nlom) topSlice mkLeftParent leftSlice (detachSegment childl)
                let
                  childl' = attachSegment childl'End childl.rslice

                  seg' = seg { op = Hori { childl: childl', childm: childm', childr: childr' } }
                pure { seg', leftover: (_ + 2) <$> lol }

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