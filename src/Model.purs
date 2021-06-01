module Model where

import Prelude
import Data.Array as A
import Data.Foldable (find, foldl, for_)
import Data.Generic.Rep (class Generic)
import Data.Int (toNumber)
import Data.List (List(..), (:))
import Data.List as L
import Data.Map as M
import Data.Maybe (Maybe(..), maybe)
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
  = Array (Array { note :: Note, hold :: Boolean })

type Note
  = { pitch :: SimplePitch, id :: String }

data StartStop a
  = Start
  | Inner a
  | Stop

derive instance eqStartStop :: (Eq a) => Eq (StartStop a)

instance showStartStop :: (Show a) => Show (StartStop a) where
  show Start = "⋊"
  show Stop = "⋉"
  show (Inner a) = show a

type Edge
  = { left :: StartStop Note, right :: StartStop Note }

isRepeatingEdge :: Edge -> Boolean
isRepeatingEdge { left: Inner left, right: Inner right } = left.pitch == right.pitch

isRepeatingEdge { left, right } = left == right

type Notes
  = Array Note

type Edges
  = { regular :: Array Edge
    , passing :: Array Edge
    }

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

type Slice
  = { id :: SliceId
    , notes :: StartStop Notes
    , x :: Number
    }

type Transition
  = { id :: TransId
    , edges :: Edges
    }

emptyTrans :: TransId -> Transition
emptyTrans id = { id: id, edges: { regular: [], passing: [] } }

data Op
  = Freeze
  | Split { childl :: Segment, childr :: Segment }
  | Hori { childl :: Segment, childm :: Segment, childr :: Segment }

derive instance genericOp :: Generic Op _

instance showOp :: Show Op where
  show x = genericShow x

type Segment
  = { trans :: Transition
    , rslice :: Slice
    , op :: Op
    }

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

-- loading a piece
-- ===============
--
startSlice :: Slice
startSlice = { id: SliceId 0, notes: Start, x: 0.0 }

thawTrans :: Array Note -> Int -> Array { note :: Note, hold :: Boolean } -> Transition
thawTrans ties id slice =
  { id: TransId id
  , edges:
      { regular: A.catMaybes $ map findSecond ties
      , passing: []
      }
  }
  where
  notes = map _.note slice

  findSecond fst = (\snd -> { left: Inner fst, right: Inner snd }) <$> find (\snd -> snd.pitch == fst.pitch) notes

thawSlice :: Array { note :: Note, hold :: Boolean } -> Int -> Slice
thawSlice slice id = { id: SliceId id, notes: Inner $ map _.note slice, x: toNumber id }

thawPiece :: Piece -> Reduction
thawPiece piece =
  { start: startSlice
  , segments: L.fromFoldable $ segs
  , nextSliceId: SliceId $ imax + 2
  , nextTransId: TransId $ imax + 1
  }
  where
  thaw st slice = { seg, ties, i: st.i + 1 }
    where
    seg =
      Just
        { trans: thawTrans st.ties st.i slice
        , rslice: thawSlice slice (st.i + 1)
        , op: Freeze
        }

    ties = map _.note $ A.filter _.hold slice

  init = { seg: Nothing, ties: [], i: 0 }

  states = scanl thaw init piece

  imax = maybe 0 _.i $ (A.last states)

  lastSeg =
    { trans: thawTrans [] imax []
    , rslice: { id: SliceId (imax + 1), notes: Stop, x: toNumber (imax + 1) }
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
doAt :: forall a. (List Segment -> Maybe (Tuple a (List Segment))) -> (a -> Maybe (List Segment)) -> List Segment -> Maybe (List Segment)
doAt match f Nil = Nothing

doAt match f segs@(Cons seg rest) = case match segs of
  Just (Tuple m remainder) -> (\result -> result <> remainder) <$> f m
  Nothing -> Cons seg <$> doAt match f rest

-- | Merges two segment into a new segment with a 'Split' operation.
mkMerge :: TransId -> Segment -> Segment -> Segment
mkMerge id seg1 seg2 = { trans: emptyTrans id, rslice: seg2.rslice, op }
  where
  op = Split { childl: seg1, childr: seg2 }

-- | Applies a merge at slice 'sliceId', if it exists on the reduction surface.
mergeAtSlice :: SliceId -> Model -> Model
mergeAtSlice sliceId model = case doAt matchSlice tryMerge model.reduction.segments of
  Just segments' -> model { reduction { segments = segments', nextTransId = incT nextTId } }
  Nothing -> model
  where
  nextTId = model.reduction.nextTransId

  matchSlice :: List Segment -> Maybe (Tuple (Tuple Segment Segment) (List Segment))
  matchSlice = case _ of
    Cons seg1 (Cons seg2 rest) -> if seg1.rslice.id == sliceId then Just (Tuple (Tuple seg1 seg2) rest) else Nothing
    _ -> Nothing

  tryMerge :: Tuple Segment Segment -> Maybe (List Segment)
  tryMerge (Tuple seg1 seg2) = Just $ mkMerge nextTId seg1 seg2 : Nil

-- | Verticalizes three segments into two new segments,
-- the first of witch gets a 'Hori' operation.
mkVert :: TransId -> SliceId -> Segment -> Segment -> Segment -> Maybe (List Segment)
mkVert tid sid l@{ rslice: { notes: Inner notesl } } m@{ rslice: { notes: Inner notesr } } r
  | A.all isRepeatingEdge m.trans.edges.regular = Just $ pleft : pright : Nil
    where
    countPitches notes = foldl (\counts n -> M.insertWith (+) n.pitch 1 counts) M.empty notes

    newNotes counts = A.mapWithIndex mkNote pitches
      where
      mkNote i p = { pitch: p, id: "note" <> show sid <> "." <> show i }

      pitches = A.reverse $ A.concatMap (\(Tuple p c) -> A.replicate c p) $ M.toUnfoldable counts

    topSlice =
      { notes: Inner $ newNotes $ M.unionWith max (countPitches notesl) (countPitches notesr)
      , id: sid
      , x: (l.rslice.x + m.rslice.x) / 2.0
      }

    pleft = { trans: emptyTrans tid, rslice: topSlice, op: Hori { childl: l, childm: m, childr: r } }

    pright = { trans: emptyTrans (incT tid), rslice: r.rslice, op: Freeze }

mkVert _ _ _ _ _ = Nothing

-- | Applies a verticalization at transition 'transId', if it exists on the reduction surface.
vertAtMid :: TransId -> Model -> Model
vertAtMid transId model = case doAt matchMid tryVert model.reduction.segments of
  Just segments' ->
    model
      { reduction
        { segments = segments'
        , nextTransId = incT $ incT nextTId
        , nextSliceId = incS nextSId
        }
      }
  Nothing -> model
  where
  nextTId = model.reduction.nextTransId

  nextSId = model.reduction.nextSliceId

  matchMid = case _ of
    Cons l (Cons m (Cons r rest)) -> if m.trans.id == transId then Just (Tuple { l, m, r } rest) else Nothing
    _ -> Nothing

  tryVert { l, m, r } = mkVert nextTId nextSId l m r

-- | Undoes a merge at transition 'transId', if it exists, restoring its children.
undoMergeAtTrans :: TransId -> Model -> Model
undoMergeAtTrans transId model = case doAt matchTrans tryUnmerge model.reduction.segments of
  Just segments' -> model { reduction { segments = segments' } }
  Nothing -> model
  where
  matchTrans = case _ of
    Cons seg rest -> if seg.trans.id == transId then Just (Tuple seg rest) else Nothing
    _ -> Nothing

  tryUnmerge seg = case seg.op of
    Split { childl, childr } -> Just $ childl : childr : Nil
    _ -> Nothing

-- | Undoes a verticalization at slice 'sliceId', if it exists, restoring its children.
undoVertAtSlice :: SliceId -> Model -> Model
undoVertAtSlice sliceId model = case doAt matchSlice tryUnvert model.reduction.segments of
  Just segments' -> model { reduction { segments = segments' } }
  Nothing -> model
  where
  matchSlice = case _ of
    Cons pl (Cons pr rest) -> if pl.rslice.id == sliceId then Just (Tuple { pl, pr } rest) else Nothing
    _ -> Nothing

  tryUnvert { pl, pr } = case pl.op of
    Hori { childl, childm, childr } -> Just $ childl : childm : childr : Nil
    _ -> Nothing
