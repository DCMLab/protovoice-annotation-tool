module Model where

import Prelude
import Data.Array as A
import Data.Either (Either(..))
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

getInner :: forall a. StartStop a -> Maybe a
getInner (Inner a) = Just a

getInner _ = Nothing

derive instance eqStartStop :: (Eq a) => Eq (StartStop a)

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

data Parents
  = NoParents
  | VertParent SliceId
  | MergeParents { left :: SliceId, right :: SliceId }

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

type Transition
  = { id :: TransId
    , edges :: Edges
    , is2nd :: Boolean
    }

emptyTrans :: TransId -> Boolean -> Transition
emptyTrans id is2nd = { id, is2nd, edges: { regular: [], passing: [] } }

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
  , notes: Inner $ map _.note slice
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

  op = Split { childl: setParents pars seg1, childr: seg2 }

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
    countPitches notes = foldl (\counts n -> M.insertWith (+) n.pitch 1 counts) M.empty notes

    newNotes counts = A.mapWithIndex mkNote pitches
      where
      mkNote i p = { pitch: p, id: "note" <> show sid <> "." <> show i }

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
            , childr: r
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
    Split { childl, childr } -> Right $ (setParents NoParents childl) : childr : Nil
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
        $ (setParents NoParents childl)
        : (setParents NoParents childm)
        : childr
        : Nil
    _ -> Left "Operation is not a hori!"
