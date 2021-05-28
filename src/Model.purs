module Model where

import Prelude
import Data.Array as A
import Data.Map as M
import Data.Generic.Rep (class Generic)
import Data.List (List(..))
import Data.List as L
import Data.Maybe (Maybe(..), maybe)
import Data.Show.Generic (genericShow)
import Data.Traversable (for, scanl, traverse, foldl)
import Data.Tuple (Tuple(..))
import SimplePitch (SimplePitch, parseSimplePitch)

type Piece
  = Array (Array { pitch :: Note, hold :: Boolean })

type Note
  = SimplePitch

data StartStop a
  = Start
  | Inner a
  | Stop

instance showStartStop :: (Show a) => Show (StartStop a) where
  show Start = "⋊"
  show Stop = "⋉"
  show (Inner a) = show a

type Edge
  = { left :: StartStop Note, right :: StartStop Note }

-- type PassingEdge
--   = { left :: Note, right :: Note }
type Notes
  = M.Map Note Int

getNotes :: Notes -> Array (Tuple Note Int)
getNotes = M.toUnfoldable

type Edges
  = { regular :: Array Edge
    , passing :: Array Edge
    }

type Slice
  = { id :: Int
    , notes :: StartStop Notes
    }

type Transition
  = { id :: Int
    , edges :: Edges
    }

data Op
  = Freeze
  | Split
  | Hori

derive instance genericOp :: Generic Op _

instance showOp :: Show Op where
  show = genericShow

type Segment
  = { trans :: Transition
    , rslice :: Slice
    , op :: Op
    }

type Reduction
  = { start :: Slice
    , segments :: List Segment
    , nextTransId :: Int
    , nextSliceId :: Int
    }

type Model
  = { piece :: Piece
    , reduction :: Reduction
    }

startSlice :: Slice
startSlice = { id: 0, notes: Start }

thawTrans :: Array Note -> Int -> Transition
thawTrans ties id =
  { id
  , edges:
      { regular: map (\a -> { left: Inner a, right: Inner a }) ties
      , passing: []
      }
  }

thawSlice :: Array { pitch :: Note, hold :: Boolean } -> Int -> Slice
thawSlice slice id = { id, notes: Inner $ count $ map _.pitch slice }
  where
  count = foldl (\acc x -> M.insertWith (+) x 1 acc) M.empty

thawPiece :: Piece -> Reduction
thawPiece piece =
  { start: startSlice
  , segments: L.fromFoldable $ segs
  , nextSliceId: imax + 2
  , nextTransId: imax + 1
  }
  where
  thaw st slice = { seg, ties, i: st.i + 1 }
    where
    seg =
      Just
        { trans: thawTrans st.ties st.i
        , rslice: thawSlice slice (st.i + 1)
        , op: Freeze
        }

    ties = map _.pitch $ A.filter _.hold slice

  init = { seg: Nothing, ties: [], i: 0 }

  states = scanl thaw init piece

  imax = maybe 0 _.i $ (A.last states)

  lastSeg =
    { trans: thawTrans [] imax
    , rslice: { id: imax + 1, notes: Stop }
    , op: Freeze
    }

  segs = A.catMaybes (map _.seg states) <> [ lastSeg ]

loadPiece :: Piece -> Model
loadPiece piece = { piece, reduction: thawPiece piece }

pieceFromJSON :: Array (Array { pitch :: String, hold :: Boolean }) -> Maybe Piece
pieceFromJSON piece =
  for piece \slice ->
    for slice \note ->
      note { pitch = _ } <$> parseSimplePitch note.pitch
