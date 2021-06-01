module Unfold where

import Prelude
import Model
  ( Edges
  , Notes
  , Op(..)
  , Reduction
  , Segment
  , Slice
  , SliceId(..)
  , StartStop
  , TransId
  , Transition
  )
import Control.Monad.State as ST
import Data.List (List(..))
import Data.List as L
import Data.Map as M

type GraphSlice
  = { id :: SliceId, depth :: Number, x :: Number, notes :: StartStop Notes }

type GraphTransition
  = { id :: TransId, left :: SliceId, right :: SliceId, edges :: Edges }

type Graph
  = { slices :: M.Map SliceId GraphSlice
    , transitions :: M.Map TransId GraphTransition
    , maxd :: Number
    , maxx :: Number
    }

type AgendaItem
  = { seg :: Segment, depth :: Number, omitSlice :: Boolean }

addSlice :: Slice -> Number -> ST.State Graph Unit
addSlice { id, notes, x } depth = do
  ST.modify_ add
  where
  slice :: GraphSlice
  slice = { id, depth, x, notes }

  add st =
    st
      { maxd = max depth st.maxd
      , maxx = max x st.maxx
      , slices = M.insert id slice st.slices
      }

addTrans :: Transition -> SliceId -> SliceId -> ST.State Graph Unit
addTrans { id, edges } il ir = ST.modify_ add
  where
  trans = { left: il, right: ir, id, edges }

  add st = st { transitions = M.insert id trans st.transitions }

nextTrans :: SliceId -> List AgendaItem -> ST.State Graph Unit
nextTrans _ Nil = pure unit

nextTrans leftId (Cons item agenda) = do
  unless item.omitSlice
    $ addSlice item.seg.rslice item.depth
  let
    rightId = item.seg.rslice.id
  addTrans item.seg.trans leftId rightId
  case item.seg.op of
    Freeze -> pure unit
    Split { childl, childr } ->
      nextTrans leftId
        $ subAgenda item.depth
            [ { seg: childl, omitSlice: false }
            , { seg: childr, omitSlice: true }
            ]
    Hori { childl, childm, childr } ->
      nextTrans leftId
        $ subAgenda item.depth
            [ { seg: childl, omitSlice: false }
            , { seg: childm, omitSlice: false }
            , { seg: childr, omitSlice: true }
            ]
  nextTrans rightId agenda
  where
  subAgenda depth items = L.fromFoldable $ map (\it -> { seg: it.seg, depth: depth + 1.0, omitSlice: it.omitSlice }) items

evalGraph :: Reduction -> Graph
evalGraph reduction =
  flip ST.execState initState
    $ do
        addSlice reduction.start 0.0
        nextTrans (SliceId 0) agenda
  where
  agenda = map (\seg -> { seg, depth: 0.0, omitSlice: false }) reduction.segments

  initState =
    { slices: M.empty
    , transitions: M.empty
    , maxd: 0.0
    , maxx: 0.0
    }
