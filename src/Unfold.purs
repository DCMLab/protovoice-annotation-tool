module Unfold where

import Model
import Prelude
import Control.Monad.State as ST
import Data.List (List(..))
import Data.Map as M

type GraphSlice
  = { id :: Int, depth :: Number, x :: Number, notes :: StartStop Notes }

type GraphTransition
  = { id :: Int, left :: Int, right :: Int, edges :: Edges }

type Graph
  = { slices :: M.Map Int GraphSlice
    , transitions :: M.Map Int GraphTransition
    , maxd :: Number
    , maxx :: Number
    }

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

addTrans :: Transition -> Int -> Int -> ST.State Graph Unit
addTrans { id, edges } il ir = ST.modify_ add
  where
  trans = { left: il, right: ir, id, edges }

  add st = st { transitions = M.insert id trans st.transitions }

nextTrans :: Int -> List { seg :: Segment, depth :: Number } -> ST.State Graph Unit
nextTrans _ Nil = pure unit

nextTrans leftId (Cons item agenda) = do
  addSlice item.seg.rslice item.depth
  let
    rightId = item.seg.rslice.id

    nextLeftId = rightId
  addTrans item.seg.trans leftId rightId
  case item.seg.op of
    Freeze -> pure unit
    Split -> pure unit -- TODO
    Hori -> pure unit -- TODO
  nextTrans nextLeftId agenda

evalGraph :: Reduction -> Graph
evalGraph reduction =
  flip ST.execState initState
    $ do
        addSlice reduction.start 0.0
        nextTrans 0 agenda
  where
  agenda = map (\seg -> { seg, depth: 0.0 }) reduction.segments

  initState =
    { slices: M.empty
    , transitions: M.empty
    , maxd: 0.0
    , maxx: 0.0
    }
