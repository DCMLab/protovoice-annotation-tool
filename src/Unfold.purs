module Unfold where

import Prelude
import Control.Monad.State as ST
import Data.List (List(..), (:))
import Data.Map as M
import Model (Edges, Notes, Op(..), Reduction, Segment, Slice, SliceId(..), StartStop, TransId, Transition)

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
  = { seg :: Segment, rdepth :: Number }

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

-- subAgenda :: forall a. Number -> List a -> List { depth :: Number, seg :: a }
-- subAgenda depth segments = map (\seg -> { seg, depth: depth + 1.0 }) segments
unfoldAgenda :: SliceId -> Number -> List AgendaItem -> ST.State Graph Unit
-- no transition left
unfoldAgenda _ _ Nil = pure unit

-- single transition left
unfoldAgenda leftId currentDepth (Cons item Nil) = do
  case item.seg.op of
    Freeze -> do
      addTrans item.seg.trans leftId item.seg.rslice.id
      addSlice item.seg.rslice item.rdepth
    Split { childl, childr } -> do
      addTrans item.seg.trans leftId item.seg.rslice.id
      unfoldAgenda leftId currentDepth
        $ { seg: childl, rdepth: max currentDepth item.rdepth + 1.0 }
        : { seg: childr, rdepth: item.rdepth }
        : Nil
    Hori _ -> pure unit -- TODO: fail in this case?

-- two or more transitions left
unfoldAgenda leftId currentDepth (Cons left agenda1@(Cons right agenda2)) = do
  case left.seg.op of
    -- left freeze
    Freeze -> do
      addTrans left.seg.trans leftId midSlice.id
      addSlice midSlice left.rdepth
      unfoldAgenda midSlice.id left.rdepth agenda1
    -- left split
    Split { childl, childr } -> do
      addTrans left.seg.trans leftId midSlice.id
      unfoldAgenda leftId currentDepth
        $ { seg: childl, rdepth: max currentDepth left.rdepth + 1.0 }
        : { seg: childr, rdepth: left.rdepth }
        : agenda1
    Hori { childl, childm, childr } -> case right.seg.op of
      -- right split
      Split rsplit -> do
        addTrans right.seg.trans midSlice.id rightSlice.id
        unfoldAgenda leftId currentDepth
          $ left
          : { seg: rsplit.childl, rdepth: max left.rdepth right.rdepth + 1.0 }
          : { seg: rsplit.childr, rdepth: right.rdepth }
          : agenda2
      -- hori
      _ -> do
        addTrans left.seg.trans leftId midSlice.id
        addSlice midSlice left.rdepth
        addTrans right.seg.trans midSlice.id rightSlice.id
        let
          dsub = max currentDepth (max left.rdepth right.rdepth) + 1.0
        unfoldAgenda leftId currentDepth
          $ { seg: childl, rdepth: dsub }
          : { seg: childm, rdepth: dsub }
          : { seg: childr, rdepth: right.rdepth }
          : agenda2
  -- TODO: check for right hori and allow to fail?
  where
  midSlice = left.seg.rslice

  rightSlice = right.seg.rslice

evalGraph :: Reduction -> Graph
evalGraph reduction =
  flip ST.execState initState
    $ do
        addSlice reduction.start 0.0
        unfoldAgenda (SliceId 0) 0.0 agenda
  where
  agenda = map (\seg -> { seg, rdepth: 0.0 }) reduction.segments

  initState =
    { slices: M.empty
    , transitions: M.empty
    , maxd: 0.0
    , maxx: 0.0
    }
