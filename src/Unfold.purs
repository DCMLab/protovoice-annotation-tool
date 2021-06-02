module Unfold where

import Prelude
import Control.Monad.State as ST
import Data.List (List(..), (:))
import Data.Map as M
import Model (Edges, Notes, Op(..), Reduction, Segment, Slice, SliceId(..), StartStop, TransId, Transition)

type AgendaItem a
  = { seg :: Segment, more :: a }

type FFreeze a s
  = AgendaItem a -> ST.State s Unit

type FSplit a s
  = AgendaItem a -> { childl :: Segment, childr :: Segment } -> ST.State s (List (AgendaItem a))

type FHori a s
  = AgendaItem a ->
    AgendaItem a ->
    { childl :: Segment, childm :: Segment, childr :: Segment } ->
    ST.State s (List (AgendaItem a))

type AgendaAlg a s
  = { init :: Slice -> ST.State s Unit
    , freezeOnly :: FFreeze a s
    , freezeLeft :: FFreeze a s
    , splitOnly :: FSplit a s
    , splitLeft :: FSplit a s
    , splitRight :: AgendaItem a -> AgendaItem a -> { childl :: Segment, childr :: Segment } -> ST.State s (List (AgendaItem a))
    , hori :: FHori a s
    }

withLeftId :: forall a. SliceId -> ST.State Graph a -> ST.State Graph a
withLeftId id action = do
  oldid <- ST.gets _.leftId
  ST.modify_ \st -> st { leftId = id }
  res <- action
  ST.modify_ \st -> st { leftId = oldid }
  pure res

graphAlg :: AgendaAlg { rdepth :: Number } Graph
graphAlg =
  { init
  , freezeOnly: freezeTrans
  , freezeLeft: freezeTrans
  , splitOnly: splitTrans
  , splitLeft: splitTrans
  , splitRight
  , hori
  }
  where
  init start = do
    addGraphSlice start 0.0
    ST.modify_ \st -> st { leftId = start.id }

  freezeTrans left = do
    addGraphTrans left.seg.trans left.seg.rslice.id
    addGraphSlice left.seg.rslice left.more.rdepth
    ST.modify_ \st -> st { currentDepth = left.more.rdepth, leftId = left.seg.rslice.id }

  splitTrans item split = do
    currentDepth <- ST.gets _.currentDepth
    addGraphTrans item.seg.trans item.seg.rslice.id
    pure
      $ { seg: split.childl, more: { rdepth: max currentDepth item.more.rdepth + 1.0 } }
      : { seg: split.childr, more: { rdepth: item.more.rdepth } }
      : Nil

  splitRight left right split = do
    withLeftId left.seg.rslice.id
      $ addGraphTrans right.seg.trans right.seg.rslice.id
    pure
      $ { seg: split.childl, more: { rdepth: max left.more.rdepth right.more.rdepth + 1.0 } }
      : { seg: split.childr, more: { rdepth: right.more.rdepth } }
      : Nil

  hori left right { childl, childm, childr } = do
    currentDepth <- ST.gets _.currentDepth
    addGraphTrans left.seg.trans left.seg.rslice.id
    addGraphSlice left.seg.rslice left.more.rdepth
    withLeftId left.seg.rslice.id
      $ addGraphTrans right.seg.trans right.seg.rslice.id
    let
      dsub = max currentDepth (max left.more.rdepth right.more.rdepth) + 1.0
    pure
      $ { seg: childl, more: { rdepth: dsub } }
      : { seg: childm, more: { rdepth: dsub } }
      : { seg: childr, more: { rdepth: right.more.rdepth } }
      : Nil

foldAgenda ::
  forall a s.
  AgendaAlg a s ->
  List (AgendaItem a) -> ST.State s Unit
-- no transition left
foldAgenda _ Nil = pure unit

-- single transition left
foldAgenda alg (Cons item Nil) = do
  case item.seg.op of
    Freeze -> alg.freezeOnly item
    Split split -> do
      children <- alg.splitOnly item split
      foldAgenda alg children
    Hori _ -> pure unit -- TODO: fail in this case?

-- two or more transitions left
foldAgenda alg (Cons left agenda1@(Cons right agenda2)) = do
  case left.seg.op of
    -- left freeze
    Freeze -> do
      alg.freezeLeft left
      foldAgenda alg agenda1
    -- left split
    Split split -> do
      children <- alg.splitLeft left split
      foldAgenda alg $ children <> agenda1
    Hori hori -> case right.seg.op of
      -- right split
      Split rsplit -> do
        children <- alg.splitRight left right rsplit
        foldAgenda alg $ left : children <> agenda2
      -- hori
      _ -> do
        children <- alg.hori left right hori
        foldAgenda alg $ children <> agenda2
  -- TODO: check for right hori and allow to fail?
  where
  midSlice = left.seg.rslice

  rightSlice = right.seg.rslice

walkGraph ::
  forall a s.
  AgendaAlg a s ->
  Reduction ->
  List (AgendaItem a) ->
  ST.State s Unit
walkGraph alg reduction agenda = do
  alg.init reduction.start
  foldAgenda alg agenda

-- evaluate a reduction to a graph (for rendering)
-- -----------------------------------------------
-- 
type GraphSlice
  = { id :: SliceId, depth :: Number, x :: Number, notes :: StartStop Notes }

type GraphTransition
  = { id :: TransId, left :: SliceId, right :: SliceId, edges :: Edges }

type Graph
  = { slices :: M.Map SliceId GraphSlice
    , transitions :: M.Map TransId GraphTransition
    , maxd :: Number
    , maxx :: Number
    , currentDepth :: Number
    , leftId :: SliceId
    }

addGraphSlice :: Slice -> Number -> ST.State Graph Unit
addGraphSlice { id, notes, x } depth = do
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

addGraphTrans :: Transition -> SliceId -> ST.State Graph Unit
addGraphTrans { id, edges } ir = ST.modify_ add
  where
  trans il = { left: il, right: ir, id, edges }

  add st = st { transitions = M.insert id (trans st.leftId) st.transitions }

evalGraph :: Reduction -> Graph
evalGraph reduction =
  flip ST.execState initState
    $ walkGraph graphAlg reduction agenda
  where
  agenda = map (\seg -> { seg, more: { rdepth: 0.0 } }) reduction.segments

  initState =
    { slices: M.empty
    , transitions: M.empty
    , maxd: 0.0
    , maxx: 0.0
    , currentDepth: 0.0
    , leftId: SliceId 0
    }

-- evaluate a reduction to a leftmost derivation
-- ---------------------------------------------
--
-- reductionToLeftmost :: Reduction -> Array (Leftmost Unit Unit Unit)
-- reductionToLeftmost reduction = flip ST.execState Nil $ walkGraph
