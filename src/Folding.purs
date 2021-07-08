module Folding where

import Prelude
import Control.Monad.State as ST
import Data.Array as A
import Data.List (List(..), (:))
import Data.Map as M
import Leftmost (Leftmost(..))
import Model (Edges, EndSegment, Op(..), Reduction, Segment, Slice, SliceId(..), TransId, Transition, attachSegment)

type AgendaItem a
  = { seg :: Segment, more :: a }

type FFreeze a s
  = AgendaItem a -> ST.State s Unit

type FSplit a s
  = AgendaItem a -> { childl :: Segment, childr :: EndSegment } -> ST.State s (List (AgendaItem a))

type FHori a s
  = AgendaItem a ->
    AgendaItem a ->
    { childl :: Segment, childm :: Segment, childr :: EndSegment } ->
    ST.State s (List (AgendaItem a))

type AgendaAlg a s
  = { init :: Slice -> ST.State s Unit
    , freezeOnly :: FFreeze a s
    , freezeLeft :: FFreeze a s
    , splitOnly :: FSplit a s
    , splitLeft :: FSplit a s
    , splitRight :: AgendaItem a -> AgendaItem a -> { childl :: Segment, childr :: EndSegment } -> ST.State s (List (AgendaItem a))
    , hori :: FHori a s
    }

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
  Slice ->
  List (AgendaItem a) ->
  ST.State s Unit
walkGraph alg start agenda = do
  alg.init start
  foldAgenda alg agenda

-- evaluate a reduction to a graph (for rendering)
-- -----------------------------------------------
-- 
type GraphSlice
  = { depth :: Number, slice :: Slice }

type GraphTransition
  = { id :: TransId, left :: SliceId, right :: SliceId, edges :: Edges } -- TODO: just embed transition

type Graph
  = { slices :: M.Map SliceId GraphSlice
    , transitions :: M.Map TransId GraphTransition
    , horis :: List { child :: SliceId, parent :: SliceId }
    , maxd :: Number
    , maxx :: Number
    , currentDepth :: Number
    , leftId :: SliceId
    }

addGraphSlice :: Slice -> Number -> ST.State Graph Unit
addGraphSlice slice depth = do
  ST.modify_ add
  where
  gslice :: GraphSlice
  gslice = { depth, slice }

  add st =
    st
      { maxd = max depth st.maxd
      , maxx = max slice.x st.maxx
      , slices = M.insert slice.id gslice st.slices
      }

addGraphTrans :: Transition -> SliceId -> ST.State Graph Unit
addGraphTrans { id, edges } ir = ST.modify_ add
  where
  trans il = { left: il, right: ir, id, edges }

  add st = st { transitions = M.insert id (trans st.leftId) st.transitions }

addHoriEdge :: Slice -> Slice -> ST.State Graph Unit
addHoriEdge { id: child } { id: parent } = ST.modify_ \st -> st { horis = { child, parent } : st.horis }

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
      : { seg: attachSegment split.childr item.seg.rslice
        , more: { rdepth: item.more.rdepth }
        }
      : Nil

  splitRight left right split = do
    withLeftId left.seg.rslice.id
      $ addGraphTrans right.seg.trans right.seg.rslice.id
    pure
      $ { seg: split.childl, more: { rdepth: max left.more.rdepth right.more.rdepth + 1.0 } }
      : { seg: attachSegment split.childr right.seg.rslice
        , more: { rdepth: right.more.rdepth }
        }
      : Nil

  hori left right { childl, childm, childr } = do
    currentDepth <- ST.gets _.currentDepth
    addGraphTrans left.seg.trans left.seg.rslice.id
    addGraphSlice left.seg.rslice left.more.rdepth
    addHoriEdge childl.rslice left.seg.rslice
    addHoriEdge childm.rslice left.seg.rslice
    withLeftId left.seg.rslice.id
      $ addGraphTrans right.seg.trans right.seg.rslice.id
    let
      dsub = max currentDepth (max left.more.rdepth right.more.rdepth) + 1.0
    pure
      $ { seg: childl, more: { rdepth: dsub } }
      : { seg: childm, more: { rdepth: dsub } }
      : { seg: attachSegment childr right.seg.rslice, more: { rdepth: right.more.rdepth } }
      : Nil

evalGraph :: Reduction -> Graph
evalGraph reduction =
  flip ST.execState initState
    $ walkGraph graphAlg reduction.start agenda
  where
  agenda = map (\seg -> { seg, more: { rdepth: 0.0 } }) reduction.segments

  initState =
    { slices: M.empty
    , transitions: M.empty
    , horis: Nil
    , maxd: 0.0
    , maxx: 0.0
    , currentDepth: 0.0
    , leftId: SliceId 0
    }

-- evaluate a reduction to a leftmost derivation
-- ---------------------------------------------
--
nothingMore :: Segment -> { seg :: Segment, more :: Unit }
nothingMore seg = { seg, more: unit }

reductionToLeftmost :: Reduction -> Array (Leftmost Unit Unit Unit)
reductionToLeftmost reduction = A.reverse $ A.fromFoldable $ flip ST.execState Nil $ walkGraph lmAlg reduction.start agenda
  where
  agenda :: List (AgendaItem Unit)
  agenda = map nothingMore reduction.segments

  lmAlg :: AgendaAlg Unit (List (Leftmost Unit Unit Unit))
  lmAlg =
    { init: \_ -> pure unit
    , freezeOnly
    , freezeLeft
    , splitOnly
    , splitLeft
    , splitRight
    , hori
    }
    where
    freezeOnly _ = ST.modify_ (Cons $ LMFreezeOnly unit)

    freezeLeft _ = ST.modify_ (Cons $ LMFreezeLeft unit)

    splitOnly { seg } { childl, childr } = do
      ST.modify_ (Cons $ LMSplitOnly unit)
      pure $ nothingMore <$> childl : attachSegment childr seg.rslice : Nil

    splitLeft { seg } { childl, childr } = do
      ST.modify_ (Cons $ LMSplitLeft unit)
      pure $ nothingMore <$> childl : attachSegment childr seg.rslice : Nil

    splitRight _ { seg } { childl, childr } = do
      ST.modify_ (Cons $ LMSplitRight unit)
      pure $ nothingMore <$> childl : attachSegment childr seg.rslice : Nil

    hori _ { seg } { childl, childm, childr } = do
      ST.modify_ (Cons $ LMHorizontalize unit)
      pure $ nothingMore <$> childl : childm : attachSegment childr seg.rslice : Nil
