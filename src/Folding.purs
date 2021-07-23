module Folding where

import Prelude
import Control.Bind (bindFlipped)
import Control.Monad.State as ST
import Data.Array (catMaybes)
import Data.Array as A
import Data.Either (Either(..))
import Data.Foldable (class Foldable, foldl)
import Data.List (List(..), (:))
import Data.List as L
import Data.Map as M
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Tuple (Tuple(..))
import Leftmost (FreezeOp(..), HoriChildren(..), HoriOp(..), Leftmost(..), RootOrnament(..), SplitOp(..), horiLeftChildren, horiRightChildren, splitGetChildNotes)
import Model (DoubleOrnament(..), Edges, EndSegment, Model, Note, NoteExplanation(..), Notes, Op(..), Parents(..), Piece, Reduction, Segment, Slice, SliceId(..), StartStop(..), Time, TransId(..), Transition, attachSegment, detachSegment, getInnerNotes, incS, incT, parentEdges, vertEdgesLeft, vertEdgesRight)

type AgendaItem a
  = { seg :: Segment, more :: a }

nothingMore :: Segment -> { seg :: Segment, more :: Unit }
nothingMore seg = { seg, more: unit }

type FFreeze a s
  = Slice -> AgendaItem a -> ST.State s Unit

type FSplit a s
  = Slice -> AgendaItem a -> { childl :: Segment, childr :: EndSegment } -> ST.State s (List (AgendaItem a))

type FSplitRight a s
  = Slice -> AgendaItem a -> AgendaItem a -> { childl :: Segment, childr :: EndSegment } -> ST.State s (List (AgendaItem a))

type FHori a s
  = Slice ->
    AgendaItem a ->
    AgendaItem a ->
    { childl :: Segment, childm :: Segment, childr :: EndSegment } ->
    ST.State s (List (AgendaItem a))

type AgendaAlg a s
  = { init :: Slice -> ST.State s Unit
    , freezeOnly :: FFreeze a s
    , freezeLeft :: FFreeze a s
    , splitOnly :: FSplit a s
    , splitLeft :: FSplit a s
    , splitRight :: FSplitRight a s
    , hori :: FHori a s
    }

foldAgenda ::
  forall a s.
  AgendaAlg a s ->
  Slice ->
  List (AgendaItem a) ->
  ST.State s Unit
-- no transition left
foldAgenda _ _ Nil = pure unit

-- single transition left
foldAgenda alg leftSlice (Cons item Nil) = do
  case item.seg.op of
    Freeze -> alg.freezeOnly leftSlice item
    Split split -> do
      children <- alg.splitOnly leftSlice item split
      foldAgenda alg leftSlice children
    Hori _ -> pure unit -- TODO: fail in this case?

-- two or more transitions left
foldAgenda alg leftSlice (Cons left agenda1@(Cons right agenda2)) = do
  case left.seg.op of
    -- left freeze
    Freeze -> do
      alg.freezeLeft leftSlice left
      foldAgenda alg left.seg.rslice agenda1
    -- left split
    Split split -> do
      children <- alg.splitLeft leftSlice left split
      foldAgenda alg leftSlice $ children <> agenda1
    Hori hori -> case right.seg.op of
      -- right split
      Split rsplit -> do
        children <- alg.splitRight leftSlice left right rsplit
        foldAgenda alg leftSlice $ left : children <> agenda2
      -- hori
      _ -> do
        children <- alg.hori leftSlice left right hori
        foldAgenda alg leftSlice $ children <> agenda2
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
  foldAgenda alg start agenda

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

addGraphTrans :: SliceId -> Transition -> SliceId -> ST.State Graph Unit
addGraphTrans il { id, edges } ir =
  ST.modify_ \st ->
    st { transitions = M.insert id { left: il, right: ir, id, edges } st.transitions }

addHoriEdge :: Slice -> Slice -> ST.State Graph Unit
addHoriEdge { id: child } { id: parent } = ST.modify_ \st -> st { horis = { child, parent } : st.horis }

-- withLeftId :: forall a. SliceId -> ST.State Graph a -> ST.State Graph a
-- withLeftId id action = do
--   oldid <- ST.gets _.leftId
--   ST.modify_ \st -> st { leftId = id }
--   res <- action
--   ST.modify_ \st -> st { leftId = oldid }
--   pure res
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

  freezeTrans lSlice item = do
    addGraphTrans lSlice.id item.seg.trans item.seg.rslice.id
    addGraphSlice item.seg.rslice item.more.rdepth
    ST.modify_ \st -> st { currentDepth = item.more.rdepth }

  splitTrans lSlice item split = do
    currentDepth <- ST.gets _.currentDepth
    addGraphTrans lSlice.id item.seg.trans item.seg.rslice.id
    pure
      $ { seg: split.childl, more: { rdepth: max currentDepth item.more.rdepth + 1.0 } }
      : { seg: attachSegment split.childr item.seg.rslice
        , more: { rdepth: item.more.rdepth }
        }
      : Nil

  splitRight _ left right split = do
    addGraphTrans left.seg.rslice.id right.seg.trans right.seg.rslice.id
    pure
      $ { seg: split.childl, more: { rdepth: max left.more.rdepth right.more.rdepth + 1.0 } }
      : { seg: attachSegment split.childr right.seg.rslice
        , more: { rdepth: right.more.rdepth }
        }
      : Nil

  hori lSlice left right { childl, childm, childr } = do
    currentDepth <- ST.gets _.currentDepth
    addGraphTrans lSlice.id left.seg.trans left.seg.rslice.id
    addGraphSlice left.seg.rslice left.more.rdepth
    addHoriEdge childl.rslice left.seg.rslice
    addHoriEdge childm.rslice left.seg.rslice
    addGraphTrans left.seg.rslice.id right.seg.trans right.seg.rslice.id
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
    }

-- evaluate a reduction to a leftmost derivation
-- ---------------------------------------------
--
data Elaboration a b c d
  = ET a
  | EN b
  | ER c
  | EL d

partitionElaborations :: forall t a b c d. Foldable t => t (Elaboration a b c d) -> { t :: List a, n :: List b, r :: List c, l :: List d }
partitionElaborations = foldl select { t: Nil, n: Nil, r: Nil, l: Nil }
  where
  select { t, n, r, l } = case _ of
    ET t' -> { t: t' : t, n, r, l }
    EN n' -> { t, n: n' : n, r, l }
    ER r' -> { t, n, r: r' : r, l }
    EL l' -> { t, n, r, l: l' : l }

reductionToLeftmost :: Model -> Either String (Array (Leftmost SplitOp FreezeOp HoriOp))
reductionToLeftmost { reduction, piece } = do
  st' <- flip ST.execState (Right { lm: Nil, piece: Cons { time: Left "", notes: [] } $ L.fromFoldable piece }) $ walkGraph lmAlg reduction.start agenda
  pure $ A.reverse $ A.fromFoldable st'.lm
  where
  agenda :: List (AgendaItem Unit)
  agenda = map nothingMore reduction.segments

  lmAlg ::
    AgendaAlg Unit
      ( Either
          String
          { lm :: List (Leftmost SplitOp FreezeOp HoriOp)
          , piece :: List { time :: Time, notes :: Array { hold :: Boolean, note :: Note } }
          }
      )
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
    getUnexplained slice = map _.note $ A.filter (\{ expl } -> expl == NoExpl) $ getInnerNotes slice

    mkFreeze seg time = FreezeOp { ties: seg.trans.edges.regular, prevTime: time }

    mkSplit seg childl childr =
      let
        { t, n, r, l } = partitionElaborations $ catMaybes $ map getElaboration $ getInnerNotes childl.rslice
      in
        SplitOp
          { regular: M.fromFoldableWith (<>) t
          , passing: M.fromFoldableWith (<>) n
          , fromLeft: M.fromFoldableWith (<>) r
          , fromRight: M.fromFoldableWith (<>) l
          , unexplained: getUnexplained childl.rslice
          , keepLeft: childl.trans.edges.regular
          , keepRight: childr.trans.edges.regular
          , ids: { left: childl.trans.id, slice: childl.rslice.id, right: childr.trans.id }
          }
      where
      getElaboration n = case n.expl of
        DoubleExpl { orn: orn, leftParent, rightParent } ->
          if orn == Just PassingLeft || orn == Just PassingRight || orn == Just PassingMid then
            Just $ EN $ Tuple { left: Inner leftParent, right: Inner rightParent } [ { child: n.note, orn } ]
          else
            Just $ ET $ Tuple { left: Inner leftParent, right: Inner rightParent } [ { child: n.note, orn: Right orn } ]
        RightExpl { orn: orn, leftParent } -> Just $ ER $ Tuple leftParent [ { child: n.note, orn } ]
        LeftExpl { orn: orn, rightParent } -> Just $ EL $ Tuple rightParent [ { child: n.note, orn } ]
        RootExpl -> Just $ ET $ Tuple { left: Start, right: Stop } [ { child: n.note, orn: Left RootNote } ]
        _ -> Nothing

    mkHori seg childl childm childr =
      let
        children = catMaybes $ map (horiChild LeftChild) (getInnerNotes childl.rslice) <> map (horiChild RightChild) (getInnerNotes childm.rslice)
      in
        HoriOp
          { children: M.fromFoldableWith (<>) children
          , midEdges: childm.trans.edges
          , ids: { left: childl.trans.id, lslice: childl.rslice.id, mid: childm.trans.id, rslice: childm.rslice.id, right: childr.trans.id }
          , unexplained: { left: getUnexplained childl.rslice, right: getUnexplained childm.rslice }
          }
      where
      horiChild wrapper { note, expl } = case expl of
        HoriExpl parent -> Just $ Tuple parent $ wrapper note
        _ -> Nothing

    modifyEither = ST.modify_ <<< bindFlipped

    freezeOnly lSlice { seg } =
      modifyEither \st -> case st.piece of
        Nil -> Left "Piece and reduction do not fit together: piece is too short!"
        Cons nextSlice piece' -> Right st { lm = Cons (LMFreezeOnly $ mkFreeze seg nextSlice.time) st.lm, piece = piece' }

    freezeLeft lSlice { seg } =
      modifyEither \st -> case st.piece of
        Nil -> Left "Piece and reduction do not fit together: piece is too short!"
        Cons nextSlice piece' -> Right st { lm = Cons (LMFreezeLeft $ mkFreeze seg nextSlice.time) st.lm, piece = piece' }

    splitOnly _ { seg } { childl, childr } = do
      modifyEither \st -> Right st { lm = Cons (LMSplitOnly $ mkSplit seg childl childr) st.lm }
      pure $ nothingMore <$> childl : attachSegment childr seg.rslice : Nil

    splitLeft _ { seg } { childl, childr } = do
      modifyEither \st -> Right st { lm = Cons (LMSplitLeft $ mkSplit seg childl childr) st.lm }
      pure $ nothingMore <$> childl : attachSegment childr seg.rslice : Nil

    splitRight _ _ { seg } { childl, childr } = do
      modifyEither \st -> Right st { lm = Cons (LMSplitRight $ mkSplit seg childl childr) st.lm }
      pure $ nothingMore <$> childl : attachSegment childr seg.rslice : Nil

    hori _ _ { seg } { childl, childm, childr } = do
      modifyEither \st -> Right st { lm = Cons (LMHorizontalize $ mkHori seg childl childm childr) st.lm }
      pure $ nothingMore <$> childl : childm : attachSegment childr seg.rslice : Nil

-- unfold a leftmost derivation to a reduction
-- -------------------------------------------
-- 
leftmostToReduction ::
  Array { trans :: Transition, rslice :: { id :: SliceId, notes :: StartStop (Array Note) } } ->
  Array (Leftmost SplitOp FreezeOp HoriOp) ->
  Either String Model
leftmostToReduction topSegments deriv = do
  (Tuple segments st) <-
    ST.runStateT
      (go startSlice (L.fromFoldable $ prepareTop <$> topSegments) (L.fromFoldable deriv))
      { maxS: SliceId 0, maxT: TransId 0, x: 0.0, piece: [] }
  pure
    { reduction:
        { start: startSlice
        , segments
        , nextSliceId: incS st.maxS
        , nextTransId: incT st.maxT
        }
    , piece: fromMaybe [] $ A.tail st.piece
    }
  where
  startSlice = { id: SliceId 0, x: 0.0, notes: Start, parents: NoParents }

  prepareTop { trans, rslice: { id, notes } } =
    { transId: trans.id
    , rslice:
        { id
        , parents: NoParents
        , notes: map (\note -> { note, expl: NoExpl }) <$> notes
        }
    }

  go ::
    Slice ->
    List { transId :: TransId, rslice :: { id :: SliceId, notes :: StartStop Notes, parents :: Parents SliceId } } ->
    List (Leftmost SplitOp FreezeOp HoriOp) ->
    ST.StateT
      { maxS :: SliceId
      , maxT :: TransId
      , x :: Number
      , piece :: Piece
      }
      (Either String)
      (List Segment)
  go _ Nil Nil = pure Nil

  go leftSlice (Cons top tops) (Cons op ops) = case tops of
    -- two or more tops left
    Cons top2 tops2 -> case op of
      LMFreezeLeft f -> do
        top' <- freeze leftSlice f top
        Cons top' <$> go top'.rslice tops ops
      LMSplitLeft split -> do
        { childlTop, childrTop } <- prepareSplit leftSlice.id top split
        segs <- go leftSlice (Cons childlTop $ Cons childrTop tops) ops
        case segs of
          Cons childl (Cons childr rest) -> pure $ Cons (completeSplit top childl childr) rest
          _ -> ST.lift $ Left "Operations after splitLeft do not fit!"
      LMSplitRight split -> do
        { childlTop, childrTop } <- prepareSplit top.rslice.id top2 split
        segs <- go leftSlice (Cons top $ Cons childlTop $ Cons childrTop tops2) ops
        case segs of
          Cons top' (Cons childl (Cons childr rest)) -> pure $ Cons top' $ Cons (completeSplit top2 childl childr) rest
          _ -> ST.lift $ Left "Operations after splitRight do not fit!"
      LMHorizontalize hori -> do
        { childlTop, childmTop, childrTop } <- prepareHori top top2 hori
        segs <- go leftSlice (Cons childlTop $ Cons childmTop $ Cons childrTop tops2) ops
        case segs of
          Cons childl (Cons childm (Cons childr rest)) -> do
            { segl, segr } <- ST.lift $ completeHori top top2 childl childm childr
            pure $ Cons segl $ Cons segr rest
          _ -> ST.lift $ Left "Operations after hori do not fit!"
      _ -> ST.lift $ Left "Applying a single-transition operation while several transitions are left!"
    -- only a single top left
    Nil -> case op of
      LMFreezeOnly f -> do
        seg <- freeze leftSlice f top
        pure $ Cons seg Nil
      LMSplitOnly split -> do
        { childlTop, childrTop } <- prepareSplit leftSlice.id top split
        segs <- go leftSlice (Cons childlTop $ Cons childrTop Nil) ops
        case segs of
          Cons childl (Cons childr Nil) -> pure $ Cons (completeSplit top childl childr) Nil
          _ -> ST.lift $ Left "Operations after splitOnly do not fit!"
      _ -> ST.lift $ Left "Applying a non-single operation to a single transition!"

  go _ _ _ = ST.lift $ Left "Lengths of top-level and derivation do not match!"

  freeze :: Slice -> FreezeOp -> _ -> _
  freeze lslice (FreezeOp fop) { transId: tid, rslice: { id: sid, notes, parents } } = do
    let
      holdNote { note } = { note, hold: A.any (\{ left } -> left == Inner note) fop.ties }

      pieceNotes = holdNote <$> getInnerNotes lslice
    st <-
      ST.modify \st ->
        st
          { x = st.x + 1.0
          , maxT = max st.maxT tid
          , maxS = max st.maxS sid
          , piece = A.snoc st.piece { notes: pieceNotes, time: fop.prevTime }
          }
    pure
      $ { trans: { id: tid, edges: { passing: [], regular: fop.ties }, is2nd: false }
        , rslice: { id: sid, notes, parents, x: st.x }
        , op: Freeze
        }

  prepareSplit leftSliceId top split@(SplitOp s) = do
    ST.modify_ \st -> st { maxT = max st.maxT top.transId }
    childNotes <- ST.lift $ splitGetChildNotes split
    let
      childSlice = { id: s.ids.slice, notes: Inner childNotes, parents: MergeParents { left: leftSliceId, right: top.rslice.id } }

      childlTop = { transId: s.ids.left, rslice: childSlice }

      childrTop = { transId: s.ids.right, rslice: top.rslice }
    pure { childlTop, childrTop }

  completeSplit top childl childr =
    { trans: { id: top.transId, edges: parentEdges childl.rslice, is2nd: childl.trans.is2nd }
    , rslice: childr.rslice
    , op: Split { childl, childr: detachSegment childr }
    }

  prepareHori topl topr hori@(HoriOp h) = do
    ST.modify_ \st -> st { maxT = max st.maxT (max topl.transId topr.transId), maxS = max st.maxS topl.rslice.id }
    let
      childlTop =
        { transId: h.ids.left
        , rslice: { id: h.ids.lslice, notes: Inner $ horiLeftChildren hori, parents: VertParent topl.rslice.id }
        }

      childmTop =
        { transId: h.ids.mid
        , rslice: { id: h.ids.rslice, notes: Inner $ horiRightChildren hori, parents: VertParent topl.rslice.id }
        }

      childrTop = { transId: h.ids.right, rslice: topr.rslice }
    pure { childlTop, childmTop, childrTop }

  completeHori topl topr childl childm childr = do
    edgesL <- vertEdgesLeft childl.trans.edges childl.rslice
    edgesR <- vertEdgesRight childr.trans.edges childm.rslice
    let
      segl =
        { trans: { id: topl.transId, edges: edgesL, is2nd: false }
        , rslice: topSlice
        , op: Hori { childl, childm, childr: detachSegment childr }
        }

      segr =
        { trans: { id: topr.transId, edges: edgesR, is2nd: true }
        , rslice: childr.rslice
        , op: Freeze
        }
    pure { segl, segr }
    where
    { id, notes, parents } = topl.rslice

    topSlice = { id, notes, parents, x: (childl.rslice.x + childm.rslice.x) / 2.0 }