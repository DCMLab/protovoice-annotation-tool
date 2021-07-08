module Validation where

import Prelude
import Control.Monad.State as ST
import Data.Array (catMaybes)
import Data.Array as A
import Data.Foldable (fold, foldl)
import Data.Generic.Rep (class Generic)
import Data.List (List(..), (:))
import Data.Map as M
import Data.Maybe (Maybe(..))
import Data.Set as S
import Data.Show.Generic (genericShow)
import Data.Traversable (for_)
import Folding (AgendaAlg, nothingMore, walkGraph)
import Model (Edge, Note, NoteExplanation(..), Reduction, Slice, SliceId, StartStop(..), attachSegment, getInnerNotes, isRepeatingEdge, transEdges)

data NoteStatus
  = NSOk
  | NSNoExpl
  | NSInvalidExplanation

data EdgeStatus
  = ESOk
  | ESNotUsed
  | ESNotStepwise
  | ESNotRepetition

-- data HoriStatus
--   = HSOk
--   | HSNotSamePitch
data SliceStatus
  = SSOk
  | SSInvalidReduction

type Validation
  = { notes :: M.Map String NoteStatus
    , edges :: M.Map Edge EdgeStatus
    -- , horis :: M.Map { parent :: Note, child :: Note } HoriStatus
    , slices :: M.Map SliceId SliceStatus
    }

validateSlice :: Slice -> Validation -> Validation
validateSlice slice st =
  st
    { notes = notes'
    , slices = M.insert slice.id SSOk st.slices
    }
  where
  notes' = case slice.notes of
    Inner slicenotes -> foldl (\mp { id, stat } -> M.insert id stat mp) st.notes $ validateNote <$> slicenotes
    _ -> st.notes

validateNote :: { note :: Note, expl :: NoteExplanation } -> { id :: String, stat :: NoteStatus }
validateNote note = { id: note.note.id, stat }
  where
  stat = case note.expl of
    NoExpl -> NSNoExpl
    -- TODO: check ornament type correctness
    HoriExpl parent -> if note.note.pitch == parent.pitch then NSOk else NSInvalidExplanation
    LeftExpl expl -> case expl.orn of
      Nothing -> NSInvalidExplanation
      _ -> NSOk
    RightExpl expl -> case expl.orn of
      Nothing -> NSInvalidExplanation
      _ -> NSOk
    DoubleExpl expl -> case expl.orn of
      Nothing -> NSInvalidExplanation
      _ -> NSOk

validationAlg :: AgendaAlg Unit Validation
validationAlg = { init, freezeLeft, freezeOnly, splitLeft, splitOnly, splitRight, hori }
  where
  init start = ST.modify_ \st -> st { slices = M.insert start.id SSOk st.slices }

  freezeLeft { seg } = ST.modify_ (validateSlice seg.rslice)

  freezeOnly = freezeLeft

  splitLeft { seg } { childl, childr } = do
    -- check if mandatory edges are used
    -- TODO: check for invalid (e.g. non-stepwise) edges too
    ST.modify_ \st -> st { edges = foldl (\mp { edge, stat } -> M.insert edge stat mp) st.edges (leftChildren <> rightChildren) }
    pure $ nothingMore <$> childl : attachSegment childr seg.rslice : Nil
    where
    explainedEdges =
      fold $ catMaybes
        $ ( \{ note, expl } -> case expl of
              DoubleExpl { leftParent, rightParent } ->
                Just
                  { ls: S.singleton { left: Inner leftParent, right: Inner note }
                  , rs: S.singleton { left: Inner note, right: Inner rightParent }
                  }
              LeftExpl { rightParent } -> Just { ls: S.empty, rs: S.singleton { left: Inner note, right: Inner rightParent } }
              RightExpl { leftParent } -> Just { rs: S.empty, ls: S.singleton { left: Inner leftParent, right: Inner note } }
              _ -> Nothing
          )
        <$> getInnerNotes childl.rslice

    leftChildren = map (\edge -> { edge, stat: if S.member edge explainedEdges.ls then ESOk else ESNotUsed }) $ transEdges childl.trans

    rightChildren = map (\edge -> { edge, stat: if S.member edge explainedEdges.rs then ESOk else ESNotUsed }) $ transEdges childr.trans

  splitOnly = splitLeft

  splitRight _ = splitLeft

  hori { seg: seg1 } { seg: seg2 } { childl, childm, childr } = do
    -- check slice and its notes
    ST.modify_ (validateSlice seg1.rslice)
    -- check validity of hori reduction
    when (not $ A.all isRepeatingEdge childm.trans.edges.regular)
      $ ST.modify_ \st -> st { slices = M.insert seg1.rslice.id SSInvalidReduction st.slices }
    -- check edges in middle transition
    for_ childm.trans.edges.regular
      $ \edge ->
          ST.modify_ \st -> st { edges = M.insert edge (if isRepeatingEdge edge then ESOk else ESNotRepetition) st.edges }
    -- pass edges in the outer transitions
    let
      otherEdges = childm.trans.edges.passing <> transEdges childl.trans <> transEdges childr.trans
    ST.modify_ \st -> st { edges = foldl (\mp edge -> M.insert edge ESOk mp) st.edges otherEdges }
    -- return new agenda items
    pure $ nothingMore <$> childl : childm : attachSegment childr seg2.rslice : Nil

validateReduction :: Reduction -> Validation
validateReduction red = flip ST.execState emptyVal $ walkGraph validationAlg red.start agenda
  where
  emptyVal = { notes: M.empty, edges: M.empty, slices: M.empty }

  agenda = nothingMore <$> red.segments

-- instances
-- ---------
-- NoteStatus
derive instance eqNoteStatus :: Eq NoteStatus

derive instance genericNoteStatus :: Generic NoteStatus _

instance showNoteStatus :: Show NoteStatus where
  show ns = genericShow ns

-- EdgeStatus
derive instance eqEdgeStatus :: Eq EdgeStatus

derive instance genericEdgeStatus :: Generic EdgeStatus _

instance showEdgeStatus :: Show EdgeStatus where
  show ns = genericShow ns

-- HoriStatus
-- derive instance eqHoriStatus :: Eq HoriStatus
-- derive instance genericHoriStatus :: Generic HoriStatus _
-- instance showHoriStatus :: Show HoriStatus where
--   show ns = genericShow ns
-- SliceStatus
derive instance eqSliceStatus :: Eq SliceStatus

derive instance genericSliceStatus :: Generic SliceStatus _

instance showSliceStatus :: Show SliceStatus where
  show ns = genericShow ns
