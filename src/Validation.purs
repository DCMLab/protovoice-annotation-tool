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

data NoteError
  = NSNoExpl
  | NSInvalidExplanation

data EdgeError
  = ESNotUsed
  | ESNotStepwise
  | ESNotRepetition

-- data HoriError
--   = HSOk
--   | HSNotSamePitch
data SliceError
  = SSInvalidReduction

type Validation
  = { notes :: M.Map String NoteError
    , edges :: M.Map Edge EdgeError
    -- , horis :: M.Map { parent :: Note, child :: Note } HoriError
    , slices :: M.Map SliceId SliceError
    }

validateSlice :: Slice -> Validation -> Validation
validateSlice slice st =
  st
    { notes = notes'
    }
  where
  notes' = case slice.notes of
    Inner slicenotes -> foldl (\mp { id, stat } -> M.insert id stat mp) st.notes $ catMaybes $ validateNote <$> slicenotes
    _ -> st.notes

validateNote :: { note :: Note, expl :: NoteExplanation } -> Maybe { id :: String, stat :: NoteError }
validateNote note = do
  stat <- err
  pure { id: note.note.id, stat }
  where
  err = case note.expl of
    NoExpl -> Just NSNoExpl
    -- TODO: check ornament type correctness
    HoriExpl parent -> if note.note.pitch == parent.pitch then Nothing else Just NSInvalidExplanation
    LeftExpl expl -> case expl.orn of
      Nothing -> Just NSInvalidExplanation
      _ -> Nothing
    RightExpl expl -> case expl.orn of
      Nothing -> Just NSInvalidExplanation
      _ -> Nothing
    DoubleExpl expl -> case expl.orn of
      Nothing -> Just NSInvalidExplanation
      _ -> Nothing

validationAlg :: AgendaAlg Unit Validation
validationAlg = { init, freezeLeft, freezeOnly, splitLeft, splitOnly, splitRight, hori }
  where
  init start = pure unit

  freezeLeft { seg } = do
    ST.modify_ $ validateSlice seg.rslice

  freezeOnly = freezeLeft

  splitLeft { seg } { childl, childr } = do
    -- check if mandatory edges are used
    -- TODO: check for invalid (e.g. non-stepwise) edges too
    ST.modify_ \st -> st { edges = foldl (\mp { edge, stat } -> M.insert edge stat mp) st.edges (catMaybes $ leftChildren <> rightChildren) }
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

    leftChildren = map (\edge -> if S.member edge explainedEdges.ls then Nothing else Just { edge, stat: ESNotUsed }) $ transEdges childl.trans

    rightChildren = map (\edge -> if S.member edge explainedEdges.rs then Nothing else Just { edge, stat: ESNotUsed }) $ transEdges childr.trans

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
          when (not $ isRepeatingEdge edge)
            $ ST.modify_ \st -> st { edges = M.insert edge ESNotRepetition st.edges }
    -- return new agenda items
    pure $ nothingMore <$> childl : childm : attachSegment childr seg2.rslice : Nil

validateReduction :: Reduction -> Validation
validateReduction red = flip ST.execState emptyVal $ walkGraph validationAlg red.start agenda
  where
  emptyVal = { notes: M.empty, edges: M.empty, slices: M.empty }

  agenda = nothingMore <$> red.segments

-- instances
-- ---------
-- NoteError
derive instance eqNoteError :: Eq NoteError

derive instance genericNoteError :: Generic NoteError _

instance showNoteError :: Show NoteError where
  show ns = genericShow ns

-- EdgeError
derive instance eqEdgeError :: Eq EdgeError

derive instance genericEdgeError :: Generic EdgeError _

instance showEdgeError :: Show EdgeError where
  show ns = genericShow ns

-- HoriError
-- derive instance eqHoriError :: Eq HoriError
-- derive instance genericHoriError :: Generic HoriError _
-- instance showHoriError :: Show HoriError where
--   show ns = genericShow ns
-- SliceError
derive instance eqSliceError :: Eq SliceError

derive instance genericSliceError :: Generic SliceError _

instance showSliceError :: Show SliceError where
  show ns = genericShow ns
