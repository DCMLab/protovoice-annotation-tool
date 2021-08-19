module ProtoVoices.Validation where

import Prelude
import Control.Monad.State as ST
import Data.Array as A
import Data.Foldable (all, fold, foldl)
import Data.Generic.Rep (class Generic)
import Data.List (List(..), (:))
import Data.Map as M
import Data.Maybe (Maybe(..))
import Data.Set as S
import Data.Show.Generic (genericShow)
import Data.Traversable (for_)
import ProtoVoices.Folding (AgendaAlg, nothingMore, walkGraph)
import ProtoVoices.Model (DoubleOrnament(..), Edge, Note, NoteExplanation(..), Reduction, Slice, SliceId, StartStop(..), attachSegment, findDoubleOrn, findLeftOrn, findRightOrn, getInnerNotes, isRepeatingEdge, passingEdge, regularEdge)

data NoteError
  = NSNoExpl
  | NSInvalidExplanation

data EdgeError
  = ESNotUsed
  | ESNotStepwise
  | ESNotRepetition

data SliceError
  = SSInvalidReduction

type Validation
  = { notes :: M.Map String NoteError
    , edges :: M.Map Edge EdgeError
    , slices :: M.Map SliceId SliceError
    }

validationIsOk :: Validation -> Boolean
validationIsOk val = M.isEmpty val.notes && M.isEmpty val.edges && M.isEmpty val.slices

-- TODO: check that every hori parent note is pointed to at least once
validateSlice :: Slice -> Validation -> Validation
validateSlice slice st =
  st
    { notes = notes'
    }
  where
  notes' = case slice.notes of
    Inner slicenotes -> foldl (\mp { id, stat } -> M.insert id stat mp) st.notes $ A.catMaybes $ validateNote <$> slicenotes
    _ -> st.notes

validateNote :: { note :: Note, expl :: NoteExplanation } -> Maybe { id :: String, stat :: NoteError }
validateNote note = do
  stat <- err
  pure { id: note.note.id, stat }
  where
  err = case note.expl of
    NoExpl -> Just NSNoExpl
    RootExpl -> Nothing
    HoriExpl parent ->
      if note.note.pitch == parent.pitch then
        Nothing
      else
        Just NSInvalidExplanation
    LeftExpl expl -> case expl.orn of
      Nothing -> Just NSInvalidExplanation
      e ->
        if e == findLeftOrn note.note.pitch expl.rightParent then
          Nothing
        else
          Just NSInvalidExplanation
    RightExpl expl -> case expl.orn of
      Nothing -> Just NSInvalidExplanation
      e ->
        if e == findRightOrn note.note.pitch expl.leftParent then
          Nothing
        else
          Just NSInvalidExplanation
    DoubleExpl { orn, leftParent, rightParent } ->
      -- TODO: check presence of child passing edges for non-mid passings
      let
        predOrn = findDoubleOrn note.note.pitch leftParent rightParent
      in
        case orn of
          Nothing -> Just NSInvalidExplanation
          Just PassingLeft ->
            if (predOrn == Just PassingLeft || predOrn == Just PassingMid) then
              Nothing
            else
              Just NSInvalidExplanation
          Just PassingRight ->
            if (predOrn == Just PassingRight || predOrn == Just PassingMid) then
              Nothing
            else
              Just NSInvalidExplanation
          _ ->
            if predOrn == orn then
              Nothing
            else
              Just NSInvalidExplanation

validationAlg :: AgendaAlg Unit Validation
validationAlg = { init, freezeLeft, freezeOnly, splitLeft, splitOnly, splitRight, hori }
  where
  init start = pure unit

  freezeLeft _ { seg } = do
    ST.modify_ $ validateSlice seg.rslice

  freezeOnly = freezeLeft

  splitLeft _ { seg } { childl, childr } = do
    -- check if mandatory edges are used
    -- TODO: check for invalid (e.g. non-stepwise) edges too
    ST.modify_ \st -> st { edges = foldl (\mp { edge, stat } -> M.insert edge stat mp) st.edges allChildren }
    pure $ nothingMore <$> childl : attachSegment childr seg.rslice : Nil
    where
    -- collect all edges that are produced by some elaboration and thus "used"
    usedEdges =
      fold
        $ ( \{ note, expl } -> case expl of
              DoubleExpl { orn, leftParent, rightParent } ->
                let
                  ledge = { left: Inner leftParent, right: Inner note }

                  redge = { left: Inner note, right: Inner rightParent }
                in
                  case orn of
                    Just PassingLeft -> { ls: regularEdge ledge, rs: passingEdge redge }
                    Just PassingRight -> { ls: passingEdge ledge, rs: regularEdge redge }
                    Just _ -> { ls: regularEdge ledge, rs: regularEdge redge }
                    _ -> mempty
              LeftExpl { rightParent } -> { ls: mempty, rs: regularEdge { left: Inner note, right: Inner rightParent } }
              RightExpl { leftParent } -> { rs: mempty, ls: regularEdge { left: Inner leftParent, right: Inner note } }
              _ -> mempty
          )
        <$> getInnerNotes childl.rslice

    checkUsed used toCheck = S.map (\edge -> if S.member edge used then Nothing else Just { edge, stat: ESNotUsed }) toCheck

    leftChildrenReg = checkUsed usedEdges.ls.regular childl.trans.edges.regular

    rightChildrenReg = checkUsed usedEdges.rs.regular childr.trans.edges.regular

    allChildren = S.catMaybes $ leftChildrenReg <> rightChildrenReg

  splitOnly = splitLeft

  splitRight lSlice _ = splitLeft lSlice

  hori _ { seg: seg1 } { seg: seg2 } { childl, childm, childr } = do
    -- check slice and its notes
    ST.modify_ (validateSlice seg1.rslice)
    -- check validity of hori reduction
    when (not $ all isRepeatingEdge childm.trans.edges.regular)
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

derive instance ordEdgeError :: Ord EdgeError

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
