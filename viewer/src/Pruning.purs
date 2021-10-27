module Pruning where

import Prelude
import Control.Monad.State as ST
import Data.Array as A
import Data.Either (Either(..))
import Data.List (List(..))
import Data.Maybe (Maybe(..))
import Data.Set as S
import ProtoVoices.Folding (AgendaAlg, addUnusedEdgesLeft, addUnusedEdgesRight, nothingMore, walkGraph)
import ProtoVoices.Model (Edges, Model, NoteExplanation(..), Op(..), Reduction, Segment, Slice, StartStop(..), attachSegment, detachSegment, horiEdgesMid)

pruneModel :: Int -> Model -> Either String Model
pruneModel n model = do
  red' <- pruneReduction n model.reduction
  pure $ model { reduction = red' }

pruneReduction :: Int -> Reduction -> Either String Reduction
pruneReduction nsteps red = do
  segs' <- ST.evalStateT (go red.segments) nsteps
  pure $ red { segments = segs' }
  where
  go :: List Segment -> ST.StateT Int (Either String) (List Segment)
  go Nil = pure Nil

  go (Cons seg Nil) = case seg.op of
    Freeze -> pure $ Cons seg Nil
    Split s -> split s seg Nil
    Hori _ -> pure $ Cons seg Nil

  go (Cons seg1 rest1@(Cons seg2 rest2)) = case seg1.op of
    Freeze -> Cons seg1 <$> go rest1
    Split s -> split s seg1 rest1
    Hori h -> do
      case seg2.op of
        Split s -> do
          n <- ST.get
          if n <= 0 then
            Cons (seg1 { op = Freeze }) <$> Cons (seg2 { op = Freeze }) <$> go rest2
          else do
            ST.modify_ (_ - 1)
            res <- go $ Cons seg1 $ Cons s.childl $ Cons (attachSegment s.childr seg2.rslice) rest2
            case res of
              Cons seg1' (Cons childl' (Cons childr' rest2')) -> pure (Cons seg1' (Cons (seg2 { op = Split { childl: childl', childr: detachSegment childr' } }) rest2'))
              _ -> ST.lift $ Left "invalid result of right split"
        _ -> do
          n <- ST.get
          if n <= 0 then
            Cons (seg1 { op = Freeze }) <$> go rest1
          else do
            ST.modify_ (_ - 1)
            res <- go (Cons h.childl (Cons h.childm (Cons (attachSegment h.childr seg2.rslice) rest2)))
            case res of
              Cons childl' (Cons childm' (Cons childr' rest2')) -> pure (Cons (seg1 { op = Hori { childl: childl', childm: childm', childr: detachSegment childr' } }) (Cons seg2 rest2'))
              _ -> ST.lift $ Left "invalid result of hori"

  split { childl, childr } seg rest = do
    n <- ST.get
    if n <= 0 then
      go (Cons (seg { op = Freeze }) rest)
    else do
      ST.modify_ (_ - 1)
      res <- go (Cons childl (Cons (attachSegment childr seg.rslice) rest))
      case res of
        Cons childl' (Cons childr' rest') -> pure (Cons (seg { op = Split { childl: childl', childr: detachSegment childr' } }) rest')
        _ -> ST.lift $ Left "invalid result of split"

countSteps :: Reduction -> Int
countSteps red = flip ST.execState 0 $ walkGraph countingAlg red.start agenda
  where
  agenda = nothingMore <$> red.segments

  split _ ag { childl, childr } = do
    ST.modify_ (_ + 1)
    pure $ map nothingMore $ Cons childl $ Cons (attachSegment childr ag.seg.rslice) Nil

  splitRight s ag1 ag2 = split s ag2

  hori _ _ ag2 { childl, childm, childr } = do
    ST.modify_ (_ + 1)
    pure $ map nothingMore $ Cons childl $ Cons childm $ Cons (attachSegment childr ag2.seg.rslice) Nil

  countingAlg :: AgendaAlg Unit Int
  countingAlg =
    { init: \_ -> pure unit
    , freezeLeft: \_ _ -> pure unit
    , freezeOnly: \_ _ -> pure unit
    , splitLeft: split
    , splitOnly: split
    , splitRight
    , hori
    }

type Surface
  = { slices :: Array Slice, transs :: Array Edges }

horiEdgesLeft :: Edges -> Slice -> Edges
horiEdgesLeft edgesl slicel
  | Inner notes <- slicel.notes =
    { regular: S.catMaybes $ S.map replaceRight edgesl.regular
    , passing: A.catMaybes $ map replaceRight edgesl.passing
    }
    where
    replaceRight { left, right }
      | Inner rightNote <- right
      , Just sliceNote <-
          A.find
            ( \n -> case n.expl of
                HoriExpl parent -> parent.id == rightNote.id
                _ -> false
            )
            notes = Just { left, right: Inner sliceNote.note }
      | otherwise = Nothing
  | otherwise = edgesl

horiEdgesRight :: Slice -> Edges -> Edges
horiEdgesRight slicer edgesr
  | Inner notes <- slicer.notes =
    { regular: S.catMaybes $ S.map replaceLeft edgesr.regular
    , passing: A.catMaybes $ map replaceLeft edgesr.passing
    }
    where
    replaceLeft { left, right }
      | Inner leftNote <- left
      , Just sliceNote <-
          A.find
            ( \n -> case n.expl of
                HoriExpl parent -> parent.id == leftNote.id
                _ -> false
            )
            notes = Just { left: Inner sliceNote.note, right }
      | otherwise = Nothing
  | otherwise = edgesr

findSurface :: Reduction -> Surface
findSurface red = flip ST.execState { slices: [], transs: [] } $ walkGraph surfaceAlg red.start agenda
  where
  agenda = nothingMore <$> red.segments

  split ag { childl, childr } = pure $ map nothingMore $ Cons (addUnusedEdgesLeft childl) $ Cons (addUnusedEdgesRight childl.rslice childr') Nil
    where
    childr' = attachSegment childr ag.seg.rslice

  hori _ ag1 ag2 { childl, childm, childr } = pure $ map nothingMore $ Cons childl' $ Cons childm $ Cons childr' Nil
    where
    childl' = childl { trans { edges = horiEdgesLeft ag1.seg.trans.edges childl.rslice } }

    childm' = childm { trans { edges = horiEdgesMid childl.rslice childm.trans.edges childm.rslice } }

    childr' = (attachSegment childr ag2.seg.rslice) { trans { edges = horiEdgesRight childm.rslice ag2.seg.trans.edges } }

  freeze lslice ag = do
    ST.modify_ \st ->
      { slices: A.snoc st.slices ag.seg.rslice
      , transs: A.snoc st.transs ag.seg.trans.edges
      }

  surfaceAlg :: AgendaAlg Unit Surface
  surfaceAlg =
    { init: \s -> ST.modify_ \st -> st { slices = A.snoc st.slices s }
    , freezeLeft: freeze
    , freezeOnly: freeze
    , splitLeft: \_ -> split
    , splitOnly: \_ -> split
    , splitRight: \_ _ -> split
    , hori
    }
