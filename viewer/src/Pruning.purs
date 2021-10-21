module Pruning where

import Prelude
import Control.Monad.State as ST
import Data.Either (Either(..))
import Data.List (List(..))
import ProtoVoices.Folding (AgendaAlg, nothingMore, walkGraph)
import ProtoVoices.Model (Model, Op(..), Reduction, Segment, attachSegment, detachSegment)

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
            ST.put (n - 1)
            res <- go (Cons s.childl (Cons (attachSegment s.childr seg2.rslice) rest2))
            case res of
              Cons childl' (Cons childr' rest2') -> pure (Cons seg1 (Cons (seg2 { op = Split { childl: childl', childr: detachSegment childr' } }) rest2'))
              _ -> ST.lift $ Left "invalid result of right split"
        _ -> do
          n <- ST.get
          if n <= 0 then
            Cons (seg1 { op = Freeze }) <$> go rest1
          else do
            ST.put (n - 1)
            res <- go (Cons h.childl (Cons h.childm (Cons (attachSegment h.childr seg2.rslice) rest2)))
            case res of
              Cons childl' (Cons childm' (Cons childr' rest2')) -> pure (Cons (seg1 { op = Hori { childl: childl', childm: childm', childr: detachSegment childr' } }) (Cons seg2 rest2'))
              _ -> ST.lift $ Left "invalid result of hori"

  split { childl, childr } seg rest = do
    n <- ST.get
    if n <= 0 then
      go (Cons (seg { op = Freeze }) rest)
    else do
      ST.put (n - 1)
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
