module Common where

import Prelude
import Data.Maybe (Maybe(..))
import Model (Piece, SliceId, TransId)

data OuterSelection
  = SelNone
  | SelSlice SliceId
  | SelTrans TransId

derive instance eqOuterSelection :: Eq OuterSelection

getSelSlice :: OuterSelection -> Maybe SliceId
getSelSlice (SelSlice sid) = Just sid

getSelSlice _ = Nothing

getSelTrans :: OuterSelection -> Maybe TransId
getSelTrans (SelTrans tid) = Just tid

getSelTrans _ = Nothing

data GraphActions
  = SelectOuter OuterSelection
  | LoadPiece Piece
  | MergeAtSelected
  | VertAtSelected
  | UnMergeAtSelected
  | UnVertAtSelected
