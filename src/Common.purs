module Common where

import Prelude
import Model (Piece, SliceId, TransId)

data OuterSelection
  = SelNone
  | SelSlice SliceId
  | SelTrans TransId

derive instance eqOuterSelection :: Eq OuterSelection

data GraphActions
  = SelectOuter OuterSelection
  | LoadPiece Piece
  | MergeAtSelected
  | VertAtSelected
  | UnMergeAtSelected
  | UnVertAtSelected
