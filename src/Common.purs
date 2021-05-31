module Common where

import Prelude
import Model (Piece)

data OuterSelection
  = SelNone
  | SelSlice Int
  | SelTrans Int

derive instance eqOuterSelection :: Eq OuterSelection

data GraphActions
  = SelectOuter OuterSelection
  | LoadPiece Piece
  | MergeAtSelected
  | VertAtSelected
  | UnMergeAtSelected
  | UnVertAtSelected
