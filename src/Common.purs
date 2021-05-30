module Common where

import Data.Maybe (Maybe)
import Model (Piece)

data GraphActions
  = SelectSlice (Maybe Int)
  | LoadPiece Piece
