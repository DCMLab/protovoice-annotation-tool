module Utils where

import Prelude
import Data.Maybe (fromJust)
import Model (pieceFromJSON, Piece)
import Partial.Unsafe (unsafePartial)

foreign import examplePieceJSON :: Array (Array { pitch :: String, hold :: Boolean })

examplePiece :: Piece
examplePiece = unsafePartial $ fromJust $ pieceFromJSON examplePieceJSON
