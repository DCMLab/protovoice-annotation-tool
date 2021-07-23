module Utils where

import Prelude
import Common (MBS)
import Data.Either (Either)
import Data.Maybe (fromJust)
import Effect (Effect)
import JSONTransport (PieceJSON, addJSONIds, pieceFromJSON)
import Model (Note)
import Partial.Unsafe (unsafePartial)

foreign import examplePieceJSON :: PieceJSON ()

foreign import examplePieceJSONLong :: PieceJSON ()

examplePieceJSONWithIds :: PieceJSON ( id :: String )
examplePieceJSONWithIds = addJSONIds examplePieceJSON

examplePieceJSONLongWithIds :: PieceJSON ( id :: String )
examplePieceJSONLongWithIds = addJSONIds examplePieceJSONLong

examplePiece :: Array { time :: Either String MBS, notes :: Array { hold :: Boolean, note :: Note } }
examplePiece = unsafePartial $ fromJust $ pieceFromJSON examplePieceJSONWithIds

examplePieceLong :: Array { time :: Either String MBS, notes :: Array { hold :: Boolean, note :: Note } }
examplePieceLong = unsafePartial $ fromJust $ pieceFromJSON examplePieceJSONLongWithIds

foreign import copyToClipboard :: String -> Effect Unit
