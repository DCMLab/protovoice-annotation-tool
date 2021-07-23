module Utils where

import Prelude
import Common (MBS, parseTime)
import Data.Array (mapWithIndex)
import Data.Either (Either)
import Data.Maybe (Maybe, fromJust)
import Data.Pitches (parseNotation)
import Data.Traversable (for)
import Effect (Effect)
import Model (Note, Piece)
import Partial.Unsafe (unsafePartial)
import Simple.JSON as JSON

type JSONPiece n
  = Array { time :: String, notes :: Array { pitch :: String, hold :: Boolean | n } }

foreign import examplePieceJSON :: JSONPiece ()

foreign import examplePieceJSONLong :: JSONPiece ()

examplePieceJSONWithIds :: JSONPiece ( id :: String )
examplePieceJSONWithIds = addJSONIds examplePieceJSON

examplePieceJSONLongWithIds :: JSONPiece ( id :: String )
examplePieceJSONLongWithIds = addJSONIds examplePieceJSONLong

addJSONIds :: JSONPiece () -> JSONPiece ( id :: String )
addJSONIds piece = mapWithIndex (\s slice -> slice { notes = addids s slice.notes }) piece
  where
  addids s = mapWithIndex (\n note -> { id: "note" <> show s <> "." <> show n, pitch: note.pitch, hold: note.hold })

pieceFromJSON ::
  JSONPiece ( id :: String ) ->
  Maybe Piece
pieceFromJSON piece =
  for piece \slice -> do
    notes <-
      for slice.notes \note ->
        (\p -> { hold: note.hold, note: { pitch: p, id: note.id } }) <$> parseNotation note.pitch
    pure { time: parseTime slice.time, notes }

examplePiece :: Array { time :: Either String MBS, notes :: Array { hold :: Boolean, note :: Note } }
examplePiece = unsafePartial $ fromJust $ pieceFromJSON examplePieceJSONWithIds

examplePieceLong :: Array { time :: Either String MBS, notes :: Array { hold :: Boolean, note :: Note } }
examplePieceLong = unsafePartial $ fromJust $ pieceFromJSON examplePieceJSONLongWithIds

foreign import unsafeStringifyPretty :: forall a. a -> String

writeJSONPretty :: forall a. JSON.WriteForeign a => a -> String
writeJSONPretty = unsafeStringifyPretty <<< JSON.writeImpl

foreign import copyToClipboard :: String -> Effect Unit
