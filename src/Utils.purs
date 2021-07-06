module Utils where

import Prelude
import Common (MBS, parseMBS)
import Data.Array (mapWithIndex)
import Data.Maybe (Maybe, fromJust)
import Data.Traversable (for)
import Model (Note, Piece)
import Partial.Unsafe (unsafePartial)
import SimplePitch (parseSimplePitch)

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
    time <- parseMBS slice.time
    notes <-
      for slice.notes \note ->
        (\p -> { hold: note.hold, note: { pitch: p, id: note.id } }) <$> parseSimplePitch note.pitch
    pure { time, notes }

examplePiece :: Array { time :: MBS, notes :: Array { hold :: Boolean, note :: Note } }
examplePiece = unsafePartial $ fromJust $ pieceFromJSON examplePieceJSONWithIds

examplePieceLong :: Array { time :: MBS, notes :: Array { hold :: Boolean, note :: Note } }
examplePieceLong = unsafePartial $ fromJust $ pieceFromJSON examplePieceJSONLongWithIds
