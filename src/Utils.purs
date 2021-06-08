module Utils where

import Prelude
import Data.Array (mapWithIndex)
import Data.Maybe (Maybe, fromJust)
import Data.Traversable (for)
import Model (Note)
import Partial.Unsafe (unsafePartial)
import SimplePitch (parseSimplePitch)

foreign import examplePieceJSON :: Array (Array { pitch :: String, hold :: Boolean })

foreign import examplePieceJSONLong :: Array (Array { pitch :: String, hold :: Boolean })

examplePieceJSONWithIds :: Array (Array { hold :: Boolean, id :: String, pitch :: String })
examplePieceJSONWithIds = addJSONIds examplePieceJSON

examplePieceJSONLongWithIds :: Array (Array { hold :: Boolean, id :: String, pitch :: String })
examplePieceJSONLongWithIds = addJSONIds examplePieceJSONLong

addJSONIds ::
  Array (Array { pitch :: String, hold :: Boolean }) ->
  Array (Array { pitch :: String, hold :: Boolean, id :: String })
addJSONIds piece = mapWithIndex (\s slice -> mapWithIndex (\n note -> { id: "note" <> show s <> "." <> show n, pitch: note.pitch, hold: note.hold }) slice) piece

pieceFromJSON ::
  Array (Array { pitch :: String, hold :: Boolean, id :: String }) ->
  Maybe (Array (Array { note :: Note, hold :: Boolean }))
pieceFromJSON piece =
  for piece \slice ->
    for slice \note ->
      (\p -> { hold: note.hold, note: { pitch: p, id: note.id } }) <$> parseSimplePitch note.pitch

examplePiece :: Array (Array { hold :: Boolean, note :: Note })
examplePiece = unsafePartial $ fromJust $ pieceFromJSON examplePieceJSONWithIds

examplePieceLong :: Array (Array { hold :: Boolean, note :: Note })
examplePieceLong = unsafePartial $ fromJust $ pieceFromJSON examplePieceJSONLongWithIds
