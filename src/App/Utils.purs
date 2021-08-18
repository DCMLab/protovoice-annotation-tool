module App.Utils where

import Prelude
import Data.Array (sort)
import Data.Either (Either(..))
import Data.Foldable (class Foldable, intercalate)
import Data.Maybe (fromJust)
import Data.Pitches (alteration, letter, octaves)
import Effect (Effect)
import Foreign (ForeignError, renderForeignError)
import Partial.Unsafe (unsafePartial)
import ProtoVoices.Common (MBS)
import ProtoVoices.JSONTransport (PieceJSON, addJSONIds, pieceFromJSON)
import ProtoVoices.Model (Note)
import Web.DOM (Element)

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

showJSONErrors :: forall a f. Foldable f => Functor f => f ForeignError -> Either String a
showJSONErrors errs = Left $ "Errors parsing JSON:\n  " <> intercalate "\n  " (renderForeignError <$> errs)

foreign import copyToClipboard :: String -> Effect Unit

foreign import data DOMScore :: Type

-- foreign import drawNotes :: Array { name :: String, oct :: Int, accs :: Int } -> DOMScore
-- foreign import drawSystem :: Number -> DOMScore
foreign import drawScore :: Array { x :: Number, notes :: Array { name :: String, oct :: Int, accs :: Int } } -> Number -> Number -> DOMScore

foreign import insertScore :: Element -> DOMScore -> Effect Unit

renderScore :: Array { x :: Number, notes :: Array Note } -> Number -> Number -> DOMScore
renderScore slices = drawScore (mkSlice <$> slices)
  where
  mkSlice s = s { notes = pitchToVex <$> sort (_.pitch <$> s.notes) }

  pitchToVex p = { name: letter p, oct: octaves p, accs: alteration p }
