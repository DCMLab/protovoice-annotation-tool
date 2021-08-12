module App.Utils where

import Prelude
import Data.Either (Either(..))
import Data.Foldable (class Foldable, intercalate)
import Data.Maybe (fromJust)
import Effect (Effect)
import Foreign (ForeignError, renderForeignError)
import Partial.Unsafe (unsafePartial)
import ProtoVoices.Common (MBS)
import ProtoVoices.JSONTransport (PieceJSON, addJSONIds, pieceFromJSON)
import ProtoVoices.Model (Note)

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

showJSONErrors :: forall a f. Foldable f => Functor f => f ForeignError -> Either String a
showJSONErrors errs = Left $ "Errors parsing JSON:\n  " <> intercalate "\n  " (renderForeignError <$> errs)
