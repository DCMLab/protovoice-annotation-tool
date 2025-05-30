module App.Utils where

import Prelude

import Control.Promise (Promise, toAffE)
import Data.Either (Either(..))
import Data.Foldable (class Foldable, intercalate)
import Data.Maybe (fromJust)
import Effect (Effect)
import Effect.Aff (Aff)
import Foreign (Foreign, ForeignError, renderForeignError, unsafeToForeign)
import Partial.Unsafe (unsafePartial)
import ProtoVoices.Common (MBS)
import ProtoVoices.JSONTransport (PieceJSON, addJSONIds, pieceFromJSON)
import ProtoVoices.Model (Note)
import Web.Event.EventTarget (EventTarget)

foreign import examplePieceJSON :: PieceJSON ()

foreign import examplePieceJSONLong :: PieceJSON ()

examplePieceJSONWithIds :: PieceJSON (id :: String)
examplePieceJSONWithIds = addJSONIds examplePieceJSON

examplePieceJSONLongWithIds :: PieceJSON (id :: String)
examplePieceJSONLongWithIds = addJSONIds examplePieceJSONLong

examplePiece :: Array { time :: Either String MBS, notes :: Array { hold :: Boolean, note :: Note } }
examplePiece = unsafePartial $ fromJust $ pieceFromJSON examplePieceJSONWithIds

examplePieceLong :: Array { time :: Either String MBS, notes :: Array { hold :: Boolean, note :: Note } }
examplePieceLong = unsafePartial $ fromJust $ pieceFromJSON examplePieceJSONLongWithIds

showJSONErrors :: forall a f. Foldable f => Functor f => f ForeignError -> Either String a
showJSONErrors errs = Left $ "Errors parsing JSON:\n  " <> intercalate "\n  " (renderForeignError <$> errs)

foreign import copyToClipboard :: String -> Effect Unit

foreign import download_ :: Foreign -> String -> String -> Effect Boolean

download :: String -> String -> String -> Effect Boolean
download str filename mimetype = download_ (unsafeToForeign str) filename mimetype

-- TODO: better error handling
foreign import musicxml2pv :: (forall a b. a -> Either a b) -> (forall a b. b -> Either a b) -> Boolean -> String -> Effect (Promise (Either String String))

convertMusicXML :: Boolean -> String -> Aff (Either String String)
convertMusicXML unfold input = do
  result <- toAffE $ musicxml2pv Left Right unfold input
  pure $ result

foreign import eventTargetIsBody :: EventTarget -> Boolean
