module Utils where

import Prelude
import Data.Array as A
import Data.Pitches (alteration, letter, octaves)
import Effect (Effect)
import ProtoVoices.Model (Note)
import Web.DOM.Element (Element)

foreign import data DOMScore :: Type

foreign import drawScore :: Array { x :: Number, notes :: Array { name :: String, oct :: Int, accs :: Int } } -> Number -> Number -> DOMScore

foreign import insertScore :: Element -> DOMScore -> Effect Unit

renderScore :: Array { x :: Number, notes :: Array Note } -> Number -> Number -> DOMScore
renderScore slices = drawScore (mkSlice <$> slices)
  where
  mkSlice s = s { notes = pitchToVex <$> A.sort (_.pitch <$> s.notes) }

  pitchToVex p = { name: letter p, oct: octaves p, accs: alteration p }
