module Common where

import Prelude
import Data.Maybe (Maybe(..))
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import ProtoVoices.Model (Note, NoteExplanation, Parents, SliceId, StartStop(..))
import Web.DOM.Element (Element)

class_ :: forall r i. String -> HH.IProp ( class :: String | r ) i
class_ str = HP.class_ $ HH.ClassName str

data ViewerAction
  = Init
  | NoOp
  | Select Selection
  | Forward
  | Backward
  | RegisterScoreElt Element

type AppSettings
  = { flatHori :: Boolean
    , xscale :: Number
    , yscale :: Number
    }

defaultSettings :: AppSettings
defaultSettings =
  { flatHori: true
  , xscale: 0.0
  , yscale: 0.0
  }

type Selection
  = Maybe { note :: Note, expl :: NoteExplanation, parents :: Parents SliceId }

noteIsSelected :: Selection -> StartStop Note -> Boolean
noteIsSelected (Just sel) (Inner note) = sel.note.id == note.id

noteIsSelected _ _ = false
