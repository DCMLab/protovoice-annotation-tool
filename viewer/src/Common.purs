module Common where

import Prelude
import Control.Monad.State as ST
import Data.Either (Either(..))
import Data.Map as M
import Data.Maybe (Maybe(..))
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import ProtoVoices.Folding (Graph, evalGraph)
import ProtoVoices.Model (Model, Note, NoteExplanation, StartStop(..))
import Pruning (Surface, findSurface, pruneModel)
import Web.DOM.Element (Element)

class_ :: forall r i. String -> HH.IProp ( class :: String | r ) i
class_ str = HP.class_ $ HH.ClassName str

data ViewerAction
  = Init
  | NoOp
  | Select Selection
  | Forward
  | Backward
  | ToFirst
  | ToLast
  | RegisterScoreElt Element
  | ToggleSettings
  | ToggleInner
  | ToggleOuter
  | ToggleScore
  | SetXScale String
  | SetYScale String

type AppSettings
  = { flatHori :: Boolean
    , xscale :: Number
    , yscale :: Number
    , showSettings :: Boolean
    , showInner :: Boolean
    , showOuter :: Boolean
    , showScore :: Boolean
    }

defaultSettings :: AppSettings
defaultSettings =
  { flatHori: true
  , xscale: 0.0
  , yscale: 0.0
  , showSettings: false
  , showInner: true
  , showOuter: true
  , showScore: true
  }

type Selection
  = Maybe { note :: Note, expl :: NoteExplanation }

noteIsSelected :: Selection -> StartStop Note -> Boolean
noteIsSelected (Just sel) (Inner note) = sel.note.id == note.id

noteIsSelected _ _ = false

type ViewerCache
  = { modelPruned :: M.Map Int Model
    , graph :: M.Map Int Graph
    , surface :: M.Map Int Surface
    }

emptyCache :: ViewerCache
emptyCache = { modelPruned: M.empty, graph: M.empty, surface: M.empty }

fillCache :: Model -> Int -> ViewerCache -> ViewerCache
fillCache model step cache =
  flip ST.evalState Nothing do
    pruned' <- insertItem identity cache.modelPruned
    graph' <- insertItem (evalGraph true true <<< _.reduction) cache.graph
    surface' <- insertItem (findSurface <<< _.reduction) cache.surface
    pure { modelPruned: pruned', graph: graph', surface: surface' }
  where
  insertItem :: forall a. (Model -> a) -> M.Map Int a -> ST.State (Maybe (Either String Model)) (M.Map Int a)
  insertItem f place =
    if M.member step place then
      pure place
    else do
      savedMPruned <- ST.get
      mpruned <- case savedMPruned of
        Nothing -> do
          let
            mp = pruneModel step model
          ST.put $ Just mp
          pure mp
        Just mp -> pure mp
      case mpruned of
        Left err -> pure place -- can't insert
        Right mp -> pure $ M.insert step (f mp) place

cacheGetPruned :: Model -> Int -> ViewerCache -> Either String Model
cacheGetPruned model step cache = case M.lookup step cache.modelPruned of
  Just mp -> Right mp
  Nothing -> pruneModel step model

cacheGetGraph :: Model -> Int -> ViewerCache -> Graph
cacheGetGraph mpruned step cache = case M.lookup step cache.graph of
  Just g -> g
  Nothing -> evalGraph true true mpruned.reduction

cacheGetSurface :: Model -> Int -> ViewerCache -> Surface
cacheGetSurface mpruned step cache = case M.lookup step cache.surface of
  Just surfs -> surfs
  Nothing -> findSurface mpruned.reduction
