module App.Common where

import Prelude
import Data.Generic.Rep (class Generic)
import Data.List as L
import Data.Maybe (Maybe(..), maybe)
import Data.Pitches (SPitch)
import Data.Show.Generic (genericShow)
import Halogen as H
import ProtoVoices.Model (Model, Note, NoteExplanation, Parents(..), Piece, SliceId, StartStop(..), TransId, setHoriExplParent, setLeftExplParent, setRightExplParent)
import Web.DOM.Element (Element)
import Web.UIEvent.KeyboardEvent (KeyboardEvent)
import Web.UIEvent.WheelEvent (WheelEvent)

data Selection
  = SelNone
  | SelSlice SliceId
  | SelTrans TransId
  | SelNote { note :: Note, expl :: NoteExplanation, parents :: Parents SliceId }

type AppState
  = { selected :: Selection
    , model :: Maybe Model
    , name :: String
    , undoStack :: L.List { m :: Model, name :: String }
    , redoStack :: L.List { m :: Model, name :: String }
    , tab :: Maybe Tab
    , settings :: AppSettings
    , scoreElt :: Maybe Element
    }

type AppSlots
  = ( exportTab :: forall query. H.Slot query Void Int
    , importTab :: forall query. H.Slot query ImportOutput Int
    , settingsTab :: forall query. H.Slot query AppSettings Int
    )

derive instance eqOuterSelection :: Eq Selection

derive instance genericOuterSelection :: Generic Selection _

instance showOuterSelection :: Show Selection where
  show os = genericShow os

getSelSlice :: Selection -> Maybe SliceId
getSelSlice (SelSlice sid) = Just sid

getSelSlice _ = Nothing

getSelTrans :: Selection -> Maybe TransId
getSelTrans (SelTrans tid) = Just tid

getSelTrans _ = Nothing

noteIsSelected :: Selection -> StartStop Note -> Boolean
noteIsSelected (SelNote sel) (Inner note) = sel.note.id == note.id

noteIsSelected _ _ = false

sliceSelected :: Selection -> Boolean
sliceSelected = case _ of
  SelSlice _ -> true
  _ -> false

transSelected :: Selection -> Boolean
transSelected = case _ of
  SelTrans _ -> true
  _ -> false

noteSelected :: Selection -> Boolean
noteSelected = case _ of
  SelNote _ -> true
  _ -> false

outerSelected :: Selection -> Boolean
outerSelected = case _ of
  SelSlice _ -> true
  SelTrans _ -> true
  _ -> false

addParentToNote :: Selection -> SliceId -> Note -> GraphAction
addParentToNote sel sliceId parNote
  | SelNote { note: selNote, parents, expl } <- sel = case parents of
    MergeParents { left, right }
      | sliceId == left
      , Just expl' <- setLeftExplParent selNote.pitch (Just parNote) expl -> setExpl expl'
      | sliceId == right
      , Just expl' <- setRightExplParent selNote.pitch (Just parNote) expl -> setExpl expl'
      | otherwise -> NoOp
    VertParent vslice
      | sliceId == vslice
      , Just expl' <- setHoriExplParent selNote.pitch (Just parNote) expl -> setExpl expl'
      | otherwise -> NoOp
    NoParents -> NoOp
    where
    setExpl e = SetNoteExplanation { noteId: selNote.id, expl: e }
  | otherwise = NoOp

removeParent :: Note -> NoteExplanation -> (SPitch -> Maybe Note -> NoteExplanation -> Maybe NoteExplanation) -> GraphAction
removeParent note expl setParent = maybe NoOp mkAction $ setParent note.pitch Nothing expl
  where
  mkAction e' = SetNoteExplanation { noteId: note.id, expl: e' }

data Tab
  = HelpTab
  | ImportTab
  | ExportTab
  | SettingsTab
  | DebugTab

derive instance eqTab :: Eq Tab

data ImportThing
  = ImportPiece Piece
  -- | ImportCurrentSurface
  | ImportModel Model

type ImportOutput
  = { name :: String, thing :: ImportThing }

type AppSettings
  = { flatHori :: Boolean
    , showAllEdges :: Boolean
    , showScore :: Boolean
    , xscale :: Number
    , yscale :: Number
    }

defaultSettings :: AppSettings
defaultSettings =
  { flatHori: true
  , showAllEdges: false
  , showScore: true
  , xscale: 0.0
  , yscale: 0.0
  }

data GraphAction
  = NoOp
  | Init
  | HandleImport ImportOutput
  | HandleSettings AppSettings
  | SwitchTab (Maybe Tab)
  | HandleKey KeyboardEvent
  | HandleScroll WheelEvent
  | Select Selection
  | MergeAtSelected
  | VertAtSelected
  | UnMergeAtSelected
  | UnVertAtSelected
  | CombineAny
  | RemoveAny
  | SetNoteExplanation { noteId :: String, expl :: NoteExplanation }
  | Undo
  | Redo
  | RegisterScoreElt Element
