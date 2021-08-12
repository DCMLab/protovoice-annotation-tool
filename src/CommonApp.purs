module CommonApp where

import Prelude
import Data.Generic.Rep (class Generic)
import Data.Maybe (Maybe(..), maybe)
import Data.Pitches (SPitch)
import Data.Show.Generic (genericShow)
import Model (Model, Note, NoteExplanation, Parents(..), Piece, SliceId, StartStop(..), TransId, setHoriExplParent, setLeftExplParent, setRightExplParent)
import Web.UIEvent.KeyboardEvent (KeyboardEvent)

data Selection
  = SelNone
  | SelSlice SliceId
  | SelTrans TransId
  | SelNote { note :: Note, expl :: NoteExplanation, parents :: Parents SliceId }

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

data ImportOutput
  = ImportPiece Piece
  | ImportModel Model

type AppSettings
  = { flatHori :: Boolean
    , xscale :: Number
    , yscale :: Number
    }

defaultSettings :: AppSettings
defaultSettings =
  { flatHori: true
  , xscale: 70.0
  , yscale: 60.0
  }

data GraphAction
  = NoOp
  | Init
  | HandleImport ImportOutput
  | HandleSettings AppSettings
  | SwitchTab (Maybe Tab)
  | HandleKey KeyboardEvent
  | Select Selection
  | MergeAtSelected
  | VertAtSelected
  | UnMergeAtSelected
  | UnVertAtSelected
  | CombineAny
  | RemoveAny
  | SetNoteExplanation { noteId :: String, expl :: NoteExplanation }
