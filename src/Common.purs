module Common where

import Prelude
import Data.Generic.Rep (class Generic)
import Data.Maybe (Maybe(..))
import Data.Show.Generic (genericShow)
import Model (Piece, SliceId, TransId, StartStop(..), Note)

data Selection
  = SelNone
  | SelSlice SliceId
  | SelTrans TransId
  | SelNote { note :: String, parentSlices :: Array SliceId }

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
noteIsSelected (SelNote sel) (Inner note) = sel.note == note.id

noteIsSelected _ _ = false

data GraphActions
  = Select Selection
  | LoadPiece Piece
  | MergeAtSelected
  | VertAtSelected
  | UnMergeAtSelected
  | UnVertAtSelected
