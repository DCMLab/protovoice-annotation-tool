module Leftmost where

import Prelude
import Data.Generic.Rep (class Generic)
import Data.Show.Generic (genericShow)

data Leftmost s f h
  = LMSplitLeft s
  | LMFreezeLeft f
  | LMSplitRight s
  | LMHorizontalize h
  | LMSplitOnly s
  | LMFreezeOnly f

derive instance genericLeftmost :: Generic (Leftmost s f h) _

instance showLeftmost :: (Show s, Show f, Show h) => Show (Leftmost s f h) where
  show lm = genericShow lm
