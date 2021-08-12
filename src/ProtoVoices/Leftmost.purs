module ProtoVoices.Leftmost where

import Prelude
import Data.Array (sortWith)
import Data.Array as A
import Data.Either (Either(..))
import Data.Generic.Rep (class Generic)
import Data.Map as M
import Data.Maybe (Maybe)
import Data.Show.Generic (genericShow)
import Data.Traversable (sequence)
import Data.Tuple (Tuple(..))
import ProtoVoices.Model (DoubleOrnament, Edge, Edges, LeftOrnament, Note, NoteExplanation(..), Notes, RightOrnament, SliceId, StartStop(..), TransId, Time)

----------------
-- operations --
----------------
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

data RootOrnament
  = RootNote

newtype SplitOp
  = SplitOp
  { regular :: M.Map Edge (Array { child :: Note, orn :: Either RootOrnament (Maybe DoubleOrnament) })
  , passing :: M.Map Edge (Array { child :: Note, orn :: Maybe DoubleOrnament })
  , fromLeft :: M.Map Note (Array { child :: Note, orn :: Maybe RightOrnament })
  , fromRight :: M.Map Note (Array { child :: Note, orn :: Maybe LeftOrnament })
  , unexplained :: Array Note
  , keepLeft :: Array Edge
  , keepRight :: Array Edge
  , ids :: { left :: TransId, slice :: SliceId, right :: TransId }
  }

leftEdges :: forall o. M.Map Edge (Array { child :: Note | o }) -> Array Edge
leftEdges splits = do
  Tuple parent children <- M.toUnfoldable splits
  child <- children
  pure { left: parent.left, right: Inner child.child }

rightEdges :: forall o. M.Map Edge (Array { child :: Note | o }) -> Array Edge
rightEdges splits = do
  Tuple parent children <- M.toUnfoldable splits
  child <- children
  pure { left: Inner child.child, right: parent.right }

extractChildNotes ::
  forall p o.
  M.Map p (Array { child :: Note, orn :: o }) ->
  (p -> o -> Either String NoteExplanation) ->
  Either String Notes
extractChildNotes splits f =
  sequence do
    Tuple parent children <- M.toUnfoldable splits
    child <- children
    pure case f parent child.orn of
      Left err -> Left err
      Right expl -> Right { note: child.child, expl }

splitGetChildNotes :: SplitOp -> Either String Notes
splitGetChildNotes (SplitOp s) = do
  regular <-
    extractChildNotes s.regular \p o -> case o of
      Left RootNote -> Right RootExpl
      Right orn -> case p.left of
        Inner leftParent -> case p.right of
          Inner rightParent -> Right $ DoubleExpl { leftParent, rightParent, orn }
          _ -> Left "Parent note cannot be Start or Stop!"
        _ -> Left "Parent note cannot be Start or Stop!"
  passing <-
    extractChildNotes s.passing \p orn -> case p.left of
      Inner leftParent -> case p.right of
        Inner rightParent -> Right $ DoubleExpl { leftParent, rightParent, orn }
        _ -> Left "Parent note cannot be Start or Stop!"
      _ -> Left "Parent note cannot be Start or Stop!"
  fromLeft <- extractChildNotes s.fromLeft \p orn -> Right $ RightExpl { leftParent: p, orn }
  fromRight <- extractChildNotes s.fromRight \p orn -> Right $ LeftExpl { rightParent: p, orn }
  let
    unexplained = map (\note -> { note, expl: NoExpl }) s.unexplained
  pure $ sortWith _.note $ regular <> passing <> fromLeft <> fromRight <> unexplained

newtype FreezeOp
  = FreezeOp { ties :: Array Edge, prevTime :: Time }

data HoriChildren
  = LeftChild Note
  | RightChild Note
  | BothChildren { left :: Note, right :: Note }
  | TooManyChildren { left :: Array Note, right :: Array Note }

instance semigroupHoriChildren :: Semigroup HoriChildren where
  append a b = case a of
    LeftChild l -> case b of
      LeftChild left -> TooManyChildren { left: [ l, left ], right: [] }
      RightChild right -> BothChildren { left: l, right }
      BothChildren { left, right } -> TooManyChildren { left: [ l, left ], right: [ right ] }
      TooManyChildren { left, right } -> TooManyChildren { left: A.cons l left, right }
    RightChild r -> case b of
      LeftChild left -> BothChildren { left, right: r }
      RightChild right -> TooManyChildren { left: [], right: [ r, right ] }
      BothChildren { left, right } -> TooManyChildren { left: [ left ], right: [ r, right ] }
      TooManyChildren { left, right } -> TooManyChildren { left, right: A.cons r right }
    BothChildren { left: l, right: r } -> case b of
      LeftChild left -> TooManyChildren { left: [ l, left ], right: [ r ] }
      RightChild right -> TooManyChildren { left: [ l ], right: [ r, right ] }
      BothChildren { left, right } -> TooManyChildren { left: [ l, left ], right: [ r, right ] }
      TooManyChildren { left, right } -> TooManyChildren { left: A.cons l left, right: A.cons r right }
    TooManyChildren { left: l, right: r } -> case b of
      LeftChild left -> TooManyChildren { left: A.snoc l left, right: r }
      RightChild right -> TooManyChildren { left: l, right: A.snoc r right }
      BothChildren { left, right } -> TooManyChildren { left: A.snoc l left, right: A.snoc r right }
      TooManyChildren { left, right } -> TooManyChildren { left: l <> left, right: r <> right }

newtype HoriOp
  = HoriOp
  { children :: M.Map Note HoriChildren
  , unexplained :: { left :: Array Note, right :: Array Note }
  , midEdges :: Edges
  , ids :: { left :: TransId, lslice :: SliceId, mid :: TransId, rslice :: SliceId, right :: TransId }
  }

horiLeftChildren :: HoriOp -> Notes
horiLeftChildren (HoriOp { children, unexplained }) = sortWith _.note $ unexpl <> explained
  where
  unexpl = (\note -> { note, expl: NoExpl }) <$> unexplained.left

  explained = A.concatMap getChild $ M.toUnfoldable children

  getChild (Tuple parent dist) = case dist of
    LeftChild note -> [ { note, expl: HoriExpl parent } ]
    RightChild _ -> []
    BothChildren { left } -> [ { note: left, expl: HoriExpl parent } ]
    TooManyChildren { left } -> map { note: _, expl: HoriExpl parent } left

horiRightChildren :: HoriOp -> Notes
horiRightChildren (HoriOp { children, unexplained }) = sortWith _.note $ unexpl <> explained
  where
  unexpl = (\note -> { note, expl: NoExpl }) <$> unexplained.right

  explained = A.concatMap getChild $ M.toUnfoldable children

  getChild (Tuple parent dist) = case dist of
    RightChild note -> [ { note, expl: HoriExpl parent } ]
    LeftChild _ -> []
    BothChildren { right } -> [ { note: right, expl: HoriExpl parent } ]
    TooManyChildren { right } -> map { note: _, expl: HoriExpl parent } right

-- boring instances
-- ----------------
derive instance genericRootOrnament :: Generic RootOrnament _

instance showRootOrnament :: Show RootOrnament where
  show ro = genericShow ro

derive instance genericSplitOp :: Generic SplitOp _

instance showSplitOp :: Show SplitOp where
  show so = genericShow so

derive instance genericFreezeOp :: Generic FreezeOp _

instance showFreezeOp :: Show FreezeOp where
  show fo = genericShow fo

derive instance genericHoriChildren :: Generic HoriChildren _

instance showHoriChildren :: Show HoriChildren where
  show hc = genericShow hc

derive instance genericHoriOp :: Generic HoriOp _

instance showHoriOp :: Show HoriOp where
  show ho = genericShow ho
