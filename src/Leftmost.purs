module Leftmost where

import Prelude
import Data.Either (Either(..))
import Data.Generic.Rep (class Generic)
import Data.Map as M
import Data.Show.Generic (genericShow)
import Data.Symbol (SProxy(..))
import Data.Tuple (Tuple(..))
import Data.Variant (Variant, inj)
import Model (DoubleOrnament, Edge, Edges, LeftOrnament, Note, RightOrnament)
import Simple.JSON (writeJSON)

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

----------------
-- operations --
----------------
data RootOrnament
  = RootNote

newtype SplitOp
  = SplitOp
  { regular :: M.Map Edge (Array { child :: Note, orn :: Either RootOrnament DoubleOrnament })
  , passing :: M.Map Edge (Array { child :: Note, orn :: DoubleOrnament })
  , fromLeft :: M.Map Note (Array { child :: Note, orn :: RightOrnament })
  , fromRight :: M.Map Note (Array { child :: Note, orn :: LeftOrnament })
  , keepLeft :: Array Edge
  , keepRight :: Array Edge
  }

newtype FreezeOp
  = FreezeOp { ties :: Array Edge }

data HoriChildren
  = LeftChild Note
  | RightChild Note
  | BothChildren { left :: Note, right :: Note }
  | TooManyChildren

instance semigroupHoriChildren :: Semigroup HoriChildren where
  append a b = case a of
    LeftChild left -> case b of
      RightChild right -> BothChildren { left, right }
      _ -> TooManyChildren
    RightChild right -> case b of
      LeftChild left -> BothChildren { left, right }
      _ -> TooManyChildren
    _ -> TooManyChildren

newtype HoriOp
  = HoriOp
  { children :: M.Map Note HoriChildren
  , midEdges :: Edges
  }

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

----------
-- JSON --
----------
type LeftmostJSON
  = Variant
      ( freezeLeft :: FreezeJSON
      , freezeOnly :: FreezeJSON
      , splitLeft :: SplitJSON
      , splitRight :: SplitJSON
      , splitOnly :: SplitJSON
      , hori :: HoriJSON
      )

type FreezeJSON
  = { ties :: Array Edge }

type SplitJSON
  = { regular :: Array { parent :: Edge, children :: Array { child :: Note, orn :: String } }
    , passing :: Array { parent :: Edge, children :: Array { child :: Note, orn :: String } }
    , fromLeft :: Array { parent :: Note, children :: Array { child :: Note, orn :: String } }
    , fromRight :: Array { parent :: Note, children :: Array { child :: Note, orn :: String } }
    , keepLeft :: Array Edge
    , keepRight :: Array Edge
    }

type HoriJSON
  = { children ::
        Array
          { parent :: Note
          , child ::
              Variant
                ( leftChild :: Note
                , rightChild :: Note
                , bothChildren :: { left :: Note, right :: Note }
                , invalidChildren :: String
                )
          }
    , midEdges :: Edges
    }

leftmostToJSON :: Leftmost SplitOp FreezeOp HoriOp -> LeftmostJSON
leftmostToJSON = case _ of
  LMFreezeLeft f -> inj (SProxy :: SProxy "freezeLeft") $ freezeToJSON f
  LMFreezeOnly f -> inj (SProxy :: SProxy "freezeOnly") $ freezeToJSON f
  LMSplitLeft s -> inj (SProxy :: SProxy "splitLeft") $ splitToJSON s
  LMSplitOnly s -> inj (SProxy :: SProxy "splitOnly") $ splitToJSON s
  LMSplitRight s -> inj (SProxy :: SProxy "splitRight") $ splitToJSON s
  LMHorizontalize h -> inj (SProxy :: SProxy "hori") $ horiToJSON h

freezeToJSON :: FreezeOp -> FreezeJSON
freezeToJSON (FreezeOp ties) = ties

splitToJSON :: SplitOp -> SplitJSON
splitToJSON (SplitOp split) =
  { regular: unwrap regToJSON <$> M.toUnfoldable split.regular
  , passing: unwrap show <$> M.toUnfoldable split.passing
  , fromLeft: unwrap show <$> M.toUnfoldable split.fromLeft
  , fromRight: unwrap show <$> M.toUnfoldable split.fromRight
  , keepLeft: split.keepLeft
  , keepRight: split.keepRight
  }
  where
  unwrap ::
    forall o p.
    (o -> String) ->
    Tuple p (Array { child :: Note, orn :: o }) ->
    { parent :: p, children :: Array { child :: Note, orn :: String } }
  unwrap f (Tuple parent children) =
    { parent
    , children: (\{ child, orn } -> { child, orn: f orn }) <$> children
    }

  regToJSON = case _ of
    Left RootNote -> "RootNote"
    Right o -> show o

horiToJSON :: HoriOp -> HoriJSON
horiToJSON (HoriOp { midEdges, children }) =
  { midEdges
  , children: childToJSON <$> M.toUnfoldable children
  }
  where
  childToJSON (Tuple parent dist) =
    let
      child = case dist of
        LeftChild l -> inj (SProxy :: SProxy "leftChild") l
        RightChild r -> inj (SProxy :: SProxy "rightChild") r
        BothChildren b -> inj (SProxy :: SProxy "bothChildren") b
        TooManyChildren -> inj (SProxy :: SProxy "invalidChildren") "invalid"
    in
      { parent, child }

exportLeftmost :: Leftmost SplitOp FreezeOp HoriOp -> String
exportLeftmost = leftmostToJSON >>> writeJSON
