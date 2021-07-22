module JSONTransport where

import Prelude
import Data.Array (fromFoldable)
import Data.Either (Either(..))
import Data.Map as M
import Data.Maybe (Maybe(..))
import Data.Traversable (sequence)
import Data.Tuple (Tuple(..))
import Data.Variant (Variant, case_, inj, on)
import Folding (leftmostToReduction, reductionToLeftmost)
import Leftmost (FreezeOp(..), HoriChildren(..), HoriOp(..), Leftmost(..), RootOrnament(..), SplitOp(..))
import Model (DoubleOrnament(..), Edge, Edges, LeftOrnament(..), Note, Reduction, RightOrnament(..), SliceId(..), StartStop, TransId(..), Transition)
import Type.Prelude (Proxy(..))

----------
-- JSON --
----------
type ReductionJSON
  = { derivation :: Array LeftmostJSON
    , start :: SliceJSON
    , topSegments :: Array { trans :: Transition, rslice :: SliceJSON }
    }

type SliceJSON
  = { id :: SliceId, notes :: StartStop (Array Note) }

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
  = { regular :: Array { parent :: Edge, children :: Array { child :: Note, orn :: Maybe String } }
    , passing :: Array { parent :: Edge, children :: Array { child :: Note, orn :: Maybe String } }
    , fromLeft :: Array { parent :: Note, children :: Array { child :: Note, orn :: Maybe String } }
    , fromRight :: Array { parent :: Note, children :: Array { child :: Note, orn :: Maybe String } }
    , unexplained :: Array Note
    , keepLeft :: Array Edge
    , keepRight :: Array Edge
    , ids :: { left :: TransId, slice :: SliceId, right :: TransId }
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
                , tooManyChildren :: { left :: Array Note, right :: Array Note }
                )
          }
    , unexplained :: { left :: Array Note, right :: Array Note }
    , midEdges :: Edges
    , ids :: { left :: TransId, lslice :: SliceId, mid :: TransId, rslice :: SliceId, right :: TransId }
    }

-- encoding JSON
-- -------------
reductionToJSON :: Reduction -> ReductionJSON
reductionToJSON red =
  { derivation: leftmostToJSON <$> reductionToLeftmost red
  , start: sliceToJSON red.start
  , topSegments: (\{ trans, rslice } -> { trans, rslice: sliceToJSON rslice }) <$> fromFoldable red.segments
  }
  where
  sliceToJSON { id, notes } = { id, notes: map _.note <$> notes }

leftmostToJSON :: Leftmost SplitOp FreezeOp HoriOp -> LeftmostJSON
leftmostToJSON = case _ of
  LMFreezeLeft f -> inj (Proxy :: Proxy "freezeLeft") $ freezeToJSON f
  LMFreezeOnly f -> inj (Proxy :: Proxy "freezeOnly") $ freezeToJSON f
  LMSplitLeft s -> inj (Proxy :: Proxy "splitLeft") $ splitToJSON s
  LMSplitOnly s -> inj (Proxy :: Proxy "splitOnly") $ splitToJSON s
  LMSplitRight s -> inj (Proxy :: Proxy "splitRight") $ splitToJSON s
  LMHorizontalize h -> inj (Proxy :: Proxy "hori") $ horiToJSON h

freezeToJSON :: FreezeOp -> FreezeJSON
freezeToJSON (FreezeOp ties) = ties

splitToJSON :: SplitOp -> SplitJSON
splitToJSON (SplitOp split) =
  { regular: unwrap regToJSON <$> M.toUnfoldable split.regular
  , passing: unwrap (map show) <$> M.toUnfoldable split.passing
  , fromLeft: unwrap (map show) <$> M.toUnfoldable split.fromLeft
  , fromRight: unwrap (map show) <$> M.toUnfoldable split.fromRight
  , unexplained: split.unexplained
  , keepLeft: split.keepLeft
  , keepRight: split.keepRight
  , ids: split.ids
  }
  where
  unwrap ::
    forall o p s.
    (o -> s) ->
    Tuple p (Array { child :: Note, orn :: o }) ->
    { parent :: p, children :: Array { child :: Note, orn :: s } }
  unwrap f (Tuple parent children) =
    { parent
    , children: (\{ child, orn } -> { child, orn: f orn }) <$> children
    }

  regToJSON = case _ of
    Left RootNote -> Just "RootNote"
    Right o -> show <$> o

horiToJSON :: HoriOp -> HoriJSON
horiToJSON (HoriOp { midEdges, children, ids, unexplained }) =
  { midEdges
  , children: childToJSON <$> M.toUnfoldable children
  , ids
  , unexplained
  }
  where
  { left: TransId left, lslice: SliceId lslice, mid: TransId mid, rslice: SliceId rslice, right: TransId right } = ids

  childToJSON (Tuple parent dist) =
    let
      child = case dist of
        LeftChild l -> inj (Proxy :: Proxy "leftChild") l
        RightChild r -> inj (Proxy :: Proxy "rightChild") r
        BothChildren b -> inj (Proxy :: Proxy "bothChildren") b
        TooManyChildren t -> inj (Proxy :: Proxy "tooManyChildren") t
    in
      { parent, child }

-- decoding JSON
-- -------------
reductionFromJSON :: ReductionJSON -> Either String Reduction
reductionFromJSON { topSegments, derivation } = do
  deriv <- sequence $ leftmostFromJSON <$> derivation
  leftmostToReduction topSegments deriv

leftmostFromJSON :: LeftmostJSON -> Either String (Leftmost SplitOp FreezeOp HoriOp)
leftmostFromJSON =
  case_
    # on (Proxy :: Proxy "freezeLeft") (map LMFreezeLeft <<< freezeFromJSON)
    # on (Proxy :: Proxy "freezeOnly") (map LMFreezeOnly <<< freezeFromJSON)
    # on (Proxy :: Proxy "splitLeft") (map LMSplitLeft <<< splitFromJSON)
    # on (Proxy :: Proxy "splitOnly") (map LMSplitOnly <<< splitFromJSON)
    # on (Proxy :: Proxy "splitRight") (map LMSplitRight <<< splitFromJSON)
    # on (Proxy :: Proxy "hori") (map LMHorizontalize <<< horiFromJSON)

freezeFromJSON :: FreezeJSON -> Either String FreezeOp
freezeFromJSON = Right <<< FreezeOp

splitFromJSON :: SplitJSON -> Either String SplitOp
splitFromJSON json@{ unexplained, keepLeft, keepRight, ids } = do
  regular <- M.fromFoldable <$> (sequence $ wrap readDoubleOrnament <$> json.regular)
  passing <- M.fromFoldable <$> (sequence $ wrap readPassingOrnament <$> json.passing)
  fromLeft <- M.fromFoldable <$> (sequence $ wrap readRightOrnament <$> json.fromLeft)
  fromRight <- M.fromFoldable <$> (sequence $ wrap readLeftOrnament <$> json.fromRight)
  pure
    $ SplitOp
        { regular
        , passing
        , fromLeft
        , fromRight
        , unexplained
        , keepLeft
        , keepRight
        , ids
        }
  where
  wrap ::
    forall a p s.
    (s -> Either String a) ->
    { parent :: p, children :: Array { child :: Note, orn :: s } } ->
    Either String (Tuple p (Array { child :: Note, orn :: a }))
  wrap read { parent, children } =
    let
      children' = sequence $ (\{ child, orn } -> { child, orn: _ } <$> read orn) <$> children
    in
      Tuple parent <$> children'

  readDoubleOrnament = case _ of
    Just "RootNote" -> Right $ Left RootNote
    Just "FullRepeat" -> Right $ Right $ Just FullRepeat
    Just "FullNeighbor" -> Right $ Right $ Just FullNeighbor
    Just "LeftRepeatOfRight" -> Right $ Right $ Just LeftRepeatOfRight
    Just "RightRepeatOfLeft" -> Right $ Right $ Just RightRepeatOfLeft
    Just other -> Left $ "Expected double ornament type but got " <> other
    Nothing -> Right $ Right Nothing

  readPassingOrnament = case _ of
    Just "PassingMid" -> Right $ Just PassingMid
    Just "PassingLeft" -> Right $ Just PassingLeft
    Just "PassingRight" -> Right $ Just PassingRight
    Just other -> Left $ "Expected passing ornament type but got " <> other
    Nothing -> Right Nothing

  readRightOrnament = case _ of
    Just "RightNeighbor" -> Right $ Just RightNeighbor
    Just "RightRepeat" -> Right $ Just RightRepeat
    Just other -> Left $ "Expected right ornament type but got " <> other
    Nothing -> Right Nothing

  readLeftOrnament = case _ of
    Just "LeftNeighbor" -> Right $ Just LeftNeighbor
    Just "LeftRepeat" -> Right $ Just LeftRepeat
    Just other -> Left $ "Expected left ornament type but got " <> other
    Nothing -> Right Nothing

horiFromJSON :: HoriJSON -> Either String HoriOp
horiFromJSON { midEdges, children, ids, unexplained } =
  Right
    $ HoriOp
        { midEdges
        , children: M.fromFoldable (childFromJSON <$> children)
        , ids
        , unexplained
        }
  where
  getDist =
    case_
      # on (Proxy :: Proxy "leftChild") LeftChild
      # on (Proxy :: Proxy "rightChild") RightChild
      # on (Proxy :: Proxy "bothChildren") BothChildren
      # on (Proxy :: Proxy "tooManyChildren") TooManyChildren

  childFromJSON { parent, child } = Tuple parent (getDist child)
