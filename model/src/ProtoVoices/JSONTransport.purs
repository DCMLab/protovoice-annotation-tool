module ProtoVoices.JSONTransport where

import Prelude
import Data.Array (fromFoldable, mapWithIndex)
import Data.Array as A
import Data.Either (Either(..), either)
import Data.Map as M
import Data.Maybe (Maybe(..))
import Data.Pitches (parseNotation)
import Data.Set as S
import Data.Traversable (for, sequence)
import Data.Tuple (Tuple(..))
import Data.Variant (Variant, case_, inj, on)
import ProtoVoices.Common (parseTime)
import ProtoVoices.Folding (leftmostToReduction, reductionToLeftmost)
import ProtoVoices.Leftmost (FreezeOp(..), HoriChildren(..), HoriOp(..), Leftmost(..), RootOrnament(..), SplitOp(..))
import ProtoVoices.Model (DoubleOrnament(..), Edge, Edges, LeftOrnament(..), Model, Note, Piece, RightOrnament(..), SliceId, StartStop, TransId, sortNotes)
import Simple.JSON as JSON
import Type.Proxy (Proxy(..))

----------
-- JSON --
----------
type PieceJSON n
  = Array { time :: String, notes :: Array { pitch :: String, hold :: Boolean | n } }

type ModelJSON
  = { derivation :: Array LeftmostJSON
    , start :: SliceJSON
    , topSegments :: Array { trans :: TransitionJSON, rslice :: SliceJSON }
    }

type TransitionJSON
  = { id :: TransId, edges :: EdgesJSON, is2nd :: Boolean }

type SliceJSON
  = { id :: SliceId, notes :: StartStop (Array Note) }

type EdgesJSON
  = { regular :: Array Edge, passing :: Array Edge }

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
  = { ties :: Array Edge
    , prevTime :: String
    }

type SplitJSON
  = { regular :: Array { parent :: Edge, children :: Array { child :: Note, orn :: Maybe String } }
    , passing :: Array { parent :: Edge, children :: Array { child :: Note, orn :: Maybe String } }
    , fromLeft :: Array { parent :: Note, children :: Array { child :: Note, orn :: Maybe String } }
    , fromRight :: Array { parent :: Note, children :: Array { child :: Note, orn :: Maybe String } }
    , unexplained :: Array Note
    , keepLeft :: Array Edge
    , keepRight :: Array Edge
    , passLeft :: Array Edge
    , passRight :: Array Edge
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
    , midEdges :: EdgesJSON
    , ids :: { left :: TransId, lslice :: SliceId, mid :: TransId, rslice :: SliceId, right :: TransId }
    }

-- encoding JSON
-- -------------
pieceToJSON :: Piece -> PieceJSON ( id :: String )
pieceToJSON = map \{ notes, time } -> { time: either identity show time, notes: noteToJSON <$> notes }
  where
  noteToJSON { note, hold } = { hold, id: note.id, pitch: show note.pitch }

modelToJSON :: Model -> Either String ModelJSON
modelToJSON model = do
  lm <- reductionToLeftmost model
  pure
    { derivation: leftmostToJSON <$> lm
    , start: sliceToJSON model.reduction.start
    , topSegments: (\{ trans, rslice } -> { trans: transToJSON trans, rslice: sliceToJSON rslice }) <$> fromFoldable model.reduction.segments
    }
  where
  sliceToJSON { id, notes } = { id, notes: map _.note <$> notes }

  transToJSON t = t { edges = edgesToJSON t.edges }

leftmostToJSON :: Leftmost SplitOp FreezeOp HoriOp -> LeftmostJSON
leftmostToJSON = case _ of
  LMFreezeLeft f -> inj (Proxy :: Proxy "freezeLeft") $ freezeToJSON f
  LMFreezeOnly f -> inj (Proxy :: Proxy "freezeOnly") $ freezeToJSON f
  LMSplitLeft s -> inj (Proxy :: Proxy "splitLeft") $ splitToJSON s
  LMSplitOnly s -> inj (Proxy :: Proxy "splitOnly") $ splitToJSON s
  LMSplitRight s -> inj (Proxy :: Proxy "splitRight") $ splitToJSON s
  LMHorizontalize h -> inj (Proxy :: Proxy "hori") $ horiToJSON h

freezeToJSON :: FreezeOp -> FreezeJSON
freezeToJSON (FreezeOp { ties, prevTime }) = { ties, prevTime: either identity show prevTime }

splitToJSON :: SplitOp -> SplitJSON
splitToJSON (SplitOp split@{ unexplained, keepLeft, keepRight, passLeft, passRight, ids }) =
  { regular: unwrap regToJSON <$> M.toUnfoldable split.regular
  , passing: unwrap (map show) <$> M.toUnfoldable split.passing
  , fromLeft: unwrap (map show) <$> M.toUnfoldable split.fromLeft
  , fromRight: unwrap (map show) <$> M.toUnfoldable split.fromRight
  , unexplained
  , keepLeft
  , keepRight
  , passLeft
  , passRight
  , ids
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
  { midEdges: edgesToJSON midEdges
  , children: childToJSON <$> M.toUnfoldable children
  , ids
  , unexplained
  }
  where
  childToJSON (Tuple parent dist) =
    let
      child = case dist of
        LeftChild l -> inj (Proxy :: Proxy "leftChild") l
        RightChild r -> inj (Proxy :: Proxy "rightChild") r
        BothChildren b -> inj (Proxy :: Proxy "bothChildren") b
        TooManyChildren t -> inj (Proxy :: Proxy "tooManyChildren") t
    in
      { parent, child }

edgesToJSON :: Edges -> EdgesJSON
edgesToJSON edges = { regular: A.fromFoldable edges.regular, passing: edges.passing }

-- decoding JSON
-- -------------
pieceFromJSON ::
  PieceJSON ( id :: String ) ->
  Maybe Piece
pieceFromJSON piece =
  for piece \slice -> do
    notes <-
      for slice.notes \note ->
        (\p -> { hold: note.hold, note: { pitch: p, id: note.id } }) <$> parseNotation note.pitch
    pure { time: parseTime slice.time, notes: sortNotes notes }

modelFromJSON :: ModelJSON -> Either String Model
modelFromJSON { topSegments, derivation } = do
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
freezeFromJSON { ties, prevTime } = Right $ FreezeOp { ties, prevTime: parseTime prevTime }

splitFromJSON :: SplitJSON -> Either String SplitOp
splitFromJSON json@{ unexplained, keepLeft, keepRight, passLeft, passRight, ids } = do
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
        , passLeft
        , passRight
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
        { midEdges: { regular: S.fromFoldable midEdges.regular, passing: midEdges.passing }
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

-------------
-- helpers --
-------------
addJSONIds :: PieceJSON () -> PieceJSON ( id :: String )
addJSONIds piece = mapWithIndex (\s slice -> slice { notes = addids s slice.notes }) piece
  where
  addids s = mapWithIndex (\n note -> { id: "note" <> show s <> "." <> show n, pitch: note.pitch, hold: note.hold })

stripJSONIds :: PieceJSON ( id :: String ) -> PieceJSON ()
stripJSONIds = map (\{ time, notes } -> { time, notes: map (\{ hold, pitch } -> { hold, pitch }) notes })

foreign import unsafeStringifyPretty :: forall a. a -> String

writeJSONPretty :: forall a. JSON.WriteForeign a => a -> String
writeJSONPretty = unsafeStringifyPretty <<< JSON.writeImpl
