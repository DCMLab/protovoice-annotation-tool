module ProtoVoices.RenderSVG where

import Prelude

import Data.Array as A
import Data.Either (Either(..))
import Data.Int (toNumber)
import Data.Map as M
import Data.Maybe (Maybe(..))
import Data.Nullable (Nullable)
import Data.Nullable as N
import Data.Pitches (alteration, letter, octaves)
import Data.Ratio ((%))
import Effect (Effect)
import ProtoVoices.Common (MBS(..))
import ProtoVoices.Folding (Graph)
import ProtoVoices.Model (Note, NoteExplanation(..), Piece, Slice, SliceId, StartStop(..), Time, BottomSurface, explParents, getInnerNotes)
import Web.DOM.Element (Element)

foreign import data DOMScore :: Type

type Selection = { note :: Note, expl :: NoteExplanation }

type RenderSlice =
  ( x :: Number
  , id :: SliceId
  , notes ::
      Array
        { id :: String
        , name :: String
        , oct :: Int
        , accs :: Int
        , sel :: Selection
        , expl :: { typ :: String, parent :: Nullable String }
        }
  )

foreign import drawScore :: Array (Record RenderSlice) -> Number -> Number -> Boolean -> DOMScore

foreign import insertScore :: Element -> DOMScore -> Effect Unit

renderScore
  :: Array Slice --  { x :: Number, id :: SliceId, notes :: Array { note :: Note, expl :: NoteExplanation } }
  -> (Number -> Number)
  -> Number
  -> Number
  -> Boolean
  -> DOMScore
renderScore slices toX = drawScore (mkRenderSlice toX <$> slices)

-- where
-- mkSlice s = s { notes = mkRenderSlice s.notes }

-- Rendering full graph

type RenderGraphSlice = { depth :: Number | RenderSlice }

type InnerEdge = { left :: Note, right :: Note }

type RenderGraphTransition = { regular :: Array InnerEdge, passing :: Array InnerEdge }

foreign import drawGraph
  :: { slices :: Array RenderGraphSlice
     , surfaceSlices :: Array (Record RenderSlice)
     , transitions :: Array RenderGraphTransition
     , surfaceTransitions :: Array RenderGraphTransition
     , horis :: Array { parent :: SliceId, child :: SliceId }
     , times :: Array { x :: Number, label :: String }
     , maxd :: Number
     , selection :: Nullable { note :: Note, parents :: Array Note }
     , select :: Nullable Selection -> Effect Unit
     }
  -> Number
  -> Number
  -> Boolean
  -> DOMScore

renderGraph :: Graph -> Piece -> BottomSurface -> Maybe Selection -> (Maybe Selection -> Effect Unit) -> (Number -> Number) -> Number -> Number -> Boolean -> DOMScore
renderGraph graph piece surface selection selectCallback toX = drawGraph
  { slices: A.fromFoldable $ mkGraphSlice <$> M.values graph.slices
  , surfaceSlices: mkRenderSlice toX <$> surface.slices
  , transitions: A.fromFoldable $ (mkTrans <<< _.edges) <$> M.values graph.transitions
  , surfaceTransitions: mkTrans <$> surface.transs
  , horis: A.fromFoldable graph.horis
  , times: A.mapWithIndex mkTime $ _.time <$> piece
  , maxd: graph.maxd
  , selection: case selection of
      Nothing -> N.null
      Just { note, expl } -> N.notNull { note, parents: explParents expl }
  , select: selectCallback <<< N.toMaybe
  }
  where
  -- toX x = scalex sett x - (noteSize / 2.0)
  mkGraphSlice s =
    { depth: s.depth
    , x: rs.x
    , id: rs.id
    , notes: rs.notes
    }
    where
    rs = mkRenderSlice toX s.slice
  mkTrans t =
    { regular: innerEdges $ A.fromFoldable t.regular
    , passing: innerEdges $ t.passing
    }
  mkTime i time = { x: toX $ toNumber (i + 1), label }
    where
    label = case time of
      Right (MBS { m, b, s }) -> if s == 0 % 1 then show m <> "." <> show b else ""
      Left str -> str

  innerEdges edges = A.catMaybes $
    ( \edge -> case edge of
        { left: Inner l, right: Inner r } -> Just { left: l, right: r }
        _ -> Nothing
    ) <$> edges

mkRenderSlice :: (Number -> Number) -> Slice -> Record RenderSlice
mkRenderSlice toX slice =
  { x: toX slice.x
  , id: slice.id
  , notes: noteToVex <$> A.sortWith _.note.pitch (getInnerNotes slice)
  }
  where
  noteToVex n =
    { id: n.note.id
    , name: letter n.note.pitch
    , oct: octaves n.note.pitch
    , accs: alteration n.note.pitch
    , sel: n
    , expl: encodeExplanation n.expl
    }

encodeExplanation :: NoteExplanation -> { typ :: String, parent :: Nullable String }
encodeExplanation expl = case expl of
  NoExpl -> { typ: "None", parent: N.null }
  RootExpl -> { typ: "Root", parent: N.null }
  HoriExpl n -> { typ: "Hori", parent: N.notNull n.id }
  RightExpl { orn } -> -- placed right of a left parent
    { typ: case orn of
        Nothing -> "RightNone"
        Just x -> show x
    , parent: N.null
    }
  LeftExpl { orn } -> -- placed left of a right parent
    { typ: case orn of
        Nothing -> "LeftNone"
        Just x -> show x
    , parent: N.null
    }
  DoubleExpl { orn } ->
    { typ: case orn of
        Nothing -> "DoubleNone"
        Just x -> show x
    , parent: N.null
    }
