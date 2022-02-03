module App.TikZ
  ( tikzOpDecor
  , tikzReduction
  ) where

import Prelude
import App.Common (Selection(..))
import Data.Array as A
import Data.Foldable as F
import Data.Map as M
import Data.Maybe (Maybe(..), fromMaybe)
import Data.String as S
import ProtoVoices.Folding (Graph, GraphTransition, GraphSlice)
import ProtoVoices.Model (DoubleOrnament(..), Edge, LeftOrnament(..), Note, NoteExplanation(..), RightOrnament(..), SliceId, StartStop(..))

tikzReduction :: Boolean -> Graph -> String
tikzReduction standalone { slices, transitions, horis } =
  if standalone then
    tikzPre <> tikz <> tikzPost
  else
    tikz
  where
  tSlices = F.intercalate "\n" $ map tikzSlice $ M.values slices

  tTranss = F.intercalate "\n" $ map (tikzTrans slices) $ M.values transitions

  tHoris = F.fold $ map (tikzHori slices) horis

  tikz = tSlices <> "\n" <> tHoris <> "\n" <> tTranss

tikzSlice :: GraphSlice -> String
tikzSlice { slice, depth } = case slice.notes of
  Start -> "\\node[slice] (start) at (0,0) {$\\rtimes$};\n"
  Stop -> "\\node[slice] (stop) at (" <> show slice.x <> ",0) {$\\ltimes$};\n"
  Inner notes ->
    let
      noteids = (\n -> idToName n.note.id) <$> notes

      tNotes = case A.uncons notes of
        Nothing -> ""
        Just { head, tail } ->
          tikzFirstNote depth slice.x head.note
            <> F.fold (A.zipWith tikzOtherNote noteids $ _.note <$> tail)

      tSlice = "{[on background layer] \\node[slice,fit={" <> F.intercalate " " (parens <$> noteids) <> "}] (" <> idToName (show slice.id) <> ") {};}\n"
    in
      tNotes <> tSlice

tikzFirstNote :: Number -> Number -> Note -> String
tikzFirstNote depth x note = "\\node[outernote] (" <> idToName note.id <> ") at (" <> show x <> ",-" <> show depth <> ") {" <> showTex note.pitch <> "};\n"

tikzOtherNote :: String -> Note -> String
tikzOtherNote pred note = "\\node[outernote,below=0 of " <> pred <> "] (" <> idToName note.id <> ") {" <> showTex note.pitch <> "};\n"

tikzTrans :: M.Map SliceId GraphSlice -> GraphTransition -> String
tikzTrans slices { left, right, edges } = tTrans <> F.fold tEdges
  where
  tEdges =
    map (tikzEdge "outeredge" left right) (A.fromFoldable edges.regular)
      <> map (tikzEdge "outerpass" left right) edges.passing

  tTrans =
    fromMaybe "" do
      l <- M.lookup left slices
      r <- M.lookup right slices
      pure $ "{[on background layer] \\draw[transition] "
        <> if l.depth == 0.0 && r.depth == 0.0 then
            "(" <> sliceName l.slice <> ".east |- 0,0) to (" <> sliceName r.slice <> ".west |- 0,0);}\n"
          else
            "(" <> sliceName l.slice <> ") to (" <> sliceName r.slice <> ");}\n"

  sliceName slice = case slice.notes of
    Start -> "start"
    Stop -> "stop"
    Inner _ -> idToName $ show slice.id

-- case A.head notes of
-- Just note -> parens $ idToName (show slice.id) <> "." <> (if isFirst then "east" else "west") <> " |- " <> idToName note.note.id
-- Nothing -> parens $ idToName $ show slice.id
tikzEdge :: String -> SliceId -> SliceId -> Edge -> String
tikzEdge style sleft sright { left, right } = "\\draw[" <> style <> "] (" <> nodeFrom <> ") to (" <> nodeTo <> "); % " <> show (_.pitch <$> left) <> " -> " <> show (_.pitch <$> right) <> "\n"
  where
  nodeFrom = case left of
    Start -> "start"
    Stop -> "stop"
    Inner note -> idToName (show sleft) <> ".east |- " <> idToName note.id

  nodeTo = case right of
    Start -> "start"
    Stop -> "stop"
    Inner note -> idToName (show sright) <> ".west |- " <> idToName note.id

tikzHori :: M.Map SliceId GraphSlice -> { child :: SliceId, parent :: SliceId } -> String
tikzHori slices { child, parent } =
  fromMaybe "" do
    c <- M.lookup child slices
    p <- M.lookup parent slices
    pure $ "\\draw[hori] " <> sliceName c <> " to " <> sliceName p <> ";\n"
  where
  sliceName { slice } = parens $ idToName $ show slice.id

idToName :: String -> String
idToName noteid = S.replaceAll (S.Pattern ".") (S.Replacement "_") noteid

parens :: String -> String
parens str = "(" <> str <> ")"

showTex :: forall a. (Show a) => a -> String
showTex = show >>> S.replaceAll (S.Pattern "♯") (S.Replacement "$\\sharp$") >>> S.replaceAll (S.Pattern "♭") (S.Replacement "$\\flat$")

tikzPre ∷ String
tikzPre =
  """\documentclass{standalone}
\usepackage{tikz,xcolor,amssymb}
\usetikzlibrary{positioning,fit,backgrounds,scopes,arrows.meta}
\definecolor{lightergray}{gray}{0.9}
\tikzset{slice/.style={rectangle,draw,fill=lightergray,semithick,minimum size=0.4cm,inner sep=3pt,align=center}}
\tikzset{transition/.style={semithick}}
\tikzset{non-terminal/.style={}}
\tikzset{terminal/.style={double}}
\tikzset{hori/.style={lightgray,dashed,thick}}
\tikzset{outeredge/.style={->,>=Stealth,out=0,in=180}}
\tikzset{outerpass/.style={outeredge,dashed}}
\tikzset{outernote/.style={inner xsep=0, inner ysep=2pt}}
\begin{document}

\begin{tikzpicture}[xscale=2,yscale=2]
"""

tikzPost ∷ String
tikzPost =
  """
\end{tikzpicture}

\end{document}
"""

tikzOpDecor :: Selection -> Maybe String
tikzOpDecor = case _ of
  SelNote { expl, slice, note } -> case tikzOpType expl of
    Just typ ->
      Just $ "\\opdecor{" <> typ <> "}{"
        <> idToName note.id
        <> "}{"
        <> show slice
        <> "} % "
        <> show note.pitch
    Nothing -> Nothing
  _ -> Nothing

tikzOpType :: NoteExplanation -> Maybe String
tikzOpType = case _ of
  RootExpl -> Just "root note"
  DoubleExpl { orn } -> case orn of
    Just FullNeighbor -> Just "full neighbor"
    Just FullRepeat -> Just "full repeat"
    Just LeftRepeatOfRight -> Just "left repeat of right"
    Just RightRepeatOfLeft -> Just "right repeat of left"
    Just PassingMid -> Just "passing mid"
    Just PassingLeft -> Just "passing left"
    Just PassingRight -> Just "passing right"
    Nothing -> Nothing
  RightExpl { orn } -> case orn of
    Just RightRepeat -> Just "right repeat"
    Just RightNeighbor -> Just "right neighbor"
    Nothing -> Nothing
  LeftExpl { orn } -> case orn of
    Just LeftRepeat -> Just "left repeat"
    Just LeftNeighbor -> Just "left neighbor"
    Nothing -> Nothing
  _ -> Nothing
