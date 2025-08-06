module App.Tabs.Debug where

import Prelude

import App.Common (AppState, GraphAction, ModelInfo)
import App.Utils (class_)
import Data.Either (Either(..))
import Data.Hashable (hash)
import Data.Maybe (Maybe(..))
import Halogen.HTML as HH
import ProtoVoices.Folding (reductionToLeftmost)
import ProtoVoices.JSONTransport (pieceToJSON, stripJSONIds)
import ProtoVoices.Model (Model)
import Simple.JSON as JSON

-- debug component
-- ---------------
debugComponent :: forall p. AppState -> Maybe ModelInfo -> HH.HTML p GraphAction
debugComponent st modelInfo =
  HH.div [ class_ "content-np tab" ] $
    [ HH.h3_ [ HH.text "Selection" ]
    , HH.p_
        [ HH.text "Selection: "
        , HH.text $ show st.selected
        ]
    , HH.h3_ [ HH.text "Settings" ]
    , HH.p_ [ HH.text $ show st.settings ]
    , HH.h3_ [ HH.text "Undo / Redo" ]
    , HH.p_
        [ HH.p_ [ HH.text $ "Undo Stack: " <> show (showStackItem <$> st.undoStack) ]
        , HH.p_ [ HH.text $ "Redo Stack: " <> show (showStackItem <$> st.redoStack) ]
        ]
    , HH.p_
        [ HH.h3_ [ HH.text "Model" ]
        , HH.text $ case st.loaded of
            Nothing -> ""
            Just { model } -> show $ hash $ show model
        ]
    ] <> case modelInfo of
      Nothing -> [ HH.text "No analysis or piece loaded." ]
      Just { model, validation } ->
        [ HH.h3_ [ HH.text "Validation" ]
        , HH.p_ [ HH.text $ show validation ]
        , HH.h3_ [ HH.text "Reduction Steps" ]
        , case reductionToLeftmost model of
            Right steps -> HH.ol_ $ map (\step -> HH.li_ [ HH.text $ show step ]) steps
            Left err -> HH.p [ class_ "alert" ] [ HH.text $ "Warning: reduction cannot be turned into leftmost derivation: " <> err ]
        , HH.h3_ [ HH.text "Piece JSON" ]
        , HH.div_
            [ HH.p_ [ HH.text "with IDs:" ]
            , HH.pre_ [ HH.text $ JSON.writeJSON $ pieceToJSON model.piece ]
            , HH.p_ [ HH.text "without IDs:" ]
            , HH.pre_ [ HH.text $ JSON.writeJSON $ stripJSONIds $ pieceToJSON model.piece ]
            ]
        ]

showStackItem :: { m :: Model, name :: String } -> String
showStackItem item = item.name <> ": " <> show (hash $ show item.m)
