module App.Tabs.Export where

import Prelude

import App.Common (Selection)
import App.TikZ (tikzOpDecor, tikzReduction)
import App.Utils (copyToClipboard, download, class_)
import Data.Either (Either(..))
import Data.List as L
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
import Effect.Class (class MonadEffect, liftEffect)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import ProtoVoices.Folding (evalGraph)
import ProtoVoices.JSONTransport (modelFromJSON, modelToJSON, writeJSONPretty)
import ProtoVoices.Model (Model)
import ProtoVoices.Validation (validateReduction, validationIsOk)
import Simple.JSON as JSON

-- export component
-- ----------------
type ExportInput = { model :: (Maybe Model), name :: String, selection :: Selection }

data ExportAction
  = CopyToClipboard String
  | DownloadJSON String String
  | TogglePretty
  | ToggleTikzStandalone
  | ExportReceive ExportInput

exportComponent :: forall query output m. MonadEffect m => H.Component query ExportInput output m
exportComponent =
  H.mkComponent
    { initialState
    , render
    , eval:
        H.mkEval
          H.defaultEval
            { handleAction = handleExportAction
            , receive = Just <<< ExportReceive
            }
    }
  where
  initialState { model, name, selection } = { model, name, selection, pretty: false, tikzStandalone: false }

  render { model: modelMaybe, name, selection, pretty, tikzStandalone } = case modelMaybe of
    Nothing -> HH.text ""
    Just model ->
      let
        json = modelToJSON model

        jsonStrOrErr = (if pretty then writeJSONPretty else JSON.writeJSON) <$> json

        val = validateReduction model.reduction

        reLoad = modelFromJSON =<< json

        tikzStrOrErr = Right $ tikzReduction tikzStandalone $ evalGraph true true model.reduction
      in
        HH.div [ class_ "content-np tab pure-form" ]
          [ if validationIsOk val then
              HH.text ""
            else
              HH.p [ class_ "alert" ] [ HH.text "Warning: reduction is incomplete and/or contains errors." ]
          , HH.h3_ [ HH.text "JSON String" ]
          , case jsonStrOrErr of
              Right jsonStr ->
                HH.div_
                  [ HH.div [ class_ "pure-g" ]
                      [ HH.label [ class_ "pure-u-3-5" ]
                          [ HH.input
                              [ HP.type_ $ HP.InputCheckbox
                              , HP.checked pretty
                              , HE.onChange \_ -> TogglePretty
                              , HP.id "prettyJSON"
                              ]
                          , HH.text " pretty"
                          ]
                      , HH.button
                          [ class_ "pure-button pure-u-1-5"
                          , HE.onClick \_ -> CopyToClipboard jsonStr
                          ]
                          [ HH.text "Copy to Clipboard" ]
                      , HH.button
                          [ class_ "pure-button pure-button-primary pure-u-1-5"
                          , HE.onClick \_ -> DownloadJSON (name <> ".analysis.json") jsonStr
                          ]
                          [ HH.text "Download" ]
                      ]
                  , HH.pre_ [ HH.text $ jsonStr ]
                  ]
              Left err -> HH.p [ class_ "alert" ] [ HH.text $ "Cannot serialize reduction to JSON:" <> err ]
          , HH.div_ case reLoad of
              Left err -> [ HH.text $ "Error re-reading reduction: " <> err ]
              Right re ->
                if re == model then
                  [ HH.text "roundtrip ok" ]
                else if re.piece /= model.piece then
                  [ HH.text "pieces are different! original"
                  , HH.pre_ [ HH.text $ show model.piece ]
                  , HH.text "re-read:"
                  , HH.pre_ [ HH.text $ show re.piece ]
                  ]
                else case L.find (\(Tuple a b) -> a /= b) $ L.zip model.reduction.segments re.reduction.segments of
                  Nothing ->
                    [ HH.text "roundtrip not ok (but can't find wrong segment)! original:"
                    , HH.pre_ [ HH.text $ show model ]
                    , HH.text "re-read:"
                    , HH.pre_ [ HH.text $ show re ]
                    ]
                  Just (Tuple sorig sre) ->
                    [ HH.text "roundtrip not ok! offending segment original: "
                    , HH.pre_ [ HH.text $ show sorig ]
                    , HH.text " re-read:"
                    , HH.pre_ [ HH.text $ show sre ]
                    ]
          , HH.h3_ [ HH.text "TikZ Code" ]
          , case tikzStrOrErr of
              Right tikzStr ->
                HH.div_
                  [ HH.div [ class_ "pure-g" ]
                      [ HH.label [ class_ "pure-u-3-5" ]
                          [ HH.input
                              [ HP.type_ $ HP.InputCheckbox
                              , HP.checked tikzStandalone
                              , HE.onChange \_ -> ToggleTikzStandalone
                              , HP.id "tikzStandalone"
                              ]
                          , HH.text " standalone document"
                          ]
                      , HH.button
                          [ class_ "pure-button pure-u-1-5"
                          , HE.onClick \_ -> CopyToClipboard tikzStr
                          ]
                          [ HH.text "Copy to Clipboard" ]
                      , HH.button
                          [ class_ "pure-button pure-button-primary pure-u-1-5"
                          , HE.onClick \_ -> DownloadJSON (name <> ".tex") tikzStr
                          ]
                          [ HH.text "Download" ]
                      ]
                  , HH.pre_ [ HH.text $ tikzStr ]
                  ]
              Left err -> HH.p [ class_ "alert" ] [ HH.text $ "Cannot create TikZ code:" <> err ]
          , HH.h4_ [ HH.text "Operation Decorator for Current Note" ]
          , case tikzOpDecor selection of
              Just str ->
                HH.div_
                  [ HH.pre [ class_ "pure-u-4-5" ] [ HH.text str ]
                  , HH.button
                      [ class_ "pure-button pure-u-1-5"
                      , HE.onClick \_ -> CopyToClipboard str
                      ]
                      [ HH.text "Copy to Clipboard" ]
                  ]
              Nothing -> HH.text ""
          ]

  handleExportAction = case _ of
    CopyToClipboard str -> liftEffect $ copyToClipboard str
    DownloadJSON filename json -> do
      _ <- liftEffect $ download json filename "application/json"
      pure unit
    TogglePretty -> H.modify_ \st -> st { pretty = not st.pretty }
    ToggleTikzStandalone -> H.modify_ \st -> st { tikzStandalone = not st.tikzStandalone }
    ExportReceive { model, name, selection } ->
      H.modify_ \st ->
        st
          { model = model
          , name = name
          , selection = selection
          }
