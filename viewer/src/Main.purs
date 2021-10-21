module Main where

import Prelude
import Common (AppSettings, ViewerAction(..), Selection, class_, defaultSettings)
import Data.Either (Either(..), hush)
import Data.Foldable (for_)
import Data.Maybe (Maybe(..), fromMaybe)
import Effect (Effect)
import Effect.Aff.Class (class MonadAff)
import Effect.Class (class MonadEffect)
import Effect.Class.Console (log)
import Halogen as H
import Halogen.Aff as HA
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.VDom.Driver (runUI)
import ProtoVoices.Folding (evalGraph)
import ProtoVoices.JSONTransport (ModelJSON, modelFromJSON)
import ProtoVoices.Model (Model)
import Pruning (countSteps, pruneModel)
import Render (renderReduction)
import Simple.JSON (readJSON)
import Web.DOM.ParentNode (QuerySelector(..))

mainJSON :: String -> String -> Effect Unit
mainJSON eltSelector json =
  HA.runHalogenAff
    $ do
        HA.awaitLoad
        elt <- HA.selectElement (QuerySelector eltSelector)
        for_ elt (runUI viewerComponent json)

type ViewerState
  = { model :: Maybe { model :: Model, step :: Int, max :: Int }
    , settings :: AppSettings
    , selected :: Selection
    }

viewerComponent :: forall query output m. MonadEffect m => MonadAff m => H.Component query String output m
viewerComponent = H.mkComponent { initialState, render, eval: H.mkEval $ H.defaultEval { handleAction = handleAction, initialize = Just Init } }
  where
  initialState :: String -> ViewerState
  initialState json = { model, settings: defaultSettings, selected: Nothing }
    where
    model = do -- Maybe
      mjson :: ModelJSON <- hush (readJSON json)
      m <- hush $ modelFromJSON mjson
      pure { model: m, step: 0, max: countSteps m.reduction }

  render st = case st.model of
    Nothing -> HH.div [ class_ "pv-content" ] [ HH.text "No analysis loaded." ]
    Just { model, step, max } ->
      let
        modelPruned = pruneModel step model
      in
        HH.div_
          [ HH.div [ class_ "pv-content" ]
              $ [ HH.button
                    [ class_ "pure-button"
                    , HE.onClick $ \_ -> Backward
                    , HP.disabled $ step <= 0
                    , HP.title $ "Move on step back"
                    ]
                    [ HH.text "Previous"
                    ]
                , HH.button
                    [ class_ "pure-button"
                    , HE.onClick $ \_ -> Forward
                    , HP.disabled $ step >= max
                    , HP.title $ "Move one step forward"
                    ]
                    [ HH.text "Next" ]
                , HH.text $ " Step " <> show step <> " of " <> show max
                ]
          , case modelPruned of
              Left err -> HH.div [ class_ "pv-content" ] [ HH.text $ "error:" <> err ]
              Right m ->
                let
                  graph = evalGraph st.settings.flatHori true m.reduction
                in
                  HH.div
                    [ class_ "pv-wide" ]
                    [ renderReduction st.settings m.piece graph st.selected ]
          ]

  handleAction = case _ of
    NoOp -> pure unit
    Init -> log "initializing."
    Select _ -> pure unit -- TODO
    Forward ->
      H.modify_ \st ->
        fromMaybe st do
          { model, step, max } <- st.model
          pure $ st { model = Just { model, step: if step < max then step + 1 else step, max } }
    Backward ->
      H.modify_ \st ->
        fromMaybe st do
          { model, step, max } <- st.model
          pure $ st { model = Just { model, step: if step > 0 then step - 1 else step, max } }
    RegisterScoreElt _ -> pure unit -- TODO
