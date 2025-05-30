module App.Tabs.Settings where

import Prelude

import App.Common (AppSettings, defaultSettings)
import App.Render (class_)
import Data.Maybe (Maybe(..))
import Data.Number (fromString)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP

-- settings component
-- ------------------
data SettingsAction
  = SettingsToggleFlatHori
  | SettingsToggleShowAllEdges
  | SettingsToggleShowScore
  | SettingsSetXScale String
  | SettingsSetYScale String

settingsComponent :: forall query m. H.Component query AppSettings AppSettings m
settingsComponent = H.mkComponent { initialState, render, eval: H.mkEval H.defaultEval { handleAction = handleOptAction } }
  where
  initialState settings = { settings }

  render { settings } =
    HH.div [ class_ "tab pure-form" ]
      [ HH.p_
          [ HH.input
              [ HP.type_ $ HP.InputCheckbox
              , HP.checked settings.flatHori
              , HE.onChange \_ -> SettingsToggleFlatHori
              , HP.id "flatHori"
              ]
          , HH.label [ HP.for "flatHori" ] [ HH.text " render horis flat" ]
          ]
      , HH.p_
          [ HH.input
              [ HP.type_ $ HP.InputCheckbox
              , HP.checked settings.showAllEdges
              , HE.onChange \_ -> SettingsToggleShowAllEdges
              , HP.id "showAllEdges"
              ]
          , HH.label [ HP.for "showAllEdges" ] [ HH.text " show all edges" ]
          ]
      , HH.p_
          [ HH.input
              [ HP.type_ $ HP.InputCheckbox
              , HP.checked settings.showScore
              , HE.onChange \_ -> SettingsToggleShowScore
              , HP.id "showScore"
              ]
          , HH.label [ HP.for "showScore" ] [ HH.text " show score" ]
          ]
      , HH.p [ class_ "pure-g" ]
          [ HH.label [ class_ "pure-u-1-5", HP.for "xscale" ] [ HH.text $ "horizontal zoom: " <> show settings.xscale ]
          , HH.input
              [ class_ "pure-u-3-5"
              , HP.type_ $ HP.InputRange
              , HP.min (-5.0)
              , HP.max 0.0
              , HP.step $ HP.Step 0.01
              , HP.value $ show settings.xscale
              , HE.onValueChange SettingsSetXScale
              , HP.name "xscale"
              ]
          , HH.div [ class_ "pure-u-1-5" ] [ HH.button [ class_ "pure-button center", HE.onClick $ \_ -> SettingsSetXScale $ show defaultSettings.xscale ] [ HH.text "Reset" ] ]
          ]
      , HH.p [ class_ "pure-g" ]
          [ HH.label [ class_ "pure-u-1-5", HP.for "yscale" ] [ HH.text $ "yscale: " <> show settings.yscale ]
          , HH.input
              [ class_ "pure-u-3-5"
              , HP.type_ $ HP.InputRange
              , HP.min (-2.0)
              , HP.max 2.0
              , HP.step $ HP.Step 0.01
              , HP.value $ show settings.yscale
              , HE.onValueChange SettingsSetYScale
              , HP.name "vertical zoom"
              ]
          , HH.div [ class_ "pure-u-1-5" ] [ HH.button [ class_ "pure-button center", HE.onClick $ \_ -> SettingsSetYScale $ show defaultSettings.yscale ] [ HH.text "Reset" ] ]
          ]
      ]

  handleOptAction msg = do
    case msg of
      SettingsToggleFlatHori -> H.modify_ \st -> st { settings { flatHori = not st.settings.flatHori } }
      SettingsToggleShowAllEdges -> H.modify_ \st -> st { settings { showAllEdges = not st.settings.showAllEdges } }
      SettingsToggleShowScore -> H.modify_ \st -> st { settings { showScore = not st.settings.showScore } }
      SettingsSetXScale s -> case fromString s of
        Nothing -> pure unit
        Just n -> H.modify_ \st -> st { settings { xscale = n } }
      SettingsSetYScale s -> case fromString s of
        Nothing -> pure unit
        Just n -> H.modify_ \st -> st { settings { yscale = n } }
    H.raise =<< H.gets _.settings

