module App.Tabs.Style where

import Prelude

import App.Common (Selection(..))
import App.Render (class_, mkSelect')
import Data.Map as M
import Data.Maybe (Maybe(..), fromMaybe)
import Data.String as Str
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import ProtoVoices.Model (Model, Staff(..), Styles, emptyStyle, styleSetClasses, styleSetLabel, stylesUpdateNote, stylesUpdateSlice, stylesUpdateTrans)

-- style component
-- ---------------

type StyleInput = { model :: Maybe Model, selection :: Selection }

data StyleAction
  = StyleActionUpdateCSS String
  | StyleActionUpdateStaff Staff
  | StyleActionUpdateClasses String
  | StyleActionUpdateLabel String
  | StyleActionReceive StyleInput

-- in this function, add a style box 
styleComponent :: forall query m. H.Component query StyleInput Styles m
styleComponent = H.mkComponent
  { initialState
  , render
  , eval: H.mkEval H.defaultEval
      { handleAction = handleStyleAction
      , receive = Just <<< StyleActionReceive
      }
  }
  where
  initialState state = state

  render { model: mmodel, selection } = case mmodel of
    Nothing -> HH.text ""
    Just model -> HH.div [ class_ "tab" ]
      [ generalBox model.styles, selectionStyle model.styles ]
    where
    showStaffType = case _ of
      GrandStaff -> "Grand"
      TrebleStaff -> "Treble"
      BassStaff -> "Bass"
    generalBox styles = HH.div_
      [ HH.h3_ [ HH.text "General Style Options" ]
      , HH.p [ class_ "pure-g" ]
          [ HH.label [ HP.for "style-staff", class_ "pure-u-1-4" ] [ HH.text "Staff Type:" ]
          , mkSelect' StyleActionUpdateStaff showStaffType styles.staff [ GrandStaff, TrebleStaff, BassStaff ]
          ]
      , HH.label [ HP.for "style-css" ] [ HH.text "CSS:" ]
      , HH.textarea [ HP.id "style-css", HP.value styles.css, HE.onValueInput StyleActionUpdateCSS ]
      ]
    selectionStyle styles = HH.div_
      [ HH.h3_ [ HH.text "Element Style" ]
      , case selection of
          SelNone -> HH.p_ [ HH.text "nothing selected" ]
          SelNote { note } -> styleBox $ fromMaybe emptyStyle $ M.lookup note.id styles.notes
          SelTrans _ -> HH.text "tbi"
          SelSlice sliceid -> styleBox $ fromMaybe emptyStyle $ M.lookup sliceid styles.slices
      ]
    -- | Creates a div with inputs for style properties (classes and label)
    styleBox style = HH.div_
      [ HH.p [ class_ "pure-g" ]
          [ HH.label [ HP.for "style-classes", class_ "pure-u-1-4" ] [ HH.text "Classes:" ]
          , HH.input
              [ HP.type_ HP.InputText
              , class_ "pure-u-1-2"
              , HP.id "style-classes"
              , HP.value style.classes
              , HE.onValueInput StyleActionUpdateClasses
              ]
          ]
      , HH.p [ class_ "pure-g" ]
          [ HH.label [ HP.for "style-label", class_ "pure-u-1-4" ] [ HH.text "Label:" ]
          , HH.input
              [ HP.type_ HP.InputText
              , class_ "pure-u-1-2"
              , HP.id "style-label"
              , HP.value style.label
              , HE.onValueInput StyleActionUpdateLabel
              ]
          ]
      ]

  handleStyleAction = case _ of
    StyleActionUpdateCSS css' -> do
      st <- H.get
      case st.model of
        Nothing -> pure unit
        Just model -> H.raise model.styles { css = css' }
    StyleActionUpdateStaff staff -> do
      st <- H.get
      case st.model of
        Nothing -> pure unit
        Just model -> H.raise model.styles { staff = staff }
    StyleActionUpdateClasses classesStr -> do
      let classes = Str.trim classesStr
      st <- H.get
      updateSelected st (styleSetClasses classes)
    StyleActionUpdateLabel labelStr -> do
      let label = Str.trim labelStr
      st <- H.get
      updateSelected st (styleSetLabel label)
    StyleActionReceive state -> H.put state
    where
    -- | updates either style or label of the selected object
    updateSelected st f = case st.model of
      Nothing -> pure unit
      Just model -> case st.selection of
        SelNone -> pure unit
        SelNote { note } -> H.raise $ stylesUpdateNote f note.id model.styles
        SelSlice id -> H.raise $ stylesUpdateSlice f id model.styles
        SelTrans id -> H.raise $ stylesUpdateTrans f id model.styles
