module App.Tabs.Style where

import Prelude

import App.Common (ModelInfo, Selection(..))
import App.Render (class_, mkSelect')
import Data.Either (Either(..))
import Data.Map as M
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Set as S
import Data.String as Str
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import ProtoVoices.Model (Edge, Staff(..), StartStop(..), Style, Styles, edgeIds, emptyStyle, styleSetClasses, styleSetLabel, stylesUpdateEdge, stylesUpdateNote, stylesUpdateSlice, stylesUpdateTrans)

-- style component
-- ---------------

type StyleTabInput = { modelInfo :: Maybe ModelInfo, selection :: Selection }
type StyleTabState = StyleTabInput

type StyleElement = Either Selection Edge

data StyleAction
  = StyleActionUpdateCSS String
  | StyleActionUpdateStaff Staff
  | StyleActionUpdateClasses StyleElement String
  | StyleActionUpdateLabel StyleElement String
  | StyleActionReceive StyleTabInput

-- in this function, add a style box 
styleComponent :: forall query m. H.Component query StyleTabInput Styles m
styleComponent = H.mkComponent
  { initialState
  , render
  , eval: H.mkEval H.defaultEval
      { handleAction = handleStyleAction
      , receive = Just <<< StyleActionReceive
      }
  }
  where
  initialState :: StyleTabInput -> StyleTabState
  initialState state = state

  render { modelInfo: minfo, selection } = case minfo of
    Nothing -> HH.text ""
    Just info -> HH.div [ class_ "tab" ]
      [ generalBox info.model.styles, selectionStyle info.model.styles info.graph ]
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

    -- | Creates style inputs for the currently selected element
    selectionStyle styles graph = case selection of
      SelNone -> HH.text ""
      SelNote { note } ->
        styleBox
          (Left selection)
          ("Note " <> note.id)
          (fromMaybe emptyStyle $ M.lookup note.id styles.notes)
      SelTrans transid -> HH.div_ $
        [ styleBox
            (Left selection)
            ("Transition " <> show transid)
            (fromMaybe emptyStyle $ M.lookup transid styles.transitions)
        ] <> edgeStyleBoxes transid graph styles
      SelSlice sliceid ->
        styleBox
          (Left selection)
          ("Slice " <> show sliceid)
          (fromMaybe emptyStyle $ M.lookup sliceid styles.slices)

    edgeStyleBoxes transid graph styles = case M.lookup transid graph.transitions of
      Nothing -> []
      Just trans ->
        map (mkEdgeBox "") (S.toUnfoldable trans.edges.regular) <>
          map (mkEdgeBox "Passing ") trans.edges.passing
      where
      mkEdgeBox prefix edge =
        styleBox
          (Right edge)
          (prefix <> "Edge " <> showNote edge.left <> " to " <> showNote edge.right)
          (fromMaybe emptyStyle $ M.lookup (edgeIds edge) styles.edges)
      showNote (Inner note) = show note.pitch <> " (" <> note.id <> ")"
      showNote s = show s

    -- | Creates style inputs (for classes and label) for one graph element
    styleBox :: StyleElement -> String -> Style -> _
    styleBox elt heading style = HH.div_
      [ HH.h3_ [ HH.text heading ]
      , HH.p
          [ class_ "pure-g" ]
          [ HH.label [ HP.for "style-classes", class_ "pure-u-1-4" ] [ HH.text "Classes:" ]
          , HH.input
              [ HP.type_ HP.InputText
              , class_ "pure-u-1-2"
              , HP.id "style-classes"
              , HP.value style.classes
              , HE.onValueInput $ StyleActionUpdateClasses elt
              ]
          ]
      , HH.p [ class_ "pure-g" ]
          [ HH.label [ HP.for "style-label", class_ "pure-u-1-4" ] [ HH.text "Label:" ]
          , HH.input
              [ HP.type_ HP.InputText
              , class_ "pure-u-1-2"
              , HP.id "style-label"
              , HP.value style.label
              , HE.onValueInput $ StyleActionUpdateLabel elt
              ]
          ]
      ]

  handleStyleAction = case _ of
    StyleActionUpdateCSS css' -> do
      st <- H.get
      case st.modelInfo of
        Nothing -> pure unit
        Just info -> H.raise info.model.styles { css = css' }
    StyleActionUpdateStaff staff -> do
      st <- H.get
      case st.modelInfo of
        Nothing -> pure unit
        Just info -> H.raise info.model.styles { staff = staff }
    StyleActionUpdateClasses elt classesStr -> do
      let classes = Str.trim classesStr
      st <- H.get
      updateEltStyle st elt (styleSetClasses classes)
    StyleActionUpdateLabel elt labelStr -> do
      let label = Str.trim labelStr
      st <- H.get
      updateEltStyle st elt (styleSetLabel label)
    StyleActionReceive state -> H.put state
    where
    -- | updates either style or label of the selected object
    updateEltStyle :: StyleTabState -> StyleElement -> (Maybe Style -> Maybe Style) -> _
    updateEltStyle st elt f = case st.modelInfo of
      Nothing -> pure unit
      Just info -> case elt of
        Left SelNone -> pure unit
        Left (SelNote { note }) -> H.raise $ stylesUpdateNote f note.id info.model.styles
        Left (SelSlice id) -> H.raise $ stylesUpdateSlice f id info.model.styles
        Left (SelTrans id) -> H.raise $ stylesUpdateTrans f id info.model.styles
        Right edge -> H.raise $ stylesUpdateEdge f (edgeIds edge) info.model.styles
