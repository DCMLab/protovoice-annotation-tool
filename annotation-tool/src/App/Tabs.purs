module App.Tabs where

import Prelude

import App.Common (AppSlots, AppState, GraphAction(..), Tab(..), ModelInfo)
import App.Utils (class_)
import App.Tabs.Debug (debugComponent)
import App.Tabs.Export (exportComponent)
import App.Tabs.Import (importComponent)
import App.Tabs.SVG (svgComponent)
import App.Tabs.Settings (settingsComponent)
import App.Tabs.Style (styleComponent)
import Data.Maybe (Maybe(..))
import Effect.Aff.Class (class MonadAff)
import Effect.Class (class MonadEffect)
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Type.Proxy (Proxy(..))

--------------------
-- sub-components --
--------------------
tabHandle :: forall r p. { tab :: Maybe Tab | r } -> Tab -> String -> HH.HTML p GraphAction
tabHandle st tab name =
  HH.li [ class_ $ "pure-menu-item" <> if st.tab == Just tab then " pure-menu-selected" else "" ]
    [ HH.a
        [ class_ $ "pure-menu-link"
        , HE.onClick \_ -> SwitchTab if st.tab == Just tab then Nothing else Just tab
        , HP.href "javascript:;"
        ]
        [ HH.text name ]
    ]

renderTabs
  :: forall m
   . MonadEffect m
  => MonadAff m
  => AppState
  -> Maybe ModelInfo
  -> HH.ComponentHTML GraphAction AppSlots m
renderTabs st modelInfo =
  HH.div_
    [ HH.div [ class_ "content-np pure-menu pure-menu-horizontal" ]
        [ HH.ul [ class_ "pure-menu-list" ]
            [ tabHandle st ImportTab "Import"
            , tabHandle st ExportTab "Export"
            , tabHandle st SVGTab "SVG"
            , tabHandle st StyleTab "Style"
            , tabHandle st SettingsTab "Settings"
            , tabHandle st HelpTab "Help"
            , tabHandle st DebugTab "Debug"
            ]
        ]
    , case st.tab of
        Nothing -> HH.text ""
        Just HelpTab -> helpText
        Just ImportTab -> HH.slot (Proxy :: Proxy "importTab") 1 importComponent unit HandleImport
        Just ExportTab -> HH.slot_ (Proxy :: Proxy "exportTab") 0 exportComponent { model: _.model <$> st.loaded, name: st.name, selection: st.selected }
        Just StyleTab -> HH.slot (Proxy :: Proxy "styleTab") 2 styleComponent { modelInfo, selection: st.selected } HandleStyle
        Just SettingsTab -> HH.slot (Proxy :: Proxy "settingsTab") 3 settingsComponent st.settings HandleSettings
        Just SVGTab -> case modelInfo of
          Nothing -> HH.text ""
          Just modelInfo' -> HH.slot_ (Proxy :: Proxy "svgTab") 4 svgComponent { modelInfo: modelInfo', settings: st.settings }
        Just DebugTab -> debugComponent st modelInfo
    ]

-- help text
-- ---------
helpText :: forall p. HH.HTML p GraphAction
helpText =
  HH.div [ class_ "content-np tab" ]
    [ HH.h2_ [ HH.text "Manual" ]
    , HH.p_ [ HH.text "This app can be used to create protovoice analyses of pieces." ]
    , HH.p_ [ HH.text "Load pieces or analyses using the Import tab. Save analyses using the Export tab." ]
    , HH.p_
        [ HH.text "A piece is analyzed by reducing it using outer structure operations. "
        , HH.text "Reducing a slice creates a 'split' operation, reducing a transition creates a horizontalization. "
        , HH.text "Select a top-level slice or transition and press Enter to reduce it. "
        , HH.text "Pressing Backspace removes the operation below the selected slice or transition, if possible. "
        ]
    , HH.p_
        [ HH.text "The inner structure is determined by providing an \"explanation\" for every note. "
        , HH.text "This done by selecting the parent(s) of a note: "
        , HH.text "Click on a note in a slice below the top-level to select it. "
        , HH.text "Then hold CTRL and click on a note in one of its parent slices. "
        , HH.text "For notes that result from a split, you also have to provide the type of operation. "
        , HH.text "This is automatically determined from to the selected pitches, but can be overriden if desired. "
        , HH.text "Split notes can have either one (left/right ornament) or two parents (double-parent ornament). "
        , HH.text "Double-parent ornaments create new \"mandatory edges\", which must be used in the next reduction step."
        , HH.text "Correctly reduced notes are shown in grey, unreduced notes are shown in black, and inconsistencies are shown in orange or red."
        ]
    ]

