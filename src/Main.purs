module Main where

import Model
import Prelude
import Render
import Common (GraphActions(..), OuterSelection(..))
import Data.Foldable (for_)
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Halogen as H
import Halogen.Aff as HA
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.VDom.Driver (runUI)
import Utils (examplePieceLong)
import Web.DOM.ParentNode (QuerySelector(..))

main :: Effect Unit
main =
  HA.runHalogenAff
    $ do
        HA.awaitLoad
        elt <- HA.selectElement (QuerySelector "#app")
        for_ elt (runUI appComponent unit)

type AppState
  = { selected :: OuterSelection
    , model :: Maybe Model
    }

appComponent :: forall query input output m. H.Component query input output m
appComponent = H.mkComponent { initialState, render, eval: H.mkEval $ H.defaultEval { handleAction = handleAction } }
  where
  initialState :: input -> AppState
  initialState _ = { selected: SelNone, model: Nothing }

  render st =
    HH.div
      []
      [ HH.h1_ [ HH.text "Proto-Voice Annotation" ]
      , HH.button [ HE.onClick $ \_ -> LoadPiece examplePieceLong ] [ HH.text "Load Example Piece" ]
      , HH.button [ HE.onClick $ \_ -> MergeAtSelected ] [ HH.text "Merge Selected" ]
      , HH.button [ HE.onClick $ \_ -> VertAtSelected ] [ HH.text "Vert Selected" ]
      , HH.button [ HE.onClick $ \_ -> UnMergeAtSelected ] [ HH.text "Unmerge Selected" ]
      , HH.button [ HE.onClick $ \_ -> UnVertAtSelected ] [ HH.text "Unvert Selected" ]
      , case st.model of
          Nothing -> HH.text ""
          Just model -> renderReduction model.reduction st.selected
      ]

  handleAction = case _ of
    SelectOuter i -> H.modify_ \st -> st { selected = i }
    LoadPiece piece -> H.modify_ \st -> st { model = Just $ loadPiece piece, selected = SelNone }
    MergeAtSelected ->
      H.modify_ \st -> case st.selected of
        SelSlice sel -> st { model = mergeAtSlice sel <$> st.model, selected = SelNone }
        _ -> st
    VertAtSelected ->
      H.modify_ \st -> case st.selected of
        SelTrans sel -> st { model = vertAtMid sel <$> st.model, selected = SelNone }
        _ -> st
    UnMergeAtSelected ->
      H.modify_ \st -> case st.selected of
        SelTrans sel -> st { model = undoMergeAtTrans sel <$> st.model, selected = SelNone }
        _ -> st
    UnVertAtSelected ->
      H.modify_ \st -> case st.selected of
        SelSlice sel -> st { model = undoVertAtSlice sel <$> st.model, selected = SelNone }
        _ -> st
