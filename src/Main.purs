module Main where

import Model
import Prelude
import Render
import Common (GraphActions(..))
import Data.Foldable (for_)
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Halogen as H
import Halogen.Aff as HA
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.VDom.Driver (runUI)
import Utils (examplePiece)
import Web.DOM.ParentNode (QuerySelector(..))

main :: Effect Unit
main =
  HA.runHalogenAff
    $ do
        HA.awaitLoad
        elt <- HA.selectElement (QuerySelector "#app")
        for_ elt (runUI appComponent unit)

type AppState
  = { selected :: Maybe Int
    , model :: Maybe Model
    }

appComponent :: forall query input output m. H.Component query input output m
appComponent = H.mkComponent { initialState, render, eval: H.mkEval $ H.defaultEval { handleAction = handleAction } }
  where
  initialState :: input -> AppState
  initialState _ = { selected: Nothing, model: Nothing }

  render st =
    HH.div
      []
      [ HH.h1_ [ HH.text "Proto-Voice Annotation" ]
      , HH.button [ HE.onClick $ \_ -> LoadPiece examplePiece ] [ HH.text "Load Example Piece" ]
      , case st.model of
          Nothing -> HH.text ""
          Just model -> renderReduction model.reduction st.selected
      ]

  handleAction (SelectSlice i) = H.modify_ \st -> st { selected = i }

  handleAction (LoadPiece piece) = H.modify_ \st -> st { model = Just $ loadPiece piece }
