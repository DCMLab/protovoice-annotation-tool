module Main where

import Model
import Prelude
import Render
import Utils
import Data.Foldable (for_)
import Effect (Effect)
import Effect.Console (log)
import Halogen as H
import Halogen.Aff as HA
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.VDom.Driver (runUI)
import Web.DOM.ParentNode (QuerySelector(..))

main :: Effect Unit
main =
  HA.runHalogenAff
    $ do
        HA.awaitLoad
        elt <- HA.selectElement (QuerySelector "#app")
        for_ elt (runUI svgComponent unit)

data Action
  = Increment
  | Decrement

component =
  H.mkComponent
    { initialState
    , render
    , eval: H.mkEval $ H.defaultEval { handleAction = handleAction }
    }
  where
  initialState _ = 0

  render state =
    HH.div_
      [ HH.h1_ [ HH.text "Proto-Voice Annotation" ]
      , HH.button [ HE.onClick \_ -> Decrement ] [ HH.text "-" ]
      , HH.div_ [ HH.text $ show state ]
      , HH.button [ HE.onClick \_ -> Increment ] [ HH.text "+" ]
      ]

  handleAction = case _ of
    Increment -> H.modify_ \state -> state + 1
    Decrement -> H.modify_ \state -> state - 1

svgComponent = H.mkComponent { initialState, render, eval: H.mkEval $ H.defaultEval { handleAction = handleAction } }
  where
  model = loadPiece examplePiece

  initialState _ = { selected: 0 }

  render st =
    HH.div
      [ HP.style "overflow-x: scroll;" ]
      [ renderReduction model.reduction st.selected ]

  handleAction _ = pure unit
