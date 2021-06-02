module Main where

import Prelude
import Common (GraphActions(..), Selection(..), getSelSlice, getSelTrans, sliceSelected, transSelected)
import Control.Monad.State (class MonadState)
import Data.Either (Either(..))
import Data.Foldable (for_)
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Class (class MonadEffect)
import Effect.Class.Console (log)
import Halogen as H
import Halogen.Aff as HA
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.VDom.Driver (runUI)
import Model (Model, loadPiece, mergeAtSlice, undoMergeAtTrans, undoVertAtSlice, vertAtMid)
import Render (renderLeftmost, renderReduction)
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
  = { selected :: Selection
    , model :: Maybe Model
    }

tryModelAction ::
  forall a m.
  (MonadState AppState m) =>
  (MonadEffect m) =>
  (Selection -> Maybe a) ->
  (a -> Model -> Either String Model) ->
  m Unit
tryModelAction selector action = do
  st <- H.get
  let
    modelAndSel = do
      sel <- selector st.selected
      model <- st.model
      pure { sel, model }
  case modelAndSel of
    Nothing -> pure unit
    Just { sel, model } -> case action sel model of
      Left err -> log err
      Right model' -> H.put st { model = Just model', selected = SelNone }

appComponent :: forall query input output m. (MonadEffect m) => H.Component query input output m
appComponent = H.mkComponent { initialState, render, eval: H.mkEval $ H.defaultEval { handleAction = handleAction } }
  where
  initialState :: input -> AppState
  initialState _ = { selected: SelNone, model: Nothing }

  render st =
    HH.div
      []
      [ HH.h1_ [ HH.text "Proto-Voice Annotation" ]
      , HH.button [ HE.onClick $ \_ -> LoadPiece examplePieceLong ] [ HH.text "Load Example Piece" ]
      , HH.button
          [ HE.onClick $ \_ -> MergeAtSelected, HP.enabled (sliceSelected st.selected) ]
          [ HH.text "Merge Selected" ]
      , HH.button
          [ HE.onClick $ \_ -> VertAtSelected, HP.enabled (transSelected st.selected) ]
          [ HH.text "Vert Selected" ]
      , HH.button
          [ HE.onClick $ \_ -> UnMergeAtSelected, HP.enabled (transSelected st.selected) ]
          [ HH.text "Unmerge Selected" ]
      , HH.button
          [ HE.onClick $ \_ -> UnVertAtSelected, HP.enabled (sliceSelected st.selected) ]
          [ HH.text "Unvert Selected" ]
      , case st.model of
          Nothing -> HH.text ""
          Just model ->
            HH.div_
              [ renderReduction model.reduction st.selected
              , renderLeftmost model.reduction
              ]
      , HH.p_
          [ HH.text "Selection: "
          , HH.text $ show st.selected
          ]
      ]

  handleAction = case _ of
    Select s -> H.modify_ \st -> st { selected = s }
    LoadPiece piece -> H.modify_ \st -> st { model = Just $ loadPiece piece, selected = SelNone }
    MergeAtSelected -> tryModelAction getSelSlice mergeAtSlice
    VertAtSelected -> tryModelAction getSelTrans vertAtMid
    UnMergeAtSelected -> tryModelAction getSelTrans undoMergeAtTrans
    UnVertAtSelected -> tryModelAction getSelSlice undoVertAtSlice
