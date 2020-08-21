module Calculator
  ( component
  ) where

import Prelude
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Core as HC
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.Aff as HA
import Web.Event.Event (Event)
import Web.Event.Event as Event
import Effect.Aff.Class

import Data.Maybe (Maybe(..))
import Data.Array
import Data.Int
import Data.Either

import Effect (Effect, foreachE)
import Effect.Console (log, logShow)
import Effect.Class

data Action = Init

type State = {
  hoge :: Int
}

component :: forall q i o m. H.Component HH.HTML q i o m
component =
  H.mkComponent
    { initialState
    , render
    , eval: H.mkEval $ H.defaultEval
      { handleAction = handleAction
      , initialize = Just Init
      }
    }

initialState :: forall i. i -> State
initialState _ = {
  hoge: 1
}

render :: forall m. State -> H.ComponentHTML Action () m
render state = do
  HH.body []
    [ HH.div
      [ HP.attr (HC.AttrName "id") "root" ]
      [ HH.div 
        [ HP.attr (HC.AttrName "class") "component-app" ]
        [ HH.div 
          [ HP.attr (HC.AttrName "class") "component-display" ]
          [ HH.div [] [HH.text "0"] ]
        , HH.div 
          [ HP.attr (HC.AttrName "class") "component-button-panel"]
          [ HH.div
            []
            [ HH.div
              [ HP.attr (HC.AttrName "class") "component-button" ]
              [ HH.button [] [HH.text "AC" ]]
            , HH.div
              [ HP.attr (HC.AttrName "class") "component-button" ]
              [ HH.button [] [HH.text "+/-" ]]
            , HH.div
              [ HP.attr (HC.AttrName "class") "component-button" ]
              [ HH.button [] [HH.text "%" ]]
            , HH.div
              [ HP.attr (HC.AttrName "class") "component-button orange" ]
              [ HH.button [] [HH.text "÷" ]]
            ]
          , HH.div
            []
            [ HH.div
              [ HP.attr (HC.AttrName "class") "component-button" ]
              [ HH.button [] [HH.text "7" ]]
            , HH.div
              [ HP.attr (HC.AttrName "class") "component-button" ]
              [ HH.button [] [HH.text "8" ]]
            , HH.div
              [ HP.attr (HC.AttrName "class") "component-button" ]
              [ HH.button [] [HH.text "9" ]]
            , HH.div
              [ HP.attr (HC.AttrName "class") "component-button orange" ]
              [ HH.button [] [HH.text "×" ]]
            ]
          , HH.div
            []
            [ HH.div
              [ HP.attr (HC.AttrName "class") "component-button" ]
              [ HH.button [] [HH.text "4" ]]
            , HH.div
              [ HP.attr (HC.AttrName "class") "component-button" ]
              [ HH.button [] [HH.text "5" ]]
            , HH.div
              [ HP.attr (HC.AttrName "class") "component-button" ]
              [ HH.button [] [HH.text "6" ]]
            , HH.div
              [ HP.attr (HC.AttrName "class") "component-button orange" ]
              [ HH.button [] [HH.text "-" ]]
            ]            
          , HH.div
            []
            [ HH.div
              [ HP.attr (HC.AttrName "class") "component-button" ]
              [ HH.button [] [HH.text "1" ]]
            , HH.div
              [ HP.attr (HC.AttrName "class") "component-button" ]
              [ HH.button [] [HH.text "2" ]]
            , HH.div
              [ HP.attr (HC.AttrName "class") "component-button" ]
              [ HH.button [] [HH.text "3" ]]
            , HH.div
              [ HP.attr (HC.AttrName "class") "component-button orange" ]
              [ HH.button [] [HH.text "+" ]]
            ]            
          , HH.div
            []
            [ HH.div
              [ HP.attr (HC.AttrName "class") "component-button wide" ]
              [ HH.button [] [HH.text "0" ]]
            , HH.div
              [ HP.attr (HC.AttrName "class") "component-button" ]
              [ HH.button [] [HH.text "." ]]
            , HH.div
              [ HP.attr (HC.AttrName "class") "component-button orange" ]
              [ HH.button [] [HH.text "=" ]]
            ]            
          ]
        ]
      ]
    , HH.a
      [ HP.attr (HC.AttrName "href") "https://github.com/ahfarmer/calculator"
      , HP.attr (HC.AttrName "target") "_blank"
      , HP.attr (HC.AttrName "class") "github-fork-ribbon left-top"
      , HP.attr (HC.AttrName "title") "Fork me on GitHub"
      ]
      [ HH.text "Fork me on GitHub" ]
    ]

handleAction :: forall m o. Action -> H.HalogenM State Action () o m Unit
handleAction = case _ of
  Init -> do
    pure unit



