module View.Calculator
  ( render
  )
  where

import Prelude
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Core as HC
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP

import Model.Operation as OpeM
import Model.Calculator as CalcM

render :: forall a c m. (CalcM.Command -> H.ComponentHTML a c m) -> CalcM.Model -> H.ComponentHTML a c m
render mkSlot model = do 
    HH.div
      [ HP.attr (HC.AttrName "id") "root" ]
      [ HH.div 
        [ HP.attr (HC.AttrName "class") "component-app" ]
        [ HH.div 
          [ HP.attr (HC.AttrName "class") "component-display" ]
          [ HH.div [] [HH.text $ show model] ]
        , HH.div 
          [ HP.attr (HC.AttrName "class") "component-button-panel"]
          [ HH.div
            []
            [ HH.div
              [ HP.attr (HC.AttrName "class") "component-button" ]
              [ mkSlot CalcM.AC ]
            , HH.div
              [ HP.attr (HC.AttrName "class") "component-button" ]
              [ mkSlot CalcM.PlusMinus ]
            , HH.div
              [ HP.attr (HC.AttrName "class") "component-button" ]
              [ mkSlot CalcM.Percent ]
            , HH.div
              [ HP.attr (HC.AttrName "class") "component-button orange" ]
              [ mkSlot $ CalcM.Operation OpeM.Div ]
            ]
          , HH.div
            []
            [ HH.div
              [ HP.attr (HC.AttrName "class") "component-button" ]
              [ mkSlot $ CalcM.Num 7 ]
            , HH.div
              [ HP.attr (HC.AttrName "class") "component-button" ]
              [ mkSlot $ CalcM.Num 8 ]
            , HH.div
              [ HP.attr (HC.AttrName "class") "component-button" ]
              [ mkSlot $ CalcM.Num 9 ]
            , HH.div
              [ HP.attr (HC.AttrName "class") "component-button orange" ]
              [ mkSlot $ CalcM.Operation OpeM.Prod ]
            ]
          , HH.div
            []
            [ HH.div
              [ HP.attr (HC.AttrName "class") "component-button" ]
              [ mkSlot $ CalcM.Num 4 ]
            , HH.div
              [ HP.attr (HC.AttrName "class") "component-button" ]
              [ mkSlot $ CalcM.Num 5 ]
            , HH.div
              [ HP.attr (HC.AttrName "class") "component-button" ]
              [ mkSlot $ CalcM.Num 6 ]
            , HH.div
              [ HP.attr (HC.AttrName "class") "component-button orange" ]
              [ mkSlot $ CalcM.Operation OpeM.Minus ]
            ]            
          , HH.div
            []
            [ HH.div
              [ HP.attr (HC.AttrName "class") "component-button" ]
              [ mkSlot $ CalcM.Num 1 ]
            , HH.div
              [ HP.attr (HC.AttrName "class") "component-button" ]
              [ mkSlot $ CalcM.Num 2 ]
            , HH.div
              [ HP.attr (HC.AttrName "class") "component-button" ]
              [ mkSlot $ CalcM.Num 3 ]
            , HH.div
              [ HP.attr (HC.AttrName "class") "component-button orange" ]
              [ mkSlot $ CalcM.Operation OpeM.Plus ]
            ]            
          , HH.div
            []
            [ HH.div
              [ HP.attr (HC.AttrName "class") "component-button wide" ]
              [ mkSlot $ CalcM.Num 0 ]
            , HH.div
              [ HP.attr (HC.AttrName "class") "component-button" ]
              [ mkSlot $ CalcM.Dot ]
            , HH.div
              [ HP.attr (HC.AttrName "class") "component-button orange" ]
              [ mkSlot $ CalcM.Equal ]
            ]            
          ]
        ]
      ]

