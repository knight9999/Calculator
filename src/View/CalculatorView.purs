module View.CalculatorView
  ( render
  )
  where

import Prelude
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Core as HC
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP

import Logic.Operation as Ope
import Logic.CalcModel as CalcModel

render :: forall a c m. (CalcModel.Command -> H.ComponentHTML a c m) -> CalcModel.Model -> H.ComponentHTML a c m
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
              [ mkSlot CalcModel.AC ]
            , HH.div
              [ HP.attr (HC.AttrName "class") "component-button" ]
              [ mkSlot CalcModel.PlusMinus ]
            , HH.div
              [ HP.attr (HC.AttrName "class") "component-button" ]
              [ mkSlot CalcModel.Percent ]
            , HH.div
              [ HP.attr (HC.AttrName "class") "component-button orange" ]
              [ mkSlot $ CalcModel.Operation Ope.Div ]
            ]
          , HH.div
            []
            [ HH.div
              [ HP.attr (HC.AttrName "class") "component-button" ]
              [ mkSlot $ CalcModel.Num 7 ]
            , HH.div
              [ HP.attr (HC.AttrName "class") "component-button" ]
              [ mkSlot $ CalcModel.Num 8 ]
            , HH.div
              [ HP.attr (HC.AttrName "class") "component-button" ]
              [ mkSlot $ CalcModel.Num 9 ]
            , HH.div
              [ HP.attr (HC.AttrName "class") "component-button orange" ]
              [ mkSlot $ CalcModel.Operation Ope.Prod ]
            ]
          , HH.div
            []
            [ HH.div
              [ HP.attr (HC.AttrName "class") "component-button" ]
              [ mkSlot $ CalcModel.Num 4 ]
            , HH.div
              [ HP.attr (HC.AttrName "class") "component-button" ]
              [ mkSlot $ CalcModel.Num 5 ]
            , HH.div
              [ HP.attr (HC.AttrName "class") "component-button" ]
              [ mkSlot $ CalcModel.Num 6 ]
            , HH.div
              [ HP.attr (HC.AttrName "class") "component-button orange" ]
              [ mkSlot $ CalcModel.Operation Ope.Minus ]
            ]            
          , HH.div
            []
            [ HH.div
              [ HP.attr (HC.AttrName "class") "component-button" ]
              [ mkSlot $ CalcModel.Num 1 ]
            , HH.div
              [ HP.attr (HC.AttrName "class") "component-button" ]
              [ mkSlot $ CalcModel.Num 2 ]
            , HH.div
              [ HP.attr (HC.AttrName "class") "component-button" ]
              [ mkSlot $ CalcModel.Num 3 ]
            , HH.div
              [ HP.attr (HC.AttrName "class") "component-button orange" ]
              [ mkSlot $ CalcModel.Operation Ope.Plus ]
            ]            
          , HH.div
            []
            [ HH.div
              [ HP.attr (HC.AttrName "class") "component-button wide" ]
              [ mkSlot $ CalcModel.Num 0 ]
            , HH.div
              [ HP.attr (HC.AttrName "class") "component-button" ]
              [ mkSlot $ CalcModel.Dot ]
            , HH.div
              [ HP.attr (HC.AttrName "class") "component-button orange" ]
              [ mkSlot $ CalcModel.Equal ]
            ]            
          ]
        ]
      ]

