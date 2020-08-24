module View.Calculator
  ( render
  )
  where

import Prelude
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Core as HC
import Halogen.HTML.Properties as HP

import Model.Operation as MO
import Model.Calculator as MC

render :: forall a c m. (MC.Command -> H.ComponentHTML a c m) -> MC.Model -> H.ComponentHTML a c m
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
              [ mkSlot MC.AC ]
            , HH.div
              [ HP.attr (HC.AttrName "class") "component-button" ]
              [ mkSlot MC.PlusMinus ]
            , HH.div
              [ HP.attr (HC.AttrName "class") "component-button" ]
              [ mkSlot MC.Percent ]
            , HH.div
              [ HP.attr (HC.AttrName "class") "component-button orange" ]
              [ mkSlot $ MC.Operation MO.Div ]
            ]
          , HH.div
            []
            [ HH.div
              [ HP.attr (HC.AttrName "class") "component-button" ]
              [ mkSlot $ MC.Num 7 ]
            , HH.div
              [ HP.attr (HC.AttrName "class") "component-button" ]
              [ mkSlot $ MC.Num 8 ]
            , HH.div
              [ HP.attr (HC.AttrName "class") "component-button" ]
              [ mkSlot $ MC.Num 9 ]
            , HH.div
              [ HP.attr (HC.AttrName "class") "component-button orange" ]
              [ mkSlot $ MC.Operation MO.Prod ]
            ]
          , HH.div
            []
            [ HH.div
              [ HP.attr (HC.AttrName "class") "component-button" ]
              [ mkSlot $ MC.Num 4 ]
            , HH.div
              [ HP.attr (HC.AttrName "class") "component-button" ]
              [ mkSlot $ MC.Num 5 ]
            , HH.div
              [ HP.attr (HC.AttrName "class") "component-button" ]
              [ mkSlot $ MC.Num 6 ]
            , HH.div
              [ HP.attr (HC.AttrName "class") "component-button orange" ]
              [ mkSlot $ MC.Operation MO.Minus ]
            ]            
          , HH.div
            []
            [ HH.div
              [ HP.attr (HC.AttrName "class") "component-button" ]
              [ mkSlot $ MC.Num 1 ]
            , HH.div
              [ HP.attr (HC.AttrName "class") "component-button" ]
              [ mkSlot $ MC.Num 2 ]
            , HH.div
              [ HP.attr (HC.AttrName "class") "component-button" ]
              [ mkSlot $ MC.Num 3 ]
            , HH.div
              [ HP.attr (HC.AttrName "class") "component-button orange" ]
              [ mkSlot $ MC.Operation MO.Plus ]
            ]            
          , HH.div
            []
            [ HH.div
              [ HP.attr (HC.AttrName "class") "component-button wide" ]
              [ mkSlot $ MC.Num 0 ]
            , HH.div
              [ HP.attr (HC.AttrName "class") "component-button" ]
              [ mkSlot $ MC.Dot ]
            , HH.div
              [ HP.attr (HC.AttrName "class") "component-button orange" ]
              [ mkSlot $ MC.Equal ]
            ]            
          ]
        ]
      ]

