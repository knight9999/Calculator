module Main where

import Prelude

import Effect (Effect)
import Control.Monad.Maybe.Trans (MaybeT(..), lift, runMaybeT)
import Web.HTML.HTMLElement (toNode) as HTML
import Web.DOM.Node (removeChild)
import Web.DOM.ParentNode (QuerySelector(..))
import Halogen as H
import Halogen.Aff as HA
import Halogen.VDom.Driver (runUI)

import Calculator as CalculatorA

main :: Effect Unit
main = HA.runHalogenAff do
  HA.awaitLoad
  runMaybeT do
    html <- MaybeT $ HA.selectElement (QuerySelector "html")
    body <- MaybeT $ HA.selectElement (QuerySelector "body")
    _ <- H.liftEffect $ removeChild (HTML.toNode body) (HTML.toNode html)
    lift $ runUI CalculatorA.component unit html
