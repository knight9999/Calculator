module Test.Main where

import Prelude

import Effect (Effect)
import Effect.Class.Console (log)

import Test.Model.Calculator as CalculatorTM

main :: Effect Unit
main = do
  log "--- Test Main ---"
  CalculatorTM.testCalculatorM
  