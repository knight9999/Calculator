module Test.Model.Calculator 
  ( testCalculatorM
  ) where

import Prelude (Unit, discard)
import Effect (Effect)
import Effect.Console (log)

import Test.Assert (assertEqual')

testCalculatorM :: Effect Unit
testCalculatorM = do
  log "--- Test Flags ---"
  assertEqual' "Check One '-'" {
    expected : "OK"
  , actual : "OK"
  }
