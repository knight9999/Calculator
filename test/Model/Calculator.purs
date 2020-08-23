module Test.Model.Calculator 
  ( testCalculatorM
  ) where

import Prelude (Unit, discard)
import Effect (Effect)
import Effect.Console (log)
import Data.Maybe (Maybe(..))

import Test.Assert (assertEqual')

import Model.Calculator as CalcM
import Model.Operation as OpeM

testCalculatorM :: Effect Unit
testCalculatorM = do
  log "--- Test Flags ---"
  assertEqual' "Check Plus" {
    expected : CalcM.calc OpeM.Plus 10.0 2.0
  , actual : Just 12.0
  }
