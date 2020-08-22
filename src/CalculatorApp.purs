module CalculatorApp
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

import Data.Maybe (Maybe(..), fromMaybe)
import Data.Array
import Data.Array.NonEmpty as NonEmptyArray
import Data.Int
import Data.Either
import Data.Symbol (SProxy(..))
import Data.String as String
import Data.String.Regex as Regex
import Data.String.Regex.Flags
import Data.Number as Number
import Data.Number.Format

import Effect (Effect, foreachE)
import Effect.Console (log, logShow)
import Effect.Class

import Button as Button
import Logic.CalcModel as CalcModel
import Logic.Operation as Ope
import View.CalculatorView as CalcView

data Action = Init | HandleButton Button.Message

type ChildSlots = (
  button :: Button.Slot
)

_button :: SProxy "button"
_button = SProxy

newtype State = State {
  model :: CalcModel.Model
}

instance showState :: Show State where
  show (State s) = show s 

component :: forall q i o m. (MonadEffect m) => H.Component HH.HTML q i o m
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
initialState _ = State {
  model:
    CalcModel.Model
      { next: ""
      , total: 0.0
      , operation: Ope.Nop
      , isError: false
      }
}

render :: forall m. (MonadEffect m) => State -> H.ComponentHTML Action ChildSlots m
render (State state) =
  HH.body []
    [ CalcView.render
      (\command -> HH.slot _button command Button.component command (Just <<< HandleButton))
      state.model 
      , HH.a
      [ HP.attr (HC.AttrName "href") "https://github.com/knight9999/Calculator"
      , HP.attr (HC.AttrName "target") "_blank"
      , HP.attr (HC.AttrName "class") "github-fork-ribbon left-top"
      , HP.attr (HC.AttrName "title") "Fork me on GitHub"
      ]
      [ HH.text "Fork me on GitHub" ]
    ]

handleAction :: forall o m. (MonadEffect m) => Action -> H.HalogenM State Action ChildSlots o m Unit
handleAction = case _ of
  Init -> do
    pure unit
  HandleButton (Button.Pushed command) -> do
    (State state) <- H.get
    let model' = CalcModel.handleCommand command state.model
    H.modify_ \_ -> State { model: model' }
