module Calculator
  ( component
  ) where

import Prelude (class Show, Unit, bind, pure, show, unit, ($), (<<<))
import Halogen as H
import Halogen.HTML as HH

import Data.Maybe (Maybe(..))
import Data.Symbol (SProxy(..))

import Effect.Class (class MonadEffect)

import Button as Button
import Model.Calculator as MC
import Model.Operation as MO
import View.Calculator as VC
import View.Info as VI

data Action = Init | HandleButton Button.Message

type ChildSlots = (
  button :: Button.Slot
)

_button :: SProxy "button"
_button = SProxy

newtype State = State {
  model :: MC.Model
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
    MC.Model
      { next: ""
      , total: 0.0
      , operation: MO.Nop
      , isError: false
      }
}

render :: forall m. (MonadEffect m) => State -> H.ComponentHTML Action ChildSlots m
render (State state) =
  HH.body []
    [ VC.render
      (\command -> HH.slot _button command Button.component command (Just <<< HandleButton))
      state.model 
    , VI.render
    ]

handleAction :: forall o m. (MonadEffect m) => Action -> H.HalogenM State Action ChildSlots o m Unit
handleAction = case _ of
  Init -> do
    pure unit
  HandleButton (Button.Pushed command) -> do
    (State state) <- H.get
    let model' = MC.handleCommand command state.model
    H.modify_ \_ -> State { model: model' }
