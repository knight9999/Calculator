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

import Data.Maybe (Maybe(..), fromMaybe)
import Data.Array
import Data.Array.NonEmpty as NonEmptyArray
import Data.Int
import Data.Either
import Data.Symbol (SProxy(..))
import Data.String
import Data.String.Regex as Regex
import Data.String.Regex.Flags

import Effect (Effect, foreachE)
import Effect.Console (log, logShow)
import Effect.Class

import Data.Number as Number
import Data.Number.Format

import Button as Button

data Action = Init | HandleButton Button.Message

data Operation = Plus | Minus | Prod | Div | Equal | Nop

derive instance eqOperation :: Eq Operation

newtype State = State {
  total :: Number
, next :: String
, operation :: Operation
}

instance showState :: Show State where
  show (State s) = 
    if s.next /= ""
      then s.next -- show Next
      else removeZeros $ toStringWith (precision 16) s.total -- show Total


removeZeros :: String -> String
removeZeros str
  = case (Regex.regex "^([^.]*)(\\.)([^0]*)0+$" noFlags) of
      Left error -> "error"
      Right regex -> 
        case Regex.match regex str of
          Just list -> let
            result
              = do
                  l1 <- join $ (NonEmptyArray.index) list 1 -- 整数部
                  l2 <- join $ (NonEmptyArray.index) list 2 -- 小数点
                  l3 <- join $ (NonEmptyArray.index) list 3 -- 小数部（非ゼロ）
                  case l3 of
                    "" -> pure $ l1 <> l3
                    _  -> pure $ l1 <> l2 <> l3
            in fromMaybe "---" result
          _ -> str

type ChildSlots = (
  button :: Button.Slot
)

_button :: SProxy "button"
_button = SProxy

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
  next: ""
, total: 0.0
, operation: Nop
}

render :: forall m. (MonadEffect m) => State -> H.ComponentHTML Action ChildSlots m
render state = do
  HH.body []
    [ HH.div
      [ HP.attr (HC.AttrName "id") "root" ]
      [ HH.div 
        [ HP.attr (HC.AttrName "class") "component-app" ]
        [ HH.div 
          [ HP.attr (HC.AttrName "class") "component-display" ]
          [ HH.div [] [HH.text $ show state] ]
        , HH.div 
          [ HP.attr (HC.AttrName "class") "component-button-panel"]
          [ HH.div
            []
            [ HH.div
              [ HP.attr (HC.AttrName "class") "component-button" ]
              [ HH.slot _button Button.AC Button.component Button.AC (Just <<< HandleButton) ]
            , HH.div
              [ HP.attr (HC.AttrName "class") "component-button" ]
              [ HH.slot _button Button.PlusMinus Button.component Button.PlusMinus (Just <<< HandleButton) ]
            , HH.div
              [ HP.attr (HC.AttrName "class") "component-button" ]
              [ HH.slot _button Button.Percent Button.component Button.Percent (Just <<< HandleButton) ]
            , HH.div
              [ HP.attr (HC.AttrName "class") "component-button orange" ]
              [ HH.slot _button Button.Div Button.component Button.Div (Just <<< HandleButton) ]
            ]
          , HH.div
            []
            [ HH.div
              [ HP.attr (HC.AttrName "class") "component-button" ]
              [ HH.slot _button (Button.Num 7) Button.component (Button.Num 7) (Just <<< HandleButton) ]
            , HH.div
              [ HP.attr (HC.AttrName "class") "component-button" ]
              [ HH.slot _button (Button.Num 8) Button.component (Button.Num 8) (Just <<< HandleButton) ]
            , HH.div
              [ HP.attr (HC.AttrName "class") "component-button" ]
              [ HH.slot _button (Button.Num 9) Button.component (Button.Num 9) (Just <<< HandleButton) ]
            , HH.div
              [ HP.attr (HC.AttrName "class") "component-button orange" ]
              [ HH.slot _button Button.Prod Button.component Button.Prod (Just <<< HandleButton) ]
            ]
          , HH.div
            []
            [ HH.div
              [ HP.attr (HC.AttrName "class") "component-button" ]
              [ HH.slot _button (Button.Num 4) Button.component (Button.Num 4) (Just <<< HandleButton) ]
            , HH.div
              [ HP.attr (HC.AttrName "class") "component-button" ]
              [ HH.slot _button (Button.Num 5) Button.component (Button.Num 5) (Just <<< HandleButton) ]
            , HH.div
              [ HP.attr (HC.AttrName "class") "component-button" ]
              [ HH.slot _button (Button.Num 6) Button.component (Button.Num 6) (Just <<< HandleButton) ]
            , HH.div
              [ HP.attr (HC.AttrName "class") "component-button orange" ]
              [ HH.slot _button Button.Minus Button.component Button.Minus (Just <<< HandleButton) ]
            ]            
          , HH.div
            []
            [ HH.div
              [ HP.attr (HC.AttrName "class") "component-button" ]
              [ HH.slot _button (Button.Num 1) Button.component (Button.Num 1) (Just <<< HandleButton) ]
            , HH.div
              [ HP.attr (HC.AttrName "class") "component-button" ]
              [ HH.slot _button (Button.Num 2) Button.component (Button.Num 2) (Just <<< HandleButton) ]
            , HH.div
              [ HP.attr (HC.AttrName "class") "component-button" ]
              [ HH.slot _button (Button.Num 3) Button.component (Button.Num 3) (Just <<< HandleButton) ]
            , HH.div
              [ HP.attr (HC.AttrName "class") "component-button orange" ]
              [ HH.slot _button Button.Plus Button.component Button.Plus (Just <<< HandleButton) ]
            ]            
          , HH.div
            []
            [ HH.div
              [ HP.attr (HC.AttrName "class") "component-button wide" ]
              [ HH.slot _button (Button.Num 0) Button.component (Button.Num 0) (Just <<< HandleButton) ]
            , HH.div
              [ HP.attr (HC.AttrName "class") "component-button" ]
              [ HH.slot _button Button.Dot Button.component Button.Dot (Just <<< HandleButton) ]
            , HH.div
              [ HP.attr (HC.AttrName "class") "component-button orange" ]
              [ HH.slot _button Button.Equal Button.component Button.Equal (Just <<< HandleButton) ]
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

handleAction :: forall o m. (MonadEffect m) => Action -> H.HalogenM State Action ChildSlots o m Unit
handleAction = case _ of
  Init -> do
    pure unit
  HandleButton (Button.Pushed str) -> do
    H.liftEffect $ log $ show str
    case str of
      Button.AC ->
        H.modify_ \(State st) -> State (st { total = 0.0, next = "", operation = Nop })
      Button.PlusMinus ->
        H.modify_ \(State st) -> State st { total = -st.total, next = "", operation = Nop }
      Button.Percent ->
        H.modify_ \(State st) -> State st { total = st.total * 0.01, next = "", operation = Nop }
      Button.Plus ->
        H.modify_ \(State st) -> State st 
          { total = updateTotal st.operation st.total st.next
          , next = ""
          , operation = Plus
          }
      Button.Minus ->
        H.modify_ \(State st) -> State st 
          { total = updateTotal st.operation st.total st.next
          , next = ""
          , operation = Minus 
          }
      Button.Prod ->
        H.modify_ \(State st) -> State st 
          { total = updateTotal st.operation st.total st.next
          , next = ""
          , operation = Prod 
          }
      Button.Div ->
        H.modify_ \(State st) -> State st
          { total = updateTotal st.operation st.total st.next
          , next = ""
          , operation = Div
          }
      Button.Num x ->
        H.modify_ \(State st) -> 
          State (st{ next = if x == 0 && st.next == "0" then st.next else st.next <> toStringAs decimal x })
      Button.Dot ->
        H.modify_ \(State st) -> 
          State (st{ next = (if st.next == "" then "0" else st.next) <> "." }) 
      Button.Equal -> 
        H.modify_ \(State st) -> State st
          { total = case st.operation of
              Nop -> if st.next == "" then st.total else fromMaybe 0.0 $ Number.fromString st.next
              operation -> calc operation st.total (fromMaybe 0.0 $ Number.fromString st.next)
          , next = ""
          , operation = Nop
          }
      _ -> pure unit

updateTotal :: Operation -> Number -> String -> Number
updateTotal op total next = 
  if next /= "" 
    then calc op total (fromMaybe 0.0 $ Number.fromString next)
    else total

calc :: Operation -> Number -> Number -> Number 
calc op num1 num2 = case op of
  Plus -> num1 + num2 
  Minus -> num1 - num2
  Prod -> num1 * num2
  Div -> num1 / num2
  Nop -> num2
  _ -> num1
