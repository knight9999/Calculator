module Model.Calculator
  ( Model(..)
  , Command(..)
  , updateModel
  , calc
  , handleCommand
  ) where

import Prelude (class Eq, class Ord, class Show, bind, join, negate, pure, ($), (&&), (*), (+), (-), (/), (/=), (<$>), (<>), (==))
import Data.Int (decimal, toStringAs)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Either (Either(..))
import Data.Array.NonEmpty as NonEmptyArray
import Data.Number as Number
import Data.Number.Format (precision, toStringWith)
import Data.String as String
import Data.String.Regex as Regex
import Data.String.Regex.Flags (noFlags)


import Model.Operation as MO

data Command = AC | PlusMinus | Percent | Dot | Equal | Operation MO.Operation | Num Int

derive instance eqCommand :: Eq Command
derive instance ordCommand :: Ord Command

instance showCommand :: Show Command where
  show AC = "AC"
  show PlusMinus = "+/-"
  show Percent = "%"
  show (Operation MO.Div) = "÷"
  show (Operation MO.Prod) = "×"
  show (Operation MO.Plus) = "+"
  show (Operation MO.Minus) = "-"
  show (Operation MO.Nop) = "Nop"
  show Equal = "="
  show Dot = "."
  show (Num x) = toStringAs decimal x

newtype Model = Model {
  total :: Number
, next :: String
, operation :: MO.Operation
, isError :: Boolean
}

updateModel :: MO.Operation -> Model -> Model
updateModel op (Model model) = 
  case (updateTotal model.operation model.total model.next) of
    Just total -> 
      Model model
      { total = total
      , next = ""
      , operation = op
      }
    Nothing ->
      Model model
      { total = 0.0
      , next = ""
      , operation = MO.Nop
      , isError = true
      }

updateTotal :: MO.Operation -> Number -> String -> Maybe Number
updateTotal op total next = 
  if next /= "" 
    then
      calc op total (fromMaybe 0.0 $ Number.fromString next)
    else 
      pure total

calc :: MO.Operation -> Number -> Number -> Maybe Number
calc op num1 num2 = case op of
  MO.Plus -> pure $ num1 + num2 
  MO.Minus -> pure $ num1 - num2
  MO.Prod -> pure $ num1 * num2
  MO.Div ->
    if num2 == 0.0
      then Nothing
      else pure $ num1 / num2
  MO.Nop -> pure $ num2


instance showModel :: Show Model where
  show (Model model) =
    if model.isError
      then
        "ERR"
      else
        if model.next /= ""
          then model.next -- show Next
          else stringFromNumber model.total -- show Total

stringFromNumber :: Number -> String
stringFromNumber total = removeZeros $ toStringWith (precision 16) total

removeZeros :: String -> String
removeZeros str
  = case (Regex.regex "^([^.]*)(\\.)(0*[^0]+|)0+$" noFlags) of
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

handleCommand :: Command -> Model -> Model
handleCommand command (Model model) =
  do
    let 
      calc command' =
        case command' of
          AC ->
            Model $ model { total = 0.0, next = "", operation = MO.Nop }
          PlusMinus ->
            if model.next == ""
              then
                Model $ model { total = -model.total, operation = MO.Nop }
              else
                Model $ model 
                  { next = 
                      case (String.singleton <$> String.codePointAt 0 model.next) of
                        Just "-" -> String.drop 1 model.next
                        _ -> "-" <> model.next
                  }
          Percent ->
            if model.next == ""
              then
                Model $ model { total = model.total * 0.01, next = "", operation = MO.Nop }
              else
                Model $ model
                  { next = stringFromNumber ((fromMaybe 0.0 $ Number.fromString model.next) / 100.0)
                  }
          Operation ope ->
            updateModel ope (Model model)
          Num x ->
            Model $ model 
              { next = 
                if x == 0 && model.next == "0"
                  then model.next
                  else model.next <> toStringAs decimal x
              }
          Dot ->
            Model $ model
              { next =
                (if model.next == ""
                  then "0"
                  else model.next
                ) <> "."
              }
          Equal ->
            updateModel MO.Nop (Model model)

    if model.isError 
      then
        case command of
          AC -> 
            let (Model model') = calc command
            in Model $ model' { isError = false }
          Num x ->
            let (Model model') = calc command
            in Model $ model' { isError = false }
          _ -> Model model
      else
        calc command
