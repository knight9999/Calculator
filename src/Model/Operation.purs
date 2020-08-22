module Model.Operation
  ( Operation(..)
  ) where

import Prelude

data Operation = Plus | Minus | Prod | Div | Nop

derive instance eqOperation :: Eq Operation
derive instance ordOperation :: Ord Operation

