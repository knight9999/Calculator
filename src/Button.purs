module Button
  (Slot, Message(..), component) where

import Prelude
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE

import Data.Int
import Data.Maybe (Maybe(..))
import Data.Number.Format

import Logic.CalcModel

-- data Btn = AC | PlusMinus | Percent | Div | Prod | Plus | Minus | Dot | Equal | Num Int

-- derive instance eqBtn :: Eq Btn
-- derive instance ordBtn :: Ord Btn

type Slot = forall q. H.Slot q Message Command

data Action = Push Command

data Message = Pushed Command

type State = Command

component :: forall q m. H.Component HH.HTML q Command Message m
component =
  H.mkComponent
    { initialState: \x -> x
    , render
    , eval: H.mkEval $ H.defaultEval
      { handleAction = handleAction }
    }

render :: forall m. State -> H.ComponentHTML Action () m
render state =
  HH.button
    [ HE.onClick \_ -> Just (Push state) ]
    [ HH.text $ show state ]

handleAction :: forall m. Action -> H.HalogenM State Action () Message m Unit
handleAction = case _ of
  Push btn -> do
    H.raise $ Pushed btn
