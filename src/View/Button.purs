module View.Button
  ( render
  )
  where

import Prelude 
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE

import Data.Maybe (Maybe(..))

render :: forall m s a c. (Show s) => a -> s -> H.ComponentHTML a c m
render action state =
  HH.button
    [ HE.onClick \_ -> Just action]
    [ HH.text $ show state ]

