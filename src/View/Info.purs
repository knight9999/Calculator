module View.Info
  ( render
  )
  where

import Prelude
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Core as HC
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP

render :: forall w i. HH.HTML w i
render = 
  HH.a
    [ HP.attr (HC.AttrName "href") "https://github.com/knight9999/Calculator"
    , HP.attr (HC.AttrName "target") "_blank"
    , HP.attr (HC.AttrName "class") "github-fork-ribbon left-top"
    , HP.attr (HC.AttrName "title") "Fork me on GitHub"
    ]
    [ HH.text "Fork me on GitHub" ]