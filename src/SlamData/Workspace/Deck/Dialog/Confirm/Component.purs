{-
Copyright 2016 SlamData, Inc.

Licensed under the Apache License, Version 2.0 (the "License");
you may not use this file except in compliance with the License.
You may obtain a copy of the License at

    http://www.apache.org/licenses/LICENSE-2.0

Unless required by applicable law or agreed to in writing, software
distributed under the License is distributed on an "AS IS" BASIS,
WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
See the License for the specific language governing permissions and
limitations under the License.
-}

module SlamData.Workspace.Deck.Dialog.Confirm.Component where

import SlamData.Prelude

import Halogen as H
import Halogen.HTML.Events.Indexed as HE
import Halogen.HTML.Indexed as HH
import Halogen.HTML.Properties.Indexed as HP
import Halogen.Themes.Bootstrap3 as B

type State =
  { title ∷ String
  , body ∷ String
  , cancel ∷ String
  , confirm ∷ String
  }

data Query a = Confirm Boolean a

comp ∷ ∀ g. Functor g ⇒ H.Component State Query g
comp = H.component { render, eval } where

  render ∷ State → H.ComponentHTML Query
  render state =
    HH.div [ HP.classes [ HH.className "deck-dialog-embed" ] ]
      [ HH.h4_ [ HH.text  state.title ]
      , HH.div [ HP.classes [ HH.className "deck-dialog-body" ] ]
          [ HH.p_
              [ HH.text state.body
              ]
          ]
      , HH.div [ HP.classes [ HH.className "deck-dialog-footer" ] ]
          [ HH.button
              [ HP.classes [ B.btn ]
              , HE.onClick (HE.input_ $ Confirm false)
              ]
              [ HH.text state.cancel ]
          , HH.button
              [ HP.classes [ B.btn, B.btnPrimary ]
              , HE.onClick (HE.input_ $ Confirm true)
              ]
              [ HH.text state.confirm
              ]
          ]
      ]

  eval ∷ Natural Query (H.ComponentDSL State Query g)
  eval (Confirm _ next) = pure next
