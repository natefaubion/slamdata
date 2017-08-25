{-
Copyright 2017 SlamData, Inc.

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

module SlamData.Workspace.Dialog.Reason.Component
  ( State
  , Query(..)
  , Message(..)
  , component
  ) where

import SlamData.Prelude

import Data.Array as Array
import Halogen as H
import Halogen.HTML.Events as HE
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import SlamData.Render.ClassName as CN
import SlamData.Workspace.Card.CardType (CardType)
import SlamData.Workspace.Card.CardType as CardType
import SlamData.Workspace.Card.InsertableCardType (InsertableCardType, print)

type State =
  { attemptedCardType ∷ CardType
  , reason ∷ String
  , cardPaths ∷ Array (Array InsertableCardType)
  }

data Query a = Raise Message a

data Message = Dismiss

component ∷ ∀ m. H.Component HH.HTML Query State Message m
component =
  H.component
    { initialState: id
    , render
    , eval
    , receiver: const Nothing
    }

render ∷ State → H.ComponentHTML Query
render state =
  HH.div
  [ HP.classes [ HH.ClassName "deck-dialog-embed" ] ]
  [ HH.h4_
      [ HH.text
        $ "Couldn't insert a "
        <> CardType.name state.attemptedCardType
        <> " card into this deck"
      ]
  , HH.div
      [ HP.classes [ HH.ClassName "deck-dialog-body" ] ]
      [ HH.p_
          [ HH.text state.reason
          ]
      , HH.p_ renderCardPathsMessage
      , HH.div_ $ map renderCardPath state.cardPaths
      ]
  , HH.div
      [ HP.classes [ HH.ClassName "deck-dialog-footer" ] ]
      [ HH.button
          [ HP.classes [ CN.btn, CN.btnDefault ]
          , HE.onClick (HE.input_ (Raise Dismiss))
          ]
          [ HH.text "Dismiss" ]
      ]
  ]
  where
  renderCardPathsMessage =
    case Array.length state.cardPaths of
      0 → []
      i →
        [ HH.text
            $ "To be able to insert a "
            <> show (CardType.name state.attemptedCardType)
            <> " card here you can add " <> setsOfCardsText i <> " first:"
        ]

  onlySingleCardPaths = Array.nub (Array.length <$> state.cardPaths) == [1]

  setsOfCardsText =
    case _ of
      1 → if onlySingleCardPaths then "this card" else "these cards in order"
      _ → if onlySingleCardPaths then "any of these cards" else "any of these sets of cards in order"

  renderCardPath cardPath =
    HH.p
      [ HP.classes [ HH.ClassName "deck-dialog-cardpath" ] ]
      (map renderCard cardPath)

  renderCard card =
    HH.span
      [ HP.classes [HH.ClassName "deck-dialog-cardpath-card" ] ]
      [ HH.text $ print card ]

eval ∷ ∀ m. Query ~> H.ComponentDSL State Query Message m
eval (Raise msg next) = H.raise msg $> next
