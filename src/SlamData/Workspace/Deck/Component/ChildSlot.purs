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

module SlamData.Workspace.Deck.Component.ChildSlot where

import SlamData.Prelude

import Halogen.Component.ChildPath (ChildPath, cpL, cpR, (:>))

import SlamData.Workspace.Card.Component (CardQueryP, CardStateP)
import SlamData.Workspace.Card.CardId (CardId)
import SlamData.Workspace.Deck.DeckId (DeckId)
import SlamData.Workspace.Deck.BackSide.Component as Back
import SlamData.Workspace.Deck.Indicator.Component as Indicator
import SlamData.Workspace.Deck.Dialog.Component as Dialog

newtype CardSlot = CardSlot (DeckId × CardId)

derive instance genericCardSlot ∷ Generic CardSlot
derive instance eqCardSlot ∷ Eq CardSlot
derive instance ordCardSlot ∷ Ord CardSlot

type BackSideSlot = Unit

type IndicatorSlot = Unit

type ChildSlot =
  CardSlot ⊹ Unit ⊹ Unit ⊹ Unit

type ChildQuery =
  CardQueryP ⨁ Back.Query ⨁ Indicator.Query ⨁ Dialog.QueryP

type ChildState =
  CardStateP ⊹ Back.State ⊹ Indicator.State ⊹ Dialog.StateP

cpCard
  ∷ ChildPath
      CardStateP ChildState
      CardQueryP ChildQuery
      CardSlot ChildSlot
cpCard = cpL

cpBackSide
  ∷ ChildPath
      Back.State ChildState
      Back.Query ChildQuery
      Unit ChildSlot
cpBackSide = cpR :> cpL

cpIndicator
  ∷ ChildPath
      Indicator.State ChildState
      Indicator.Query ChildQuery
      Unit ChildSlot
cpIndicator = cpR :> cpR :> cpL

cpDialog
  ∷ ChildPath
      Dialog.StateP ChildState
      Dialog.QueryP ChildQuery
      Unit ChildSlot
cpDialog = cpR :> cpR :> cpR
