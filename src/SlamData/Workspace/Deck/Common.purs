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

module SlamData.Workspace.Deck.Common where

import SlamData.Prelude

import Data.Map as Map

import Halogen as H

import SlamData.Effects (Slam)
import SlamData.Workspace.Card.CardId (CardId(..))
import SlamData.Workspace.Card.CardType as CT
import SlamData.Workspace.Card.Draftboard.Component.State as DBS
import SlamData.Workspace.Deck.Component.Query (Query)
import SlamData.Workspace.Deck.Component.State (State)
import SlamData.Workspace.Deck.Component.ChildSlot (ChildSlot, ChildQuery, ChildState)
import SlamData.Workspace.Deck.DeckId (DeckId)
import SlamData.Workspace.Deck.Model (Deck, emptyDeck)

type DeckHTML = H.ParentHTML ChildState Query ChildQuery Slam ChildSlot

type DeckDSL = H.ParentDSL State ChildState Query ChildQuery Slam ChildSlot

defaultPosition ∷ DBS.DeckPosition
defaultPosition =
  { x: 1.0
  , y: 1.0
  , width: 20.0
  , height: 10.0
  }

wrappedDeck ∷ DBS.DeckPosition → DeckId → Deck
wrappedDeck rect deckId = emptyDeck
  { cards = pure
    { cardId: CardId 0
    , cardType: CT.Draftboard
    , state: DBS.encode $ DBS.initialState { decks = Map.singleton deckId rect }
    , hasRun: false
    }
  }
