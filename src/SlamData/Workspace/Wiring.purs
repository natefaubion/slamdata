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

module SlamData.Workspace.Wiring
  ( Wiring
  , Cache
  , CardEval
  , DeckRef
  , ActiveState
  , PendingMessage
  , DeckMessage(..)
  , makeWiring
  , makeCache
  , putDeck
  , putDeck'
  , getDeck
  , putCache
  , getCache
  , putCardEval
  ) where

import SlamData.Prelude

import Control.Monad.Aff.AVar (AVar, makeVar', takeVar, putVar, modifyVar)
import Control.Monad.Aff.Bus as Bus
import Control.Monad.Aff.Free (class Affable, fromAff)
import Control.Monad.Aff.Promise (Promise, wait, defer)
import Control.Monad.Eff.Exception (message)
import Data.Set as Set

import Data.Map as Map

import SlamData.Effects (SlamDataEffects)
import SlamData.Quasar.Data as Quasar
import SlamData.Workspace.Card.Model as Card
import SlamData.Workspace.Card.CardId (CardId)
import SlamData.Workspace.Card.Port (Port)
import SlamData.Workspace.Deck.Model (Deck, deckIndex, decode, encode)
import SlamData.Workspace.Deck.DeckId (DeckId)
import SlamData.Workspace.Deck.AdditionalSource (AdditionalSource)

import Utils.Path (DirPath)

type CardEval =
  { card ∷ DeckId × Card.Model
  , input ∷ Maybe (Promise Port)
  , output ∷ Maybe (Promise (Port × (Set.Set AdditionalSource)))
  }

type DeckRef = Promise (Either String Deck)

type Cache k v = AVar (Map.Map k v)

type PendingMessage =
  { source ∷ DeckId
  , pendingCard ∷ DeckId × Card.Model
  , cards ∷ Cache (DeckId × CardId) CardEval
  }

data DeckMessage
  = DeckFocused DeckId

type ActiveState =
  { cardIndex ∷ Int
  }

type Wiring =
  { decks ∷ Cache DeckId DeckRef
  , activeState ∷ Cache DeckId ActiveState
  , cards ∷ Cache (DeckId × CardId) CardEval
  , pending ∷ Bus.BusRW PendingMessage
  , messaging ∷ Bus.BusRW DeckMessage
  }

makeWiring
  ∷ ∀ m
  . (Affable SlamDataEffects m)
  ⇒ m Wiring
makeWiring = fromAff do
  decks ← makeCache
  activeState ← makeCache
  cards ← makeCache
  pending ← Bus.make
  messaging ← Bus.make
  pure { decks, activeState, cards, pending, messaging }

makeCache
  ∷ ∀ m k v
  . (Affable SlamDataEffects m, Ord k)
  ⇒ m (Cache k v)
makeCache = fromAff (makeVar' mempty)

putDeck
  ∷ ∀ m
  . (Affable SlamDataEffects m)
  ⇒ DirPath
  → DeckId
  → Deck
  → Cache DeckId DeckRef
  → m (Either String Deck)
putDeck path deckId deck cache = fromAff do
  ref ← defer do
    res ← Quasar.save (deckIndex path deckId) $ encode deck
    when (isLeft res) do
      modifyVar (Map.delete deckId) cache
    pure $ bimap message (const deck) res
  modifyVar (Map.insert deckId ref) cache
  wait ref

putDeck'
  ∷ ∀ m
  . (Affable SlamDataEffects m)
  ⇒ DeckId
  → Deck
  → Cache DeckId DeckRef
  → m Unit
putDeck' deckId deck =
  putCache deckId (pure (Right deck))

getDeck
  ∷ ∀ m
  . (Affable SlamDataEffects m)
  ⇒ DirPath
  → DeckId
  → Cache DeckId DeckRef
  → m (Either String Deck)
getDeck path deckId cache = fromAff do
  decks ← takeVar cache
  case Map.lookup deckId decks of
    Just ref → do
      putVar cache decks
      wait ref
    Nothing → do
      ref ← defer do
        res ← (decode =<< _) <$> Quasar.load (deckIndex path deckId)
        when (isLeft res) do
          modifyVar (Map.delete deckId) cache
        pure res
      putVar cache (Map.insert deckId ref decks)
      wait ref

getCache
  ∷ ∀ m k v
  . (Affable SlamDataEffects m, Ord k)
  ⇒ k
  → Cache k v
  → m (Maybe v)
getCache key cache = fromAff do
  vals ← takeVar cache
  putVar cache vals
  pure $ Map.lookup key vals

putCache
  ∷ ∀ m k v
  . (Affable SlamDataEffects m, Ord k)
  ⇒ k
  → v
  → Cache k v
  → m Unit
putCache key val cache = fromAff do
  modifyVar (Map.insert key val) cache

putCardEval
  ∷ ∀ m
  . (Affable SlamDataEffects m)
  ⇒ CardEval
  → Cache (DeckId × CardId) CardEval
  → m Unit
putCardEval step = putCache (map _.cardId step.card) step
