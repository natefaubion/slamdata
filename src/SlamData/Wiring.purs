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

module SlamData.Wiring
  ( Wiring(..)
  , Cache
  , CardEval
  , DeckRef
  , ActiveState
  , PendingMessage
  , DeckMessage(..)
  , StepByStepGuide(..)
  , makeWiring
  , makeCache
  , putDeck
  , getDeck
  , putCache
  , getCache
  , clearCache
  , putCardEval
  ) where

import SlamData.Prelude

import Control.Monad.Aff.AVar (AVar, makeVar', takeVar, putVar, modifyVar)
import Control.Monad.Aff.Bus as Bus
import Control.Monad.Aff.Free (class Affable, fromAff, fromEff)
import Control.Monad.Aff.Promise (Promise, wait, defer)
import Control.Monad.Eff.Ref (Ref, newRef)
import Control.Monad.Fork (class MonadFork)

import Data.Map as Map
import Data.Set as Set

import SlamData.Effects (SlamDataEffects)
import SlamData.GlobalError as GE
import SlamData.Notification as N
import SlamData.Quasar.Auth.Authentication as Auth
import SlamData.Quasar.Data as Quasar
import SlamData.Quasar.Class (class QuasarDSL)
import SlamData.Quasar.Error as QE
import SlamData.GlobalMenu.Bus (SignInBus)
import SlamData.Workspace.Card.CardId (CardId)
import SlamData.Workspace.Card.Eval.Monad as CEM
import SlamData.Workspace.Card.Model as Card
import SlamData.Workspace.Card.Port (Port)
import SlamData.Workspace.Card.Port.VarMap as Port
import SlamData.Workspace.Deck.AdditionalSource (AdditionalSource)
import SlamData.Workspace.Deck.DeckId (DeckId)
import SlamData.Workspace.Deck.Model (Deck, deckIndex, decode, encode)

import Utils.Path (DirPath)

type CardEval =
  { card ∷ DeckId × Card.Model
  , input ∷ Maybe (Promise Port)
  , output ∷ Maybe (Promise (Port × (Set.Set AdditionalSource)))
  , eval ∷ Maybe (Promise CEM.EvalMachine)
  }

type DeckRef = Promise (Either QE.QError Deck)

type Cache k v = AVar (Map.Map k v)

type PendingMessage =
  { source ∷ DeckId
  , pendingCard ∷ DeckId × Card.Model
  , cards ∷ Cache (DeckId × CardId) CardEval
  }

data DeckMessage
  = DeckFocused DeckId
  | URLVarMapsUpdated

data StepByStepGuide
  = CardGuide
  | FlipGuide

type ActiveState =
  { cardIndex ∷ Int
  }

newtype Wiring = Wiring
    { decks ∷ Cache DeckId DeckRef
    , activeState ∷ Cache DeckId ActiveState
    , cards ∷ Cache (DeckId × CardId) CardEval
    , pending ∷ Bus.BusRW PendingMessage
    , messaging ∷ Bus.BusRW DeckMessage
    , notify ∷ Bus.BusRW N.NotificationOptions
    , globalError ∷ Bus.BusRW GE.GlobalError
    , requestNewIdTokenBus ∷ Auth.RequestIdTokenBus
    , urlVarMaps ∷ Ref (Map.Map DeckId Port.URLVarMap)
    , signInBus ∷ SignInBus
    , hasIdentified ∷ Ref Boolean
    , presentStepByStepGuide ∷ Bus.BusRW StepByStepGuide
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
  notify ← Bus.make
  globalError ← Bus.make
  requestNewIdTokenBus ← Auth.authentication
  urlVarMaps ← fromEff (newRef mempty)
  signInBus ← Bus.make
  hasIdentified ← fromEff (newRef false)
  presentStepByStepGuide ← Bus.make
  pure $
    Wiring
      { decks
      , activeState
      , cards
      , pending
      , messaging
      , notify
      , globalError
      , requestNewIdTokenBus
      , urlVarMaps
      , signInBus
      , hasIdentified
      , presentStepByStepGuide
      }

makeCache
  ∷ ∀ m k v
  . (Affable SlamDataEffects m, Ord k)
  ⇒ m (Cache k v)
makeCache = fromAff (makeVar' mempty)

putDeck
  ∷ ∀ m
  . (Affable SlamDataEffects m, MonadFork m, QuasarDSL m, MonadReader Wiring m)
  ⇒ DirPath
  → DeckId
  → Deck
  → m (Either QE.QError Unit)
putDeck path deckId deck = do
  Wiring wiring ← ask
  ref ← defer do
    res ← Quasar.save (deckIndex path deckId) $ encode deck
    when (isLeft res) do
      fromAff $ modifyVar (Map.delete deckId) wiring.decks
    pure $ const deck <$> res
  putCache deckId ref wiring.decks
  rmap (const unit) <$> wait ref

getDeck
  ∷ ∀ m
  . (Affable SlamDataEffects m, MonadFork m, QuasarDSL m, MonadReader Wiring m)
  ⇒ DirPath
  → DeckId
  → m (Either QE.QError Deck)
getDeck path deckId = do
  Wiring wiring ← ask
  decks ← fromAff $ takeVar wiring.decks
  case Map.lookup deckId decks of
    Just ref → do
      fromAff $ putVar wiring.decks decks
      wait ref
    Nothing → do
      ref ← defer do
        res ← ((lmap QE.msgToQError ∘ decode) =<< _) <$> Quasar.load (deckIndex path deckId)
        when (isLeft res) do
          fromAff $ modifyVar (Map.delete deckId) wiring.decks
        pure res
      fromAff $ putVar wiring.decks (Map.insert deckId ref decks)
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

clearCache
  ∷ ∀ m k v
  . (Affable SlamDataEffects m, Ord k)
  ⇒ Cache k v
  → m Unit
clearCache cache = fromAff do
  modifyVar (const $ Map.empty) cache
