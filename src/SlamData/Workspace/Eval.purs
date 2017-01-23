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

module SlamData.Workspace.Eval
  ( evalGraph
  , publish
  ) where

import SlamData.Prelude

import Control.Monad.Aff.AVar as AVar
import Control.Monad.Aff.Bus as Bus
import Control.Monad.Aff.Class (class MonadAff, liftAff)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Exception as Exn
import Control.Monad.Eff.Ref (readRef, modifyRef)
import Control.Monad.Fork (class MonadFork, fork)

import Data.Array as Array
import Data.Lens (preview, _Left)
import Data.List (List)
import Data.List as List
import Data.Map (Map)
import Data.Set (Set)

import SlamData.Effects (SlamDataEffects)
import SlamData.GlobalError as GE
import SlamData.Quasar.Class (class QuasarDSL)
import SlamData.Wiring (Wiring)
import SlamData.Wiring as Wiring
import SlamData.Wiring.Cache (Cache)
import SlamData.Wiring.Cache as Cache
import SlamData.Workspace.Card.Port as Port
import SlamData.Workspace.Card.Port.VarMap (URLVarMap)
import SlamData.Workspace.Eval.Card as Card
import SlamData.Workspace.Eval.Deck as Deck
import SlamData.Workspace.Eval.Traverse (resolveUrlVarMaps)

import Utils (hush)
import Utils.Path (DirPath)

type Tick = Int

type Eval f m a =
  ( MonadAff SlamDataEffects m
  , MonadAsk Wiring m
  , MonadFork Exn.Error m
  , Parallel f m
  , QuasarDSL m
  ) ⇒ a

evalGraph ∷ ∀ f m. Eval f m (List Card.DisplayCoord → m Unit)
evalGraph sources = do
  { path, varMaps, eval } ← Wiring.expose
  tick ← nextTick
  urlVarMaps ←
    resolveUrlVarMaps
      <$> Cache.snapshot eval.decks
      <*> Cache.snapshot eval.cards
      <*> liftEff (readRef varMaps)
  flip parTraverse_ sources \source →
    runEvalLoop path eval.decks eval.cards tick urlVarMaps source (snd source)

runEvalLoop
  ∷ ∀ f m
  . Eval f m
  ( DirPath
  → Cache Deck.Id Deck.Cell
  → Cache Card.Id Card.Cell
  → Tick
  → Map Card.Id URLVarMap
  → Card.DisplayCoord
  → Card.Id
  → m Unit )
runEvalLoop path decks cards tick urlVarMaps source = goInit
  where
    goInit ∷ Card.Id → m Unit
    goInit cardId = do
      Cache.get cardId cards >>= traverse_ \card → do
        let
          cardInput = fromMaybe Card.emptyOut card.input
          card' = card { pending = Just cardInput }
        Cache.put cardId card' cards
        evalCard mempty cardInput cardId card'

    evalCard ∷ Array Card.Id → Card.Out → Card.Id → Card.Cell → m Unit
    evalCard history cardInput@(cardPort × varMap) cardId card = do
      publish card (Card.Pending source cardInput)
      let
        deckEval = { getDeck: flip Cache.get decks, runDeck }
        cardEnv = Card.CardEnv { path, cardId, urlVarMaps }
        cardTrans = Card.modelToEval card.model
      result ← Card.runCard deckEval cardEnv card.state cardTrans cardPort varMap
      let
        history' =
          case cardPort of
            Card.CardError _ → history
            _ → Array.snoc history cardId
        cardOutput =
          case result.output of
            Right out → out
            Left  err → Card.portOut (Card.CardError (either id GE.print (GE.fromQError err)))
        card' = card
          { pending = Nothing
          , output = Just cardOutput
          , state = result.state
          , sources = result.sources
          , tick = Just tick
          }
      Cache.put cardId card' cards
      for_ result.state (publish card ∘ Card.StateChange source)
      publish card (Card.Complete source cardOutput)
      goNext history' cardOutput card.next

    goNext ∷ Array Card.Id → Card.Out → Set (Either Deck.Id Card.Id) -> m Unit
    goNext history cardInput next = do
      let
        next' = List.fromFoldable next
        deckIds = List.mapMaybe (preview _Left) next'
        cardIds = List.mapMaybe hush next'
      parentCardIds ← List.nub ∘ List.mapMaybe join <$> for deckIds \deckId → runMaybeT do
        deck ← MaybeT $ Cache.get deckId decks
        let deck' = deck { status = Deck.Completed cardInput }
        Cache.put deckId deck' decks
        publish deck (Deck.Complete history cardInput)
        pure deck.parent
      fork $ parTraverse_ goInit parentCardIds
      parTraverse_ (goNextCard history cardInput) cardIds

    goNextCard ∷ Array Card.Id → Card.Out → Card.Id → m Unit
    goNextCard history cardInput cardId =
      Cache.get cardId cards >>= traverse_ \card → do
        let card' = card { input = Just cardInput, pending = Just cardInput }
        Cache.put cardId card' cards
        evalCard history cardInput cardId card'

    runDeck ∷ Deck.Id → m (Maybe Port.Out)
    runDeck deckId = runMaybeT do
      deck ← MaybeT $ Cache.get deckId decks
      case Array.head deck.model.cards of
        Nothing → pure Port.emptyOut
        Just cardId → lift do
          outVar ← liftAff AVar.makeVar
          fork $ liftAff ∘ AVar.putVar outVar =<< Deck.waitComplete deck.bus
          fork $ goInit cardId
          liftAff $ AVar.peekVar outVar

publish ∷ ∀ m r a b. MonadAff SlamDataEffects m ⇒ { bus ∷ Bus.BusW' b a | r } → a → m Unit
publish rec message = liftAff (Bus.write message rec.bus)

nextTick ∷ ∀ m. (MonadAff SlamDataEffects m, MonadAsk Wiring m) ⇒ m Int
nextTick = do
  { eval } ← Wiring.expose
  liftEff do
    modifyRef eval.tick (add 1)
    readRef eval.tick

currentTick ∷ ∀ m. (MonadAff SlamDataEffects m, MonadAsk Wiring m) ⇒ m Int
currentTick = do
  { eval } ← Wiring.expose
  liftEff (readRef eval.tick)
