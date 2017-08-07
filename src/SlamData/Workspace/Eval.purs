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

import Control.Monad.Aff.Bus as Bus
import Control.Monad.Aff.Class (class MonadAff, liftAff)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Exception as Exn
import Control.Monad.Eff.Ref as Ref
import Control.Monad.Fork (class MonadFork, fork)
import Data.Array as Array
import Data.Lens (preview, _Left)
import Data.List (List)
import Data.List as List
import Data.Map (Map)
import Data.Map as Map
import Data.Set (Set)
import SlamData.Effects (SlamDataEffects)
import SlamData.GlobalError as GlobalError
import SlamData.Quasar.Class (class QuasarDSL)
import SlamData.Wiring (Wiring)
import SlamData.Wiring as Wiring
import SlamData.Wiring.Cache (Cache)
import SlamData.Wiring.Cache as Cache
import SlamData.Workspace.AccessType as AT
import SlamData.Workspace.Card.Error as CE
import SlamData.Workspace.Card.Port.VarMap (URLVarMap)
import SlamData.Workspace.Eval.Card as Card
import SlamData.Workspace.Eval.Deck as Deck
import SlamData.Workspace.Eval.Traverse (resolveUrlVarMaps)
import Utils.Path (DirPath)

type Tick = Int

type Eval f m a =
  MonadAff SlamDataEffects m
  ⇒ MonadAsk Wiring m
  ⇒ MonadFork Exn.Error m
  ⇒ Parallel f m
  ⇒ QuasarDSL m
  ⇒ a

evalGraph ∷ ∀ f m. Eval f m (List Card.DisplayCoord → m Unit)
evalGraph sources = do
  { path, varMaps, eval } ← Wiring.expose
  tick ← nextTick
  urlVarMaps ←
    resolveUrlVarMaps
      <$> Cache.snapshot eval.decks
      <*> Cache.snapshot eval.cards
      <*> liftEff (Ref.readRef varMaps)
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
        tryEvalCard mempty cardInput cardId card'

    tryEvalCard ∷ Array Card.Id → Card.Out → Card.Id → Card.Cell → m Unit
    tryEvalCard history cardInput cardId card =
      evaluatedOutputs (Card.childDeckIds card.model) >>= traverse_ \childOutputs → do
        let
          children = List.mapMaybe (toChildOut ∘ snd) childOutputs
          pendingIds = List.mapMaybe (preview Deck._PendingEval ∘ _.status ∘ snd) childOutputs
          unevaluatedIds = List.nub (List.mapMaybe (traverse (preview Deck._NeedsEval ∘ _.status)) childOutputs)
        if List.null pendingIds && List.null unevaluatedIds
          then evalCard history cardInput cardId card children
          else goChildDecks unevaluatedIds

    evalCard ∷ Array Card.Id → Card.Out → Card.Id → Card.Cell → List Card.ChildOut → m Unit
    evalCard history cardInput@(cardPort × varMap) cardId card children = do
      { accessType } ← Wiring.expose
      publish card $ Card.Pending source cardInput
      let
        readOnly = AT.isReadOnly accessType
        cardEnv = Card.CardEnv { path, cardId, urlVarMaps, children, varMap, readOnly }
        cardTrans = Card.modelToEval card.model

      for_ card.decks \deckId → runMaybeT do
        deck ← MaybeT $ Cache.get deckId decks
        Cache.put deckId deck { status = Deck.PendingEval cardId } decks

      result ← Card.runCard cardEnv card.state cardTrans cardPort

      for_ card.decks \deckId → runMaybeT do
        deck ← MaybeT $ Cache.get deckId decks
        publish deck $ Deck.CardComplete cardId

      let
        history' =
          case cardPort of
            Card.CardError _ → history
            _ → Array.snoc history cardId
        cardOutput =
          case result.output of
            Right out → out
            Left err → Card.CardError err × varMap
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
      { auth, bus } ← Wiring.expose
      case result.output of
        Left error → do
          liftEff $ Ref.writeRef auth.retryEval (Just cardId)
          traverse_
            (liftAff ∘ flip Bus.write bus.notify ∘ GlobalError.toNotificationOptions)
            (CE.cardToGlobalError error)
        Right _ →
          liftEff $ Ref.writeRef auth.retryEval Nothing

    goNext ∷ Array Card.Id → Card.Out → Set (Either Deck.Id Card.Id) → m Unit
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
      _ ← fork $ parTraverse_ goInit parentCardIds
      parTraverse_ (goNextCard history cardInput) cardIds

    goNextCard ∷ Array Card.Id → Card.Out → Card.Id → m Unit
    goNextCard history cardInput cardId =
      Cache.get cardId cards >>= traverse_ \card → do
        let card' = card { input = Just cardInput, pending = Just cardInput }
        Cache.put cardId card' cards
        tryEvalCard history cardInput cardId card'

    goChildDecks ∷ List (Deck.Id × Card.Id) → m Unit
    goChildDecks cardCoords = do
      for_ cardCoords \(deckId × cardId) → do
        Cache.get deckId decks >>= traverse_ \deck → do
          let deck' = deck { status = Deck.PendingEval cardId }
          Cache.put deckId deck' decks
      parTraverse_ goInit (List.nub (snd <$> cardCoords))

    evaluatedOutputs ∷ ∀ t. Traversable t ⇒ t Deck.Id → m (Maybe (t (Deck.Id × Deck.Cell)))
    evaluatedOutputs deckIds = do
      deckGraph ← Cache.snapshot decks
      let stats = for deckIds \deckId → Tuple deckId <$> Map.lookup deckId deckGraph
      pure stats

    toChildOut ∷ Deck.Cell → Maybe Card.ChildOut
    toChildOut { model, status } = case status of
      Deck.Completed (_ × varMap) → Just { namespace: model.name, varMap }
      _ → Nothing

publish ∷ ∀ m r a b. MonadAff SlamDataEffects m ⇒ { bus ∷ Bus.BusW' b a | r } → a → m Unit
publish rec message = liftAff (Bus.write message rec.bus)

nextTick ∷ ∀ m. MonadAff SlamDataEffects m ⇒ MonadAsk Wiring m ⇒ m Int
nextTick = do
  { eval } ← Wiring.expose
  liftEff do
    Ref.modifyRef eval.tick (add 1)
    Ref.readRef eval.tick

currentTick ∷ ∀ m. MonadAff SlamDataEffects m ⇒ MonadAsk Wiring m ⇒ m Int
currentTick = do
  { eval } ← Wiring.expose
  liftEff (Ref.readRef eval.tick)
