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
import Control.Monad.Throw as Throw
import Control.Parallel.Class (sequential, parallel)

import Data.Array as Array
import Data.Lens (preview, _Left)
import Data.List (List)
import Data.List as List
import Data.Map (Map)
import Data.Set (Set)

import SlamData.Effects (SlamDataEffects)
import SlamData.GlobalError as GE
import SlamData.Quasar.Class (class QuasarDSL)
import SlamData.Quasar.Error as QE
import SlamData.Wiring (Wiring)
import SlamData.Wiring as Wiring
import SlamData.Wiring.Cache (Cache)
import SlamData.Wiring.Cache as Cache
import SlamData.Workspace.Card.Eval.Class (ParEvalDecks, ParEvalDecks'(..), unParEvalDecks)
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

data EvalLevel = Leaf | Nested

derive instance eqEvalLevel ∷ Eq EvalLevel

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
runEvalLoop path decks cards tick urlVarMaps source = goInit Leaf
  where
    goInit ∷ EvalLevel → Card.Id → m Unit
    goInit level cardId = do
      Cache.get cardId cards >>= traverse_ \card → do
        -- When an child deck triggers a change in a parent, we don't want to
        -- start evaluating the parent if it is already pending. The parent
        -- should already be in control of that.
        when (level ≡ Leaf || isNothing card.pending) do
          let
            cardInput = fromMaybe Card.emptyOut card.input
            card' = card { pending = Just cardInput }
          Cache.put cardId card' cards
          evalCard mempty cardInput cardId card'

    evalCard ∷ Array Card.Id → Card.Out → Card.Id → Card.Cell → m Unit
    evalCard history cardInput@(cardPort × varMap) cardId card = do
      publish card (Card.Pending source cardInput)
      let
        cardEnv = Card.CardEnv { path, cardId, urlVarMaps }
        cardTrans = Card.modelToEval card.model
      result ← Card.runCard parEvalDecks cardEnv card.state cardTrans cardPort varMap
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
      fork $ parTraverse_ (goInit Nested) parentCardIds
      parTraverse_ (goNextCard history cardInput) cardIds

    goNextCard ∷ Array Card.Id → Card.Out → Card.Id → m Unit
    goNextCard history cardInput cardId =
      Cache.get cardId cards >>= traverse_ \card → do
        let card' = card { input = Just cardInput, pending = Just cardInput }
        Cache.put cardId card' cards
        evalCard history cardInput cardId card'

    -- We must attempt to evaluate child decks till we reach a stable point.
    -- Stable is defined as all children having a `Completed` status. We can't
    -- accomplish this in a single tick because other children may have been
    -- queued for evaluation while we are evaluating any one child.
    parEvalDecks ∷ ∀ a. ParEvalDecks a → m (Either QE.QError a)
    parEvalDecks = unParEvalDecks \(ParEvalDecks traverse' f deckIds co) → do
      let
        loopEval = do
          var ← liftAff $ AVar.makeVar' 0
          res ← runExceptT $ sequential $ flip traverse' deckIds $ parallel ∘ \deckId → do
            cell ← Throw.note "Deck not found" =<< lift (Cache.get deckId decks)
            case cell.status of
              Deck.Completed out → pure (cell.model × out)
              Deck.NeedsEval cardId → do
                liftAff $ AVar.modifyVar (_ + 1) var
                lift do
                  outVar ← liftAff AVar.makeVar
                  fork $ liftAff ∘ AVar.putVar outVar =<< Deck.waitComplete cell.bus
                  fork $ goInit Leaf cardId
                  Tuple cell.model <$> liftAff (AVar.peekVar outVar)
              Deck.PendingEval _ → do
                liftAff $ AVar.modifyVar (_ + 1) var
                out ← Deck.waitComplete cell.bus
                pure (cell.model × out)
          cnt ← liftAff $ AVar.peekVar var
          case res, cnt of
            Left err, _ → pure (Left (QE.msgToQError err))
            Right as, 0 → Right ∘ co <$> traverse' (pure ∘ uncurry f) as
            _, _ → loopEval
      loopEval

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
