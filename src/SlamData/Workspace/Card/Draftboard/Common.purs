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

module SlamData.Workspace.Card.Draftboard.Common
  ( transitiveGraphProducer
  , transitiveChildren
  , childDeckIds
  , deleteGraph
  , replacePointer
  , unsafeUpdateCachedDraftboard
  ) where

import SlamData.Prelude

import Control.Coroutine (Producer, runProcess, ($$), await)
import Control.Monad.Aff (Aff, runAff)
import Control.Monad.Aff.Free (class Affable, fromAff, fromEff)
import Control.Monad.Aff.Par (Par(..), runPar)
import Control.Monad.Eff.Exception as Exn
import Control.Monad.Eff.Ref as Ref
import Control.Monad.Except.Trans (ExceptT(..), runExceptT, withExceptT)

import Data.Array as Array
import Data.List as L
import Data.Map as Map
import Data.Path.Pathy ((</>))
import Data.Path.Pathy as Pathy
import Data.Set as Set

import SlamData.Effects (SlamDataEffects)
import SlamData.Quasar.Aff (QEff)
import SlamData.Quasar.Data as Quasar
import SlamData.Workspace.Card.CardId (CardId)
import SlamData.Workspace.Card.Model as CM
import SlamData.Workspace.Deck.DeckId (DeckId, deckIdToString)
import SlamData.Workspace.Deck.Model as DM
import SlamData.Workspace.Wiring (Wiring, getCache, putCardEval)

import Utils.AffableProducer (produce)
import Utils.Path (DirPath)

transitiveGraphProducer
  ∷ ∀ eff m
  . (Functor m, Affable (QEff eff) m)
  ⇒ DirPath
  → DeckId
  → Producer (Tuple DeckId (Either String (Array DeckId))) m Unit
transitiveGraphProducer path deckId = produce \emit → do
  pending ← Ref.newRef $ Set.singleton deckId
  runAff Exn.throwException (const (pure unit)) $
    go emit pending deckId

  where
  go emit pending parentId = do
    fromEff $ Ref.modifyRef pending (Set.insert parentId)

    loadChildIds parentId >>= case _ of
      Left err →
        fromEff $ emit (Left (Tuple parentId (Left err)))
      Right cids → do
        fromEff $ emit (Left (Tuple parentId (Right cids)))
        runPar $ traverse_ (Par ∘ go emit pending) cids

    fromEff $ Ref.modifyRef pending (Set.delete parentId)
    remaining ← fromEff $ Ref.readRef pending

    when (Set.isEmpty remaining) do
      fromEff $ emit (Right unit)

  loadChildIds parentId = runExceptT do
    json ← ExceptT $ Quasar.load (DM.deckIndex path parentId)
    ExceptT $ pure $ childDeckIds ∘ _.cards <$> DM.decode json

transitiveChildren
  ∷ ∀ eff m
  . Affable (QEff eff) m
  ⇒ DirPath
  → DeckId
  → m (Either String (Array DeckId))
transitiveChildren path deckId = fromAff go
  where
  go ∷ Aff (QEff eff) (Either String (Array DeckId))
  go = do
    ref ← fromEff $ Ref.newRef (Right [])
    runProcess (transitiveGraphProducer path deckId $$ collectIds ref)
    fromEff $ Ref.readRef ref

  collectIds ref = do
    Tuple _ cids ← await
    case cids of
      Left err →
        lift $ fromEff (Ref.modifyRef ref (const (Left err)))
      Right cids' → do
        lift $ fromEff (Ref.modifyRef ref (map (_ <> cids')))
        collectIds ref

childDeckIds ∷ Array (CM.Model) → Array DeckId
childDeckIds = (_ >>= getDeckIds ∘ _.model)
  where
  getDeckIds =
    case _ of
      CM.Draftboard { decks } → L.fromList $ Map.keys decks
      _ → []

deleteGraph
  ∷ ∀ eff m
  . Affable (QEff eff) m
  ⇒ DirPath
  → DeckId
  → m (Either String Unit)
deleteGraph path parentId = fromAff $ runExceptT do
  cids ← ExceptT $ transitiveChildren path parentId
  void
    $ withExceptT Exn.message
    $ ExceptT
    $ map sequence
    $ runPar
    $ traverse (Par ∘ delete)
    $ Array.cons parentId cids

  where
  delete deckId =
    Quasar.delete $ Left $ path </> Pathy.dir (deckIdToString deckId)

replacePointer
  ∷ DeckId
  → Maybe DeckId
  → CardId
  → Array (CM.Model)
  → Array (CM.Model)
replacePointer from to cid = map replace
  where
  replace model =
    case model of
      { cardId, model = CM.Draftboard { decks } } | cardId ≡ cid →
        { cardId, model: CM.Draftboard { decks: update decks } }
      _ → model

  update decks =
    case Map.lookup from decks of
      Nothing → decks
      Just rect →
        decks
          # Map.delete from
          # maybe id (flip Map.insert rect) to

-- | This shouldn't be done in general, but since draftboards have no inputs or
-- | outputs it's OK to just swap out the model for the cached card eval.
unsafeUpdateCachedDraftboard
  ∷ ∀ m
  . (Monad m, Affable SlamDataEffects m)
  ⇒ Wiring
  → DeckId
  → CM.Model
  → m Unit
unsafeUpdateCachedDraftboard wiring deckId model =
  case model of
    { cardId, model: CM.Draftboard db } → do
      let coord = deckId × cardId
      getCache coord wiring.cards >>= traverse_ \ce → do
        let card = map (_ { model = CM.Draftboard db }) ce.card
        putCardEval (ce { card = card }) wiring.cards
    _ → pure unit
