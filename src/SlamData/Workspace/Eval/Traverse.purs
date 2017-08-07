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

module SlamData.Workspace.Eval.Traverse
  ( TraverseDeck(..)
  , TraverseCard(..)
  , unfoldTree
  , unfoldModelTree
  , unfoldEvalTree
  , getVarMaps
  , getSharingInput
  , hydrateCursor
  , resolveUrlVarMaps
  , isCyclical
  ) where

import SlamData.Prelude

import Data.Array as Array
import Data.Foldable as F
import Data.List (List(..), (:))
import Data.List as List
import Data.Map (Map)
import Data.Map as Map
import Data.Set as Set
import Data.StrMap (StrMap)
import Data.StrMap as StrMap
import Data.Validation.Semigroup as V
import SlamData.Workspace.Card.CardId as CID
import SlamData.Workspace.Card.Model as CM
import SlamData.Workspace.Card.Port as Port
import SlamData.Workspace.Card.Port.VarMap as VM
import SlamData.Workspace.Card.Variables.Eval as Variables
import SlamData.Workspace.Deck.DeckId as DID
import SlamData.Workspace.Dialog.Share.Model (SharingInput)
import SlamData.Workspace.Eval.Card as Card
import SlamData.Workspace.Eval.Deck as Deck
import Utils.Path (DirPath)

newtype TraverseDeck d c = TraverseDeck
  { deckId ∷ Deck.Id
  , deck ∷ d
  , cards ∷ List (TraverseCard d c)
  }

newtype TraverseCard d c = TraverseCard
  { cardId ∷ Card.Id
  , card ∷ c
  , decks ∷ List (TraverseDeck d c)
  }

unfoldTree
  ∷ ∀ d c
  . (d → Deck.Model)
  → (c → Card.Model)
  → Map Deck.Id d
  → Map Card.Id c
  → Deck.Id
  → Maybe (TraverseDeck d c)
unfoldTree getDeck getCard decks cards = unfoldDeck
  where
    unfoldDeck deckId =
      Map.lookup deckId decks <#> \deck →
        TraverseDeck
          { deckId
          , deck
          , cards: List.mapMaybe unfoldCard (List.fromFoldable (getDeck deck).cards)
          }

    unfoldCard cardId =
      Map.lookup cardId cards <#> \card →
        TraverseCard
          { cardId
          , card
          , decks: List.catMaybes (unfoldDeck <$> CM.childDeckIds (getCard card))
          }

unfoldModelTree
  ∷ Map Deck.Id Deck.Model
  → Map Card.Id Card.Model
  → Deck.Id
  → Maybe (TraverseDeck Deck.Model Card.Model)
unfoldModelTree = unfoldTree id id

unfoldEvalTree
  ∷ Map Deck.Id Deck.Cell
  → Map Card.Id Card.Cell
  → Deck.Id
  → Maybe (TraverseDeck Deck.Cell Card.Cell)
unfoldEvalTree = unfoldTree _.model _.model

getVarMaps ∷ TraverseDeck Deck.Cell Card.Cell → Map Card.Id Port.VarMap
getVarMaps = Map.fromFoldable ∘ goDeck
  where
    goDeck (TraverseDeck { cards }) =
      foldMap goCard cards

    goCard ∷ TraverseCard Deck.Cell Card.Cell → List (Card.Id × Port.VarMap)
    goCard (TraverseCard { cardId, card, decks }) =
      case card.model of
        CM.Variables vars →
          let varMap = Variables.buildVarMap cardId Map.empty vars
          in pure (cardId × V.unV (const VM.empty) id varMap)
        _ → foldMap goDeck decks

getSharingInput ∷ DirPath → TraverseDeck Deck.Cell Card.Cell → SharingInput
getSharingInput path root@(TraverseDeck { deckId }) =
  { workspacePath: path
  , deckId
  , caches: List.mapMaybe isCache resources
  , sources: List.mapMaybe isSource resources
  }
  where
    resources =
      List.fromFoldable (goDeck root)

    goCard (TraverseCard { cardId, card, decks }) =
      foldMap goDeck decks <> card.sources

    goDeck (TraverseDeck { cards }) =
      foldMap goCard cards

    isCache (Card.Cache fp) = Just fp
    isCache _ = Nothing

    isSource (Card.Source fp) = Just fp
    isSource _ = Nothing

hydrateCursor
  ∷ Map Deck.Id Deck.Cell
  → Map Card.Id Card.Cell
  → List Deck.Id
  → List Deck.Id
hydrateCursor decks cards = maybe Nil List.reverse ∘ go Nil
  where
    go c Nil = pure c
    go c (deckId : rest) = do
      deck ← Map.lookup deckId decks
      case deck.parent of
        Nothing → pure (deckId : c)
        Just cardId → do
          card ← Map.lookup cardId cards
          case List.fromFoldable card.decks, List.head rest of
            Nil, _ → Nothing
            _, Just next | Set.member next card.decks → go (deckId : c) rest
            next : _, _ → go (deckId : c) (next : rest)

resolveUrlVarMaps
  ∷ Map Deck.Id Deck.Cell
  → Map Card.Id Card.Cell
  → StrMap Port.URLVarMap
  → Map Card.Id Port.URLVarMap
resolveUrlVarMaps decks cards = StrMap.fold go mempty
  where
    go vms key val =
      case toCard key <|> toDeck key <|> toDeckName key of
        Just cardId → Map.alter (Just ∘ maybe val (StrMap.union val)) cardId vms
        Nothing → vms

    toVariables cardId = do
      card ← Map.lookup cardId cards
      case card.model of
        CM.Variables _ → pure cardId
        _ → Nothing

    toCard key =
      CID.fromString key >>= toVariables

    toDeck key = do
      deckId ← DID.fromString key
      deck ← Map.lookup deckId decks
      cardId ← Array.head deck.model.cards
      toVariables cardId

    toDeckName key = do
      guard (key ≠ "")
      deck ← F.find (eq key ∘ _.model.name) decks
      cardId ← Array.head deck.model.cards
      toVariables cardId

isCyclical
  ∷ Map Deck.Id Deck.Cell
  → Map Card.Id Card.Cell
  → Card.Id
  → Deck.Id
  → Boolean
isCyclical decks cards cardId = goDeck
  where
    goDeck deckId =
      case Map.lookup deckId decks of
        Nothing → false
        Just deck → F.or (goCard <$> deck.model.cards)

    goCard cardId' | cardId ≡ cardId' = true
    goCard cardId' =
      case Map.lookup cardId' cards of
        Nothing → false
        Just card → F.or (goDeck <$> Card.childDeckIds card.model)
