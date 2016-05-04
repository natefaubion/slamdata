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

module SlamData.Notebook.Component.Query where

import SlamData.Prelude

import Data.BrowserFeatures as BF
import Data.List as L

import Halogen as H
import Halogen.Component.ChildPath (injSlot, injQuery)

import SlamData.Notebook.AccessType (AccessType)
import SlamData.Notebook.Card.CardId (CardId)
import SlamData.Notebook.Component.ChildSlot (ChildQuery, ChildSlot, cpDeck)
import SlamData.Notebook.Deck.Component.Query as Deck
import SlamData.Notebook.Deck.DeckId (DeckId)

import Utils.Path as UP

data Query a
  = SetAccessType AccessType a
  | GetAccessType (AccessType → a)
  | SetViewingCard (Maybe CardId) a
  | GetViewingCard (Maybe CardId → a)
  | SetParentHref String a
  | DismissAll a
  | Reset BF.BrowserFeatures UP.DirPath a
  | Load BF.BrowserFeatures UP.DirPath (L.List DeckId) a
  | GetPath (Maybe UP.DirPath → a)


type QueryP = Coproduct Query (H.ChildF ChildSlot ChildQuery)

toDraftboard ∷ H.Action Query → QueryP Unit
toDraftboard = left ∘ H.action

fromDraftboard ∷ ∀ a. (∀ i. (a → i) → Query i) → QueryP a
fromDraftboard r = left (H.request r)

toDeck ∷ H.Action Deck.Query → QueryP Unit
toDeck =
  right
    ∘ H.ChildF (injSlot cpDeck unit)
    ∘ injQuery cpDeck
    ∘ left
    ∘ H.action

fromDeck ∷ ∀ a. (∀ i. (a → i) → Deck.Query i) → QueryP a
fromDeck r =
  right
    $ H.ChildF (injSlot cpDeck unit)
    $ injQuery cpDeck
    $ left
    $ H.request r
