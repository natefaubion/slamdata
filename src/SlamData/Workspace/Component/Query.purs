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

module SlamData.Workspace.Component.Query where

import SlamData.Prelude

import Halogen as H
import Halogen.Component.ChildPath (injSlot, injQuery)

import SlamData.Workspace.AccessType (AccessType)
import SlamData.Workspace.Card.Port.VarMap as Port
import SlamData.Workspace.Component.ChildSlot (ChildQuery, ChildSlot, cpDeck)
import SlamData.Workspace.Deck.Component.Query as Deck
import SlamData.Workspace.Deck.DeckId (DeckId)

import Utils.Path as UP

data Query a
  = SetGlobalVarMap Port.VarMap a
  | DismissAll a
  | Reset UP.DirPath a
  | Init a
  | Load UP.DirPath (Maybe DeckId) AccessType a

type QueryP = Coproduct Query (H.ChildF ChildSlot ChildQuery)

toWorkspace ∷ H.Action Query → QueryP Unit
toWorkspace = left ∘ H.action

fromWorkspace ∷ ∀ a. (∀ i. (a → i) → Query i) → QueryP a
fromWorkspace r = left (H.request r)

toDeck ∷ H.Action Deck.Query → QueryP Unit
toDeck =
  right
    ∘ H.ChildF (injSlot cpDeck unit)
    ∘ injQuery cpDeck
    ∘ right
    ∘ H.action

fromDeck ∷ ∀ a. (∀ i. (a → i) → Deck.Query i) → QueryP a
fromDeck r =
  right
    $ H.ChildF (injSlot cpDeck unit)
    $ injQuery cpDeck
    $ right
    $ H.request r
