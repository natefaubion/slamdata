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

module SlamData.Workspace.Card.Tabs.Component.State where

import SlamData.Prelude

import Control.Monad.Aff.EventLoop (Breaker)

import Data.Array as Array

import Halogen as H

import SlamData.Monad (Slam)
import SlamData.Workspace.Card.Tabs.Component.Query (QueryC)
import SlamData.Workspace.Card.Tabs.Model (Model)
import SlamData.Workspace.Deck.Component.Nested.Query as DNQ
import SlamData.Workspace.Deck.Component.Nested.State as DNS
import SlamData.Workspace.Deck.DeckId (DeckId)

type StateP = H.ParentState State DNS.State QueryC DNQ.QueryP Slam DeckId

type Tab =
  { deckId ∷ DeckId
  , breaker ∷ Breaker Unit
  , name ∷ String
  , loaded ∷ Boolean
  }

type State =
  { tabs ∷ Array Tab
  , activeTab ∷ Maybe Int
  , ordering ∷ Maybe DragStatus
  }

type DragStatus =
  { source ∷ Int
  , offset ∷ Number
  , over ∷ Maybe Int
  }

initialState ∷ State
initialState =
  { tabs: []
  , activeTab: Nothing
  , ordering: Nothing
  }

modelFromState ∷ State → Model
modelFromState { tabs } = { tabs: _.deckId <$> tabs }

activateTab ∷ Maybe Int → State → State
activateTab Nothing st = st { activeTab = Nothing }
activateTab (Just ix) st =
  case Array.modifyAt ix (_ { loaded = true }) st.tabs of
    Nothing → st
    Just tabs' → st { tabs = tabs', activeTab = Just ix }

updateName ∷ DeckId → String → State → State
updateName deckId name st = st { tabs = tabs' }
  where
    tabs' = st.tabs <#> \tab →
      if tab.deckId ≡ deckId
        then tab { name = name }
        else tab

reorder ∷ ∀ a. Int → Int → Array a → Array a
reorder ix1 ix2 arr | ix1 ≡ ix2 = arr
reorder ix1 ix2 arr | ix1 < ix2 =
  Array.slice 0 ix1 arr
  <> Array.slice (ix1 + 1) (ix2 + 1) arr
  <> Array.slice ix1 (ix1 + 1) arr
  <> Array.slice (ix2 + 1) (Array.length arr) arr
reorder ix1 ix2 arr =
  Array.slice 0 ix2 arr
  <> Array.slice ix1 (ix1 + 1) arr
  <> Array.slice ix2 ix1 arr
  <> Array.slice (ix1 + 1) (Array.length arr) arr
