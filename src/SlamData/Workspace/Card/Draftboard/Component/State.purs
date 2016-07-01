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

module SlamData.Workspace.Card.Draftboard.Component.State
  ( State
  , StateP
  , initialState
  , stateFromModel
  , modelFromState
  , _decks
  , _moving
  , _grouping
  , _inserting
  , module Model
  ) where

import SlamData.Prelude

import DOM.HTML.Types (HTMLElement)

import Halogen as H

import Data.Lens (LensP, lens)
import Data.Map as Map

import SlamData.Effects (Slam)

import SlamData.Workspace.Card.Draftboard.Component.Query (QueryC)
import SlamData.Workspace.Deck.Component.Nested.Query as DNQ
import SlamData.Workspace.Deck.Component.Nested.State as DNS
import SlamData.Workspace.Deck.DeckId (DeckId)

import SlamData.Workspace.Card.Draftboard.Model as Model

type State =
  { decks ∷ Map.Map DeckId Model.DeckPosition
  , moving ∷ Maybe (Tuple DeckId Model.DeckPosition)
  , canvas ∷ Maybe HTMLElement
  , grouping ∷ Maybe DeckId
  , inserting ∷ Boolean
  }

type StateP =
  H.ParentState
    State DNS.State
    QueryC DNQ.QueryP
    Slam DeckId

initialState ∷ State
initialState =
  { decks: Map.empty
  , moving: Nothing
  , canvas: Nothing
  , grouping: Nothing
  , inserting: false
  }

stateFromModel
  ∷ Model.Model
  → State
stateFromModel { decks } =
  initialState
    { decks = decks }

modelFromState
  ∷ State
  → Model.Model
modelFromState { decks } =
  { decks }

-- | An array of positioned decks.
_decks ∷ LensP State (Map.Map DeckId Model.DeckPosition)
_decks = lens _.decks _{ decks = _ }

_moving ∷ LensP State (Maybe (Tuple DeckId Model.DeckPosition))
_moving = lens _.moving _{ moving = _ }

_grouping ∷ LensP State (Maybe DeckId)
_grouping = lens _.grouping _{ grouping = _ }

_inserting ∷ LensP State Boolean
_inserting = lens _.inserting _{ inserting = _ }
