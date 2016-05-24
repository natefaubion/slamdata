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

module SlamData.Workspace.Card.Draftboard.Component.State where

import SlamData.Prelude

import Halogen as H
import Halogen.Component.Opaque.Unsafe (OpaqueQuery, OpaqueState)

import Data.Argonaut (Json, (.?), decodeJson, jsonEmptyObject, (~>), (:=))
import Data.Lens (LensP, lens)
import Data.Map as Map

import SlamData.Effects (Slam)
import SlamData.Workspace.Card.Draftboard.Component.Query (QueryC)
import SlamData.Workspace.Deck.Component.Query as DCQ
import SlamData.Workspace.Deck.Component.State as DCS
import SlamData.Workspace.Deck.DeckId (DeckId)

import Utils.Path (DirPath)

type State =
  { decks ∷ Map.Map DeckId DeckPosition
  , zoomed ∷ Maybe DeckId
  , path ∷ Maybe DirPath
  , moving ∷ Maybe (Tuple DeckId DeckPosition)
  }

type StateP =
  H.ParentState
    State (OpaqueState DCS.State)
    QueryC (OpaqueQuery DCQ.Query)
    Slam DeckId

type DeckPosition =
  { x ∷ Number
  , y ∷ Number
  , width ∷ Number
  , height ∷ Number
  }

initialState ∷ State
initialState =
  { decks: Map.empty
  , zoomed: Nothing
  , path: Nothing
  , moving: Nothing
  }

-- | An array of positioned decks.
_decks ∷ LensP State (Map.Map DeckId DeckPosition)
_decks = lens _.decks _{ decks = _ }

-- | The currently zoomed in Deck.
_zoomed ∷ LensP State (Maybe DeckId)
_zoomed = lens _.zoomed _{ zoomed = _ }

_path ∷ LensP State (Maybe DirPath)
_path = lens _.path _{ path = _ }

_moving ∷ LensP State (Maybe (Tuple DeckId DeckPosition))
_moving = lens _.moving _{ moving = _ }

encode ∷ State → Json
encode state
   = "decks" := map encodeDeckPosition state.decks
  ~> jsonEmptyObject

encodeDeckPosition ∷ DeckPosition → Json
encodeDeckPosition pos
   = "x" := pos.x
  ~> "y" := pos.y
  ~> "width" := pos.width
  ~> "height" := pos.height
  ~> jsonEmptyObject

decode ∷ Json → Either String State
decode = decodeJson >=> \obj →
  { decks: _
  , zoomed: Nothing
  , path: Nothing
  , moving: Nothing
  } <$> (traverse decodeDeckPosition =<< obj .? "decks")

decodeDeckPosition ∷ Json → Either String DeckPosition
decodeDeckPosition = decodeJson >=> \obj →
  { x: _
  , y: _
  , width: _
  , height: _
  } <$> obj .? "x"
    <*> obj .? "y"
    <*> obj .? "width"
    <*> obj .? "height"
