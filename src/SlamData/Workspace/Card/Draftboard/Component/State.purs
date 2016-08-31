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
  , SplitOpts
  , SplitLocation
  , ResizeLocation
  , MoveLocation(..)
  , initialState
  , initialRect
  , modelFromState
  , stateFromModel
  , childSlots
  , recalc
  , updateRect
  , updateLayout
  ) where

import SlamData.Prelude
import Data.List as List
import Data.Map as Map
import Data.Rational (Rational)
import DOM.HTML.Types (HTMLElement)

import Halogen as H

import SlamData.Monad (Slam)
import SlamData.Workspace.Card.Draftboard.Component.Query (QueryC)
import SlamData.Workspace.Card.Draftboard.Layout (SplitBias, Rect, Edge, Cell)
import SlamData.Workspace.Card.Draftboard.Layout as Layout
import SlamData.Workspace.Card.Draftboard.Model (Model)
import SlamData.Workspace.Card.Draftboard.Orientation (Orientation)
import SlamData.Workspace.Card.Draftboard.Pane (Pane(..), Cursor, walkWithCursor)
import SlamData.Workspace.Deck.Component.Nested.Query as DNQ
import SlamData.Workspace.Deck.Component.Nested.State as DNS
import SlamData.Workspace.Deck.DeckId (DeckId)

type StateP = H.ParentState State DNS.State QueryC DNQ.QueryP Slam DeckId

data MoveLocation
  = Floating Number Number
  | Move (Cell (Maybe DeckId) Number)
  | Group (Cell (Maybe DeckId) Number) Orientation SplitBias

type State =
  { layout ∷ Pane (Maybe DeckId)
  , splitOpts ∷ Maybe SplitOpts
  , splitLocation ∷ Maybe SplitLocation
  , resizeLocation ∷ Maybe ResizeLocation
  , moveLocation ∷ Maybe MoveLocation
  , root ∷ Maybe HTMLElement
  , rootRect ∷ Rect Number
  , cellLayout ∷ List.List (Cell (Maybe DeckId) Number)
  , edgeLayout ∷ List.List (Edge Number)
  , cursors ∷ Map.Map DeckId Cursor
  , inserting ∷ Boolean
  }

type SplitOpts =
  { orientation ∷ Orientation
  , bias ∷ SplitBias
  , root ∷ Boolean
  }

type SplitLocation =
  { orientation ∷ Orientation
  , bias ∷ SplitBias
  , cursor ∷ Cursor
  , ratio ∷ Rational
  , valid ∷ Boolean
  , x ∷ Number
  , y ∷ Number
  , z ∷ Number
  }

type ResizeLocation =
  { edge ∷ Edge Number
  , ratio ∷ Rational
  , valid ∷ Boolean
  , collapse ∷ Maybe SplitBias
  , initial ∷ Number
  , offset ∷ Number
  }

initialState ∷ State
initialState =
  { layout: Cell Nothing
  , splitOpts: Nothing
  , splitLocation: Nothing
  , resizeLocation: Nothing
  , moveLocation: Nothing
  , root: Nothing
  , rootRect: initialRect
  , cellLayout: mempty
  , edgeLayout: mempty
  , cursors: mempty
  , inserting: false
  }

initialRect ∷ Rect Number
initialRect =
  { top: 0.0
  , left: 0.0
  , width: 0.0
  , height: 0.0
  }

modelFromState ∷ State → Model
modelFromState { layout } = { layout }

stateFromModel ∷ Model → State
stateFromModel { layout } = initialState { layout = layout }

childSlots ∷ State → List.List DeckId
childSlots = Map.keys ∘ _.cursors

recalc
  ∷ Rect Number
  → Pane (Maybe DeckId)
  → State
  → State
recalc rect layout = _
  { rootRect = rect
  , layout = layout
  , cellLayout = Layout.absoluteCells rect (Layout.cells layout)
  , edgeLayout = Layout.absoluteEdges rect (Layout.edges layout)
  , cursors = walkWithCursor goCursors mempty layout
  }
  where
  goCursors c m (Cell (Just deckId)) = Map.insert deckId c m
  goCursors _ m _ = m

updateRect ∷ Rect Number → State → State
updateRect rect st = recalc rect st.layout st

updateLayout ∷ Pane (Maybe DeckId) → State → State
updateLayout layout st = recalc st.rootRect layout st
