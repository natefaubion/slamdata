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

module SlamData.Workspace.Deck.Component.Query
  ( Query(..)
  , DeckAction(..)
  , QueryP
  ) where

import SlamData.Prelude

import DOM.HTML.Types (HTMLElement)

import Halogen.Component.Opaque.Unsafe (OpaqueQuery)
import Halogen.HTML.Events.Types (Event, MouseEvent)

import SlamData.Workspace.AccessType as AT
import SlamData.Workspace.Card.Port.VarMap as Port
import SlamData.Workspace.Deck.DeckId (DeckId)
import SlamData.Workspace.Deck.Model (Deck)

import Utils.Path as UP

data Query a
  = RunActiveCard a
  | RunPendingCards a
  | GetId (Maybe DeckId → a)
  | GetPath (Maybe UP.DirPath → a)
  | SetName String a
  | SetAccessType AT.AccessType a
  | ExploreFile UP.FilePath a
  | Publish a
  | Load UP.DirPath DeckId a
  | SetModel DeckId Deck a
  | Save a
  | Reset (Maybe UP.DirPath) a
  | GetGlobalVarMap (Port.VarMap → a)
  | SetGlobalVarMap Port.VarMap a
  | FlipDeck a
  | GrabDeck (Event MouseEvent) a
  | ResizeDeck (Event MouseEvent) a
  | StartSliding (Event MouseEvent) a
  | StopSlidingAndSnap (Event MouseEvent) a
  | UpdateSliderPosition (Event MouseEvent) a
  | SetCardElement (Maybe HTMLElement) a
  | StopSliderTransition a
  | DoAction DeckAction a

data DeckAction
  = Mirror
  | Wrap
  | DeleteDeck

type QueryP = OpaqueQuery Query
