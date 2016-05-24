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

module SlamData.Workspace.Card.Draftboard.Component.Query where

import SlamData.Prelude

import Halogen as H
import Halogen.Component.Opaque.Unsafe (OpaqueQuery)
import Halogen.Component.Utils.Drag (DragEvent)

import SlamData.Workspace.Card.Common.EvalQuery (CardEvalQuery)
import SlamData.Workspace.Deck.Component.Query as DCQ
import SlamData.Workspace.Deck.DeckId (DeckId)

data Query a
  = Grabbing DeckId DragEvent a
  | Resizing DeckId DragEvent a
  | AddDeck a

type QueryC = Coproduct CardEvalQuery Query

type QueryP = H.ParentQuery QueryC (OpaqueQuery DCQ.Query) DeckId
