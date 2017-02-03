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

module SlamData.Workspace.Card.Tabs.Component.Query
  ( Query(..)
  , QueryP
  , QueryC
  ) where

import SlamData.Prelude

import Halogen as H
import Halogen.Component.Utils.Drag (DragEvent)
import Halogen.HTML.Events.Types as HET

import SlamData.Workspace.Card.Common.EvalQuery (CardEvalQuery)
import SlamData.Workspace.Deck.Component.Nested.Query as DNQ
import SlamData.Workspace.Eval.Deck as Deck

data Query a
  = AddTab a
  | HandleMessage Deck.Id Deck.EvalMessage a
  | OrderStart Int (HET.Event HET.MouseEvent) a
  | Ordering Int DragEvent a
  | OrderOver Int a
  | OrderOut Int a

type QueryC = Coproduct CardEvalQuery Query

type QueryP = H.ParentQuery QueryC DNQ.QueryP Deck.Id
