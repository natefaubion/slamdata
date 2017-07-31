{-
Copyright 2017 SlamData, Inc.

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

module SlamData.Workspace.Card.Chart.PivotTableRenderer.Component.Query where

import DOM.Event.Types (Event)
import SlamData.Workspace.Card.Chart.PivotTableRenderer.Model as PTRM
import SlamData.Workspace.Card.Eval.State as ES

data Query a
  = Update ES.PivotTableR a
  | Load PTRM.Model a
  | Save (PTRM.Model → a)
  | StepPage PageStep a
  | SetCustomPage String a
  | UpdatePage Event a
  | ChangePageSize String a

data Message
  = ModelUpdated
  | StateUpdated (ES.PivotTableR → ES.PivotTableR)

data PageStep
  = First
  | Prev
  | Next
  | Last
