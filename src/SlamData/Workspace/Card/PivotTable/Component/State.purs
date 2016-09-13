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

module SlamData.Workspace.Card.PivotTable.Component.State
  ( State
  , StateP
  , initialState
  , modelFromState
  ) where

import SlamData.Prelude
import Data.Argonaut as J
import Halogen as H
import SlamData.Monad (Slam)
import SlamData.Workspace.Card.PivotTable.Component.Query (QueryC)
import SlamData.Workspace.Card.PivotTable.Model (Model)

type StateP = H.ParentState State Void QueryC (Const Void) Slam Void

type State =
  { rows ∷ Array (J.Json)
  , cursors ∷ Array J.JCursor
  , dimensions ∷ Array J.JCursor
  , columns ∷ Array J.JCursor
  }

initialState ∷ State
initialState =
  { rows: []
  , cursors: []
  , dimensions: []
  , columns: []
  }

modelFromState ∷ State → Model
modelFromState s = {}
