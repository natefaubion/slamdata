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

module SlamData.Workspace.Card.Viz.Renderer.PivotTable.Component.State where

import SlamData.Prelude

import Data.Argonaut as J
import SlamData.Workspace.Card.Port as Port
import SlamData.Workspace.Card.Viz.Renderer.PivotTable.Common (PTree)
import SlamData.Workspace.Card.Viz.Renderer.PivotTable.Model as PTRM

type State =
  { pageCount ∷ Int
  , pageIndex ∷ Int
  , pageSize ∷ Int
  , buckets ∷ Maybe (PTree J.Json J.Json)
  , customPage ∷ Maybe String
  , loading ∷ Boolean
  , port ∷ Maybe Port.PivotTablePort
  }

initialState ∷ State
initialState =
  { pageCount: 0
  , pageIndex: 0
  , pageSize: PTRM.initialModel.pageSize
  , buckets: Nothing
  , customPage: Nothing
  , loading: false
  , port: Nothing
  }
