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

module SlamData.Workspace.Card.Setups.Viz.Component.Query where

import SlamData.Workspace.Card.Setups.VizPicker.Component as VT
import SlamData.Workspace.Card.Setups.DimensionMap.Component.Query as Q
import SlamData.Workspace.Card.Setups.PivotTable.Component.Query as PQ
import SlamData.Workspace.Card.Setups.Auxiliary as Aux

data Query a
  = HandlePicker VT.Message a
  | HandleDims Q.Message a
  | HandleAux Aux.State a
  | HandlePivotTable PQ.Message a
  | ToggleVizPicker a
