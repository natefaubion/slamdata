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

module SlamData.Workspace.Card.Setups.Viz.Component.ChildSlot where

import SlamData.Prelude

import Halogen.Component.ChildPath (ChildPath, cp1, cp2, cp3, cp4)
import Halogen.Component.Proxy (ProxyQ)
import SlamData.Workspace.Card.Setups.Auxiliary as Aux
import SlamData.Workspace.Card.Setups.DimensionMap.Component.Query as DQ
import SlamData.Workspace.Card.Setups.PivotTable.Component.Query as PQ
import SlamData.Workspace.Card.Setups.VizPicker.Component as VT

type AuxQuery = ProxyQ (Const Void) Aux.State Aux.State

type ChildSlot = Unit ⊹ Unit ⊹ Unit ⊹ Unit ⊹ Void
type ChildQuery =
  DQ.Query ⨁ VT.Query ⨁ AuxQuery ⨁ PQ.Query ⨁ Const Void

type Path a b = ChildPath a ChildQuery b ChildSlot

cpDims ∷ Path DQ.Query Unit
cpDims = cp1

cpPicker ∷ Path VT.Query Unit
cpPicker = cp2

cpAux ∷ Path AuxQuery Unit
cpAux = cp3

cpPivot ∷ Path PQ.Query Unit
cpPivot = cp4
