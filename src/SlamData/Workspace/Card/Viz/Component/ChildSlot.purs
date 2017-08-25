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

module SlamData.Workspace.Card.Viz.Component.ChildSlot where

import SlamData.Prelude

import Halogen.Component.ChildPath as CP
import Halogen.ECharts as HEC

import SlamData.Workspace.Card.Viz.Renderer.Metric.Component as MR
import SlamData.Workspace.Card.Viz.Renderer.PivotTable.Component as PR
import SlamData.Workspace.Card.Viz.Renderer.Input.Component as IR
import SlamData.Workspace.Card.Viz.Renderer.Select.Component as SR
import SlamData.Workspace.Card.Viz.Renderer.Geo.Component as GR

type ChildQuery
  = MR.Query
  ⨁ PR.Query
  ⨁ HEC.EChartsQuery
  ⨁ IR.Query
  ⨁ SR.Query
  ⨁ GR.Query
  ⨁ Const Void

type ChildSlot
  = Unit
  ⊹ Unit
  ⊹ Unit
  ⊹ Unit
  ⊹ Unit
  ⊹ Unit
  ⊹ Void

type Path a = CP.ChildPath a ChildQuery Unit ChildSlot

cpMetric ∷ Path MR.Query
cpMetric = CP.cp1

cpPivotTable ∷ Path PR.Query
cpPivotTable = CP.cp2

cpECharts ∷ Path HEC.EChartsQuery
cpECharts = CP.cp3

cpInput ∷ Path IR.Query
cpInput = CP.cp4

cpSelect ∷ Path SR.Query
cpSelect = CP.cp5

cpGeo ∷ Path GR.Query
cpGeo = CP.cp6
