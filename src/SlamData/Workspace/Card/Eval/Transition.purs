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

module SlamData.Workspace.Card.Eval.Transition
  ( Eval(..)
  , tagEval
  ) where

import SlamData.Prelude

import Quasar.Types (SQL)

import SlamData.FileSystem.Resource as R
import SlamData.Workspace.Card.Setups.Chart.Metric.Model as BuildMetric
import SlamData.Workspace.Card.Setups.Chart.Sankey.Model as BuildSankey
import SlamData.Workspace.Card.Setups.Chart.Gauge.Model as BuildGauge
import SlamData.Workspace.Card.Setups.Chart.Graph.Model as BuildGraph
import SlamData.Workspace.Card.Setups.Chart.Pie.Model as BuildPie
import SlamData.Workspace.Card.Setups.Chart.Radar.Model as BuildRadar
import SlamData.Workspace.Card.Setups.Chart.Area.Model as BuildArea
import SlamData.Workspace.Card.Setups.Chart.Line.Model as BuildLine
import SlamData.Workspace.Card.Setups.Chart.Bar.Model as BuildBar
import SlamData.Workspace.Card.Setups.Chart.Scatter.Model as BuildScatter
import SlamData.Workspace.Card.Setups.Chart.Funnel.Model as BuildFunnel
import SlamData.Workspace.Card.Setups.Chart.Heatmap.Model as BuildHeatmap
import SlamData.Workspace.Card.Setups.Chart.Boxplot.Model as BuildBoxplot
import SlamData.Workspace.Card.Setups.Chart.PivotTable.Model as BuildPivotTable
import SlamData.Workspace.Card.DownloadOptions.Component.State as Download
import SlamData.Workspace.Card.Markdown.Model as Markdown
import SlamData.Workspace.Card.Variables.Model as Variables
import SlamData.Workspace.Card.Setups.Chart.PunchCard.Eval as BuildPunchCard
import SlamData.Workspace.Card.Setups.Chart.Candlestick.Eval as BuildCandlestick
import SlamData.Workspace.Card.Setups.Chart.Parallel.Eval as BuildParallel
import SlamData.Workspace.Card.Setups.FormInput.Labeled.Model as SetupLabeled
import SlamData.Workspace.Card.Setups.FormInput.TextLike.Model as SetupTextLike
import SlamData.Workspace.Card.Setups.FormInput.Static.Model as SetupStatic
import SlamData.Workspace.Card.FormInput.Model as FormInput

data Eval
  = Pass
  | Composite
  | Terminal
  | Chart
  | Query SQL
  | Search String
  | Cache (Maybe String)
  | Error String
  | Markdown String
  | MarkdownForm Markdown.Model
  | Open R.Resource
  | Variables Variables.Model
  | DownloadOptions Download.State
  | BuildMetric BuildMetric.Model
  | BuildSankey BuildSankey.Model
  | BuildGauge BuildGauge.Model
  | BuildGraph BuildGraph.Model
  | BuildPie BuildPie.Model
  | BuildRadar BuildRadar.Model
  | BuildArea BuildArea.Model
  | BuildLine BuildLine.Model
  | BuildBar BuildBar.Model
  | BuildScatter BuildScatter.Model
  | BuildFunnel BuildFunnel.Model
  | BuildHeatmap BuildHeatmap.Model
  | BuildBoxplot BuildBoxplot.Model
  | BuildPivotTable BuildPivotTable.Model
  | BuildPunchCard BuildPunchCard.Model
  | BuildCandlestick BuildCandlestick.Model
  | BuildParallel BuildParallel.Model
  | SetupDropdown SetupLabeled.Model
  | SetupRadio SetupLabeled.Model
  | SetupCheckbox SetupLabeled.Model
  | SetupText SetupTextLike.Model
  | SetupNumeric SetupTextLike.Model
  | SetupDate SetupTextLike.Model
  | SetupTime SetupTextLike.Model
  | SetupDatetime SetupTextLike.Model
  | SetupStatic SetupStatic.Model
  | FormInput FormInput.Model

tagEval ∷ Eval → String
tagEval = case _ of
  Pass → "Pass"
  Composite → "Composite"
  Terminal → "Terminal"
  Chart → "Chart"
  Query str → "Query " <> show str
  Search str → "Search " <> show str
  Cache str → "Cache " <> show str
  Error str → "Error " <> show str
  Markdown str → "Markdown " <> show str
  Open res → "Open " <> show res
  MarkdownForm m → "MarkdownForm"
  Variables m → "Variables"
  DownloadOptions m → "DownloadOptions"
  BuildMetric _ → "BuildMetric"
  BuildSankey _ → "BuildSankey"
  BuildGauge _ → "BuildGauge"
  BuildGraph _ → "BuildGraph"
  BuildPie _ → "BuildPie"
  BuildRadar _ → "BuildRadar"
  BuildArea _ → "BuildArea"
  BuildLine _ → "BuildLine"
  BuildBar _ → "BuildBar"
  BuildScatter _ → "BuildScatter"
  BuildFunnel _ → "BuildFunnel"
  BuildHeatmap _ → "BuildHeatmap"
  BuildBoxplot _ → "BuildBoxplot"
  BuildPivotTable _ → "BuildPivotTable"
  BuildPunchCard _ → "BuildPunchCard"
  BuildCandlestick _ → "BuildCandlestick"
  BuildParallel _ → "BuildParallel"
  SetupDropdown _ → "SetupDropdown"
  SetupRadio _ → "SetupRadio"
  SetupCheckbox _ → "SetupCheckbox"
  SetupText _ → "SetupText"
  SetupNumeric _ → "SetupNumeric"
  SetupDate _ → "SetupDate"
  SetupTime _ → "SetupTime"
  SetupDatetime _ → "SetupDatetime"
  SetupStatic _ → "SetupStatic"
  FormInput _ → "FormInput"
