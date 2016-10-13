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
  ( CardEvalInput
  , Eval(..)
  , tagEval
  ) where

import SlamData.Prelude

import Data.Map as Map

import Quasar.Types (SQL)

import SlamData.FileSystem.Resource as R
import SlamData.Workspace.Card.BuildChart.Metric.Model as BuildMetric
import SlamData.Workspace.Card.BuildChart.Sankey.Model as BuildSankey
import SlamData.Workspace.Card.BuildChart.Gauge.Model as BuildGauge
import SlamData.Workspace.Card.BuildChart.Graph.Model as BuildGraph
import SlamData.Workspace.Card.BuildChart.Pie.Model as BuildPie
import SlamData.Workspace.Card.BuildChart.Radar.Model as BuildRadar
import SlamData.Workspace.Card.BuildChart.Area.Model as BuildArea
import SlamData.Workspace.Card.BuildChart.Line.Model as BuildLine
import SlamData.Workspace.Card.BuildChart.Bar.Model as BuildBar
import SlamData.Workspace.Card.BuildChart.Scatter.Model as BuildScatter
import SlamData.Workspace.Card.BuildChart.Funnel.Model as BuildFunnel
import SlamData.Workspace.Card.BuildChart.Heatmap.Model as BuildHeatmap
import SlamData.Workspace.Card.BuildChart.Boxplot.Model as BuildBoxplot
import SlamData.Workspace.Card.BuildChart.PivotTable.Model as BuildPivotTable
import SlamData.Workspace.Card.DownloadOptions.Component.State as Download
import SlamData.Workspace.Card.Markdown.Model as Markdown
import SlamData.Workspace.Card.Variables.Model as Variables
import SlamData.Workspace.Card.CardId as CID
import SlamData.Workspace.Card.Port as Port
import SlamData.Workspace.Deck.DeckId as DID
import Utils.Path (DirPath)

type CardEvalInput =
  { path ∷ DirPath
  , input ∷ Maybe Port.Port
  , cardCoord ∷ DID.DeckId × CID.CardId
  , urlVarMaps ∷ Map.Map DID.DeckId Port.URLVarMap
  }

data Eval
  = Pass
  | Query SQL
  | Search String
  | Cache (Maybe String)
  | Error String
  | Markdown String
  | MarkdownForm Markdown.Model
  | Open R.Resource
  | Variables Variables.Model
  | DownloadOptions Download.State
  | Draftboard
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

tagEval ∷ Eval → String
tagEval = case _ of
  Pass → "Pass"
  Query str → "Query " <> show str
  Search str → "Search " <> show str
  Cache str → "Cache " <> show str
  Error str → "Error " <> show str
  Markdown str → "Markdown " <> show str
  Open res → "Open " <> show res
  MarkdownForm m → "MarkdownForm"
  Variables m → "Variables"
  DownloadOptions m → "DownloadOptions"
  Draftboard → "Draftboard"
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
