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

module SlamData.Workspace.Card.Component.Query
  ( CardQuery(..)
  , CardQueryP
  , InnerCardQuery
  , _CardEvalQuery
  , _AnyCardQuery
  , AnyCardQuery(..)
  , _AceQuery
  , _MarkdownQuery
  , _SearchQuery
  , _TableQuery
  , _ChartQuery
  , _DownloadQuery
  , _VariablesQuery
  , _TroubleshootQuery
  , _CacheQuery
  , _OpenQuery
  , _DownloadOptionsQuery
  , _DraftboardQuery
  , _BuildMetricQuery
  , _BuildSankeyQuery
  , _BuildGaugeQuery
  , _BuildGraphQuery
  , _BuildPieQuery
  , _BuildBarQuery
  , _BuildLineQuery
  , _BuildAreaQuery
  , _BuildScatterQuery
  , _BuildRadarQuery
  , _BuildPivotTableQuery
  , _BuildFunnelQuery
  , _BuildBoxplotQuery
  , _BuildHeatmapQuery
  , _BuildPunchCardQuery
  , _BuildCandlestickQuery
  , _BuildParallelQuery
  , _SetupDropdownQuery
  , _SetupRadioQuery
  , _SetupCheckboxQuery
  , _SetupTextQuery
  , _SetupNumericQuery
  , _SetupDateQuery
  , _SetupTimeQuery
  , _SetupDatetimeQuery
  , _SetupStaticQuery
  , _FormInputQuery
  , _TabsQuery
  , module SlamData.Workspace.Card.Common.EvalQuery
  ) where

import SlamData.Prelude

import Data.Lens (Prism', prism')
import Data.Lens.Prism.Coproduct (_Left, _Right)

import DOM.HTML.Types (HTMLElement)

import Halogen (ChildF)

import SlamData.Workspace.Card.Ace.Component.Query as Ace
import SlamData.Workspace.Card.Variables.Component.Query as Variables
import SlamData.Workspace.Card.Troubleshoot.Component.Query as Troubleshoot
import SlamData.Workspace.Card.Cache.Component.Query as Cache
import SlamData.Workspace.Card.Chart.Component.Query as Chart
import SlamData.Workspace.Card.Common.EvalQuery (CardEvalQuery)
import SlamData.Workspace.Card.Download.Component.Query as Download
import SlamData.Workspace.Card.DownloadOptions.Component.Query as DOpts
import SlamData.Workspace.Card.Draftboard.Component.Query as Draftboard
import SlamData.Workspace.Card.Table.Component.Query as Table
import SlamData.Workspace.Card.Markdown.Component.Query as Markdown
import SlamData.Workspace.Card.Open.Component.Query as Open
import SlamData.Workspace.Card.Search.Component.Query as Search
import SlamData.Workspace.Card.Setups.Chart.Metric.Component.Query as BuildMetric
import SlamData.Workspace.Card.Setups.Chart.Sankey.Component.Query as BuildSankey
import SlamData.Workspace.Card.Setups.Chart.Gauge.Component.Query as BuildGauge
import SlamData.Workspace.Card.Setups.Chart.Graph.Component.Query as BuildGraph
import SlamData.Workspace.Card.Setups.Chart.Pie.Component.Query as BuildPie
import SlamData.Workspace.Card.Setups.Chart.Bar.Component.Query as BuildBar
import SlamData.Workspace.Card.Setups.Chart.Line.Component.Query as BuildLine
import SlamData.Workspace.Card.Setups.Chart.Area.Component.Query as BuildArea
import SlamData.Workspace.Card.Setups.Chart.Scatter.Component.Query as BuildScatter
import SlamData.Workspace.Card.Setups.Chart.Radar.Component.Query as BuildRadar
import SlamData.Workspace.Card.Setups.Chart.PivotTable.Component.Query as BuildPivotTable
import SlamData.Workspace.Card.Setups.Chart.Funnel.Component.Query as BuildFunnel
import SlamData.Workspace.Card.Setups.Chart.Boxplot.Component.Query as BuildBoxplot
import SlamData.Workspace.Card.Setups.Chart.Heatmap.Component.Query as BuildHeatmap
import SlamData.Workspace.Card.Setups.Chart.PunchCard.Component.Query as BuildPunchCard
import SlamData.Workspace.Card.Setups.Chart.Candlestick.Component.Query as BuildCandlestick
import SlamData.Workspace.Card.Setups.Chart.Parallel.Component.Query as BuildParallel
import SlamData.Workspace.Eval.Card as Card
import SlamData.Workspace.Card.Setups.FormInput.Labeled.Component.Query as SetupLabeled
import SlamData.Workspace.Card.Setups.FormInput.TextLike.Component.Query as SetupTextLike
import SlamData.Workspace.Card.Setups.FormInput.Static.Component.Query as SetupStatic
import SlamData.Workspace.Card.FormInput.Component.Query as FormInput
import SlamData.Workspace.Card.Tabs.Component.Query as Tabs

-- | The common query algebra for a card.
data CardQuery a
  = Initialize a
  | Finalize a
  | ActivateCard a
  | DeactivateCard a
  | UpdateDimensions a
  | SetElement (Maybe HTMLElement) a
  | HandleEvalMessage (Card.EvalMessage) a

type CardQueryP = Coproduct CardQuery (ChildF Unit InnerCardQuery)

type InnerCardQuery = Coproduct CardEvalQuery AnyCardQuery

_CardEvalQuery ∷ ∀ a. Prism' (InnerCardQuery a) (CardEvalQuery a)
_CardEvalQuery = _Left

_AnyCardQuery ∷ ∀ a. Prism' (InnerCardQuery a) (AnyCardQuery a)
_AnyCardQuery = _Right

data AnyCardQuery a
  = AceQuery (Ace.QueryP a)
  | MarkdownQuery (Markdown.QueryP a)
  | SearchQuery (Search.Query a)
  | TableQuery (Table.QueryP a)
  | ChartQuery (Chart.QueryP a)
  | DownloadQuery (Download.QueryP a)
  | VariablesQuery (Variables.QueryP a)
  | TroubleshootQuery (Troubleshoot.QueryP a)
  | CacheQuery (Cache.QueryP a)
  | OpenQuery (Open.QueryP a)
  | DownloadOptionsQuery (DOpts.QueryP a)
  | DraftboardQuery (Draftboard.QueryP a)
  | BuildMetricQuery (BuildMetric.QueryP a)
  | BuildSankeyQuery (BuildSankey.QueryP a)
  | BuildGaugeQuery (BuildGauge.QueryP a)
  | BuildGraphQuery (BuildGraph.QueryP a)
  | BuildPieQuery (BuildPie.QueryP a)
  | BuildBarQuery (BuildBar.QueryP a)
  | BuildLineQuery (BuildLine.QueryP a)
  | BuildAreaQuery (BuildArea.QueryP a)
  | BuildScatterQuery (BuildScatter.QueryP a)
  | BuildRadarQuery (BuildRadar.QueryP a)
  | BuildPivotTableQuery (BuildPivotTable.QueryP a)
  | BuildFunnelQuery (BuildFunnel.QueryP a)
  | BuildBoxplotQuery (BuildBoxplot.QueryP a)
  | BuildHeatmapQuery (BuildHeatmap.QueryP a)
  | BuildPunchCardQuery (BuildPunchCard.QueryP a)
  | BuildCandlestickQuery (BuildCandlestick.QueryP a)
  | BuildParallelQuery (BuildParallel.QueryP a)
  | SetupDropdownQuery (SetupLabeled.QueryP a)
  | SetupRadioQuery (SetupLabeled.QueryP a)
  | SetupCheckboxQuery (SetupLabeled.QueryP a)
  | SetupTextQuery (SetupTextLike.QueryP a)
  | SetupNumericQuery (SetupTextLike.QueryP a)
  | SetupDateQuery (SetupTextLike.QueryP a)
  | SetupTimeQuery (SetupTextLike.QueryP a)
  | SetupDatetimeQuery (SetupTextLike.QueryP a)
  | SetupStaticQuery (SetupStatic.QueryP a)
  | FormInputQuery (FormInput.QueryP a)
  | TabsQuery (Tabs.QueryP a)

_AceQuery ∷ ∀ a. Prism' (AnyCardQuery a) (Ace.QueryP a)
_AceQuery = prism' AceQuery case _ of
  AceQuery q → Just q
  _ → Nothing

_MarkdownQuery ∷ ∀ a. Prism' (AnyCardQuery a) (Markdown.QueryP a)
_MarkdownQuery = prism' MarkdownQuery case _ of
  MarkdownQuery q → Just q
  _ → Nothing

_SearchQuery ∷ ∀ a. Prism' (AnyCardQuery a) (Search.Query a)
_SearchQuery = prism' SearchQuery case _ of
  SearchQuery q → Just q
  _ → Nothing

_TableQuery ∷ ∀ a. Prism' (AnyCardQuery a) (Table.QueryP a)
_TableQuery = prism' TableQuery case _ of
  TableQuery q → Just q
  _ → Nothing

_ChartQuery ∷ ∀ a. Prism' (AnyCardQuery a) (Chart.QueryP a)
_ChartQuery = prism' ChartQuery case _ of
  ChartQuery q → Just q
  _ → Nothing

_DownloadQuery ∷ ∀ a. Prism' (AnyCardQuery a) (Download.QueryP a)
_DownloadQuery = prism' DownloadQuery case _ of
  DownloadQuery q → Just q
  _ → Nothing

_VariablesQuery ∷ ∀ a. Prism' (AnyCardQuery a) (Variables.QueryP a)
_VariablesQuery = prism' VariablesQuery case _ of
  VariablesQuery q → Just q
  _ → Nothing

_TroubleshootQuery ∷ ∀ a. Prism' (AnyCardQuery a) (Troubleshoot.QueryP a)
_TroubleshootQuery = prism' TroubleshootQuery case _ of
  TroubleshootQuery q → Just q
  _ → Nothing

_CacheQuery ∷ ∀ a. Prism' (AnyCardQuery a) (Cache.QueryP a)
_CacheQuery = prism' CacheQuery case _ of
  CacheQuery q → Just q
  _ → Nothing

_OpenQuery ∷ ∀ a. Prism' (AnyCardQuery a) (Open.QueryP a)
_OpenQuery = prism' OpenQuery case _ of
  OpenQuery q → Just q
  _ → Nothing

_DownloadOptionsQuery ∷ ∀ a. Prism' (AnyCardQuery a) (DOpts.QueryP a)
_DownloadOptionsQuery = prism' DownloadOptionsQuery case _ of
  DownloadOptionsQuery q → Just q
  _ → Nothing

_DraftboardQuery ∷ ∀ a. Prism' (AnyCardQuery a) (Draftboard.QueryP a)
_DraftboardQuery = prism' DraftboardQuery case _ of
  DraftboardQuery q → Just q
  _ → Nothing

_BuildMetricQuery ∷ ∀ a. Prism' (AnyCardQuery a) (BuildMetric.QueryP a)
_BuildMetricQuery = prism' BuildMetricQuery case _ of
  BuildMetricQuery q → Just q
  _ → Nothing

_BuildSankeyQuery ∷ ∀ a. Prism' (AnyCardQuery a) (BuildSankey.QueryP a)
_BuildSankeyQuery = prism' BuildSankeyQuery case _ of
  BuildSankeyQuery q → Just q
  _ → Nothing

_BuildGaugeQuery ∷ ∀ a. Prism' (AnyCardQuery a) (BuildGauge.QueryP a)
_BuildGaugeQuery = prism' BuildGaugeQuery case _ of
  BuildGaugeQuery q → Just q
  _ → Nothing

_BuildGraphQuery ∷ ∀ a. Prism' (AnyCardQuery a) (BuildGraph.QueryP a)
_BuildGraphQuery = prism' BuildGraphQuery case _ of
  BuildGraphQuery q → Just q
  _ → Nothing

_BuildPieQuery ∷ ∀ a. Prism' (AnyCardQuery a) (BuildPie.QueryP a)
_BuildPieQuery = prism' BuildPieQuery case _ of
  BuildPieQuery q → Just q
  _ → Nothing

_BuildBarQuery ∷ ∀ a. Prism' (AnyCardQuery a) (BuildBar.QueryP a)
_BuildBarQuery = prism' BuildBarQuery case _ of
  BuildBarQuery q → Just q
  _ → Nothing

_BuildLineQuery ∷ ∀ a. Prism' (AnyCardQuery a) (BuildLine.QueryP a)
_BuildLineQuery = prism' BuildLineQuery case _ of
  BuildLineQuery q → Just q
  _ → Nothing

_BuildAreaQuery ∷ ∀ a. Prism' (AnyCardQuery a) (BuildArea.QueryP a)
_BuildAreaQuery = prism' BuildAreaQuery case _ of
  BuildAreaQuery q → Just q
  _ → Nothing

_BuildScatterQuery ∷ ∀ a. Prism' (AnyCardQuery a) (BuildScatter.QueryP a)
_BuildScatterQuery = prism' BuildScatterQuery case _ of
  BuildScatterQuery q → Just q
  _ → Nothing

_BuildRadarQuery ∷ ∀ a. Prism' (AnyCardQuery a) (BuildRadar.QueryP a)
_BuildRadarQuery = prism' BuildRadarQuery case _ of
  BuildRadarQuery q → Just q
  _ → Nothing

_BuildPivotTableQuery ∷ ∀ a. Prism' (AnyCardQuery a) (BuildPivotTable.QueryP a)
_BuildPivotTableQuery = prism' BuildPivotTableQuery case _ of
  BuildPivotTableQuery q → Just q
  _ → Nothing

_BuildFunnelQuery ∷ ∀ a. Prism' (AnyCardQuery a) (BuildFunnel.QueryP a)
_BuildFunnelQuery = prism' BuildFunnelQuery case _ of
  BuildFunnelQuery q → Just q
  _ → Nothing

_BuildBoxplotQuery ∷ ∀ a. Prism' (AnyCardQuery a) (BuildBoxplot.QueryP a)
_BuildBoxplotQuery = prism' BuildBoxplotQuery case _ of
  BuildBoxplotQuery q → Just q
  _ → Nothing

_BuildHeatmapQuery ∷ ∀ a. Prism' (AnyCardQuery a) (BuildHeatmap.QueryP a)
_BuildHeatmapQuery = prism' BuildHeatmapQuery case _ of
  BuildHeatmapQuery q → Just q
  _ → Nothing

_BuildPunchCardQuery ∷ ∀ a. Prism' (AnyCardQuery a) (BuildPunchCard.QueryP a)
_BuildPunchCardQuery = prism' BuildPunchCardQuery case _ of
  BuildPunchCardQuery q → Just q
  _ → Nothing

_BuildCandlestickQuery ∷ ∀ a. Prism' (AnyCardQuery a) (BuildCandlestick.QueryP a)
_BuildCandlestickQuery = prism' BuildCandlestickQuery case _ of
  BuildCandlestickQuery q → Just q
  _ → Nothing

_BuildParallelQuery ∷ ∀ a. Prism' (AnyCardQuery a) (BuildParallel.QueryP a)
_BuildParallelQuery = prism' BuildParallelQuery case _ of
  BuildParallelQuery q → Just q
  _ → Nothing

_SetupDropdownQuery ∷ ∀ a. Prism' (AnyCardQuery a) (SetupLabeled.QueryP a)
_SetupDropdownQuery = prism' SetupDropdownQuery case _ of
  SetupDropdownQuery q → Just q
  _ → Nothing

_SetupRadioQuery ∷ ∀ a. Prism' (AnyCardQuery a) (SetupLabeled.QueryP a)
_SetupRadioQuery = prism' SetupRadioQuery case _ of
  SetupRadioQuery q → Just q
  _ → Nothing

_SetupCheckboxQuery ∷ ∀ a. Prism' (AnyCardQuery a) (SetupLabeled.QueryP a)
_SetupCheckboxQuery = prism' SetupCheckboxQuery case _ of
  SetupCheckboxQuery q → Just q
  _ → Nothing

_SetupTextQuery ∷ ∀ a. Prism' (AnyCardQuery a) (SetupTextLike.QueryP a)
_SetupTextQuery = prism' SetupTextQuery case _ of
  SetupTextQuery q → Just q
  _ → Nothing

_SetupNumericQuery ∷ ∀ a. Prism' (AnyCardQuery a) (SetupTextLike.QueryP a)
_SetupNumericQuery = prism' SetupNumericQuery case _ of
  SetupNumericQuery q → Just q
  _ → Nothing

_SetupDateQuery ∷ ∀ a. Prism' (AnyCardQuery a) (SetupTextLike.QueryP a)
_SetupDateQuery = prism' SetupDateQuery case _ of
  SetupDateQuery q → Just q
  _ → Nothing

_SetupTimeQuery ∷ ∀ a. Prism' (AnyCardQuery a) (SetupTextLike.QueryP a)
_SetupTimeQuery = prism' SetupTimeQuery case _ of
  SetupTimeQuery q → Just q
  _ → Nothing

_SetupDatetimeQuery ∷ ∀ a. Prism' (AnyCardQuery a) (SetupTextLike.QueryP a)
_SetupDatetimeQuery = prism' SetupDatetimeQuery case _ of
  SetupDatetimeQuery q → Just q
  _ → Nothing

_SetupStaticQuery ∷ ∀ a. Prism' (AnyCardQuery a) (SetupStatic.QueryP a)
_SetupStaticQuery = prism' SetupStaticQuery case _ of
  SetupStaticQuery q → Just q
  _ → Nothing

_FormInputQuery ∷ ∀ a. Prism' (AnyCardQuery a) (FormInput.QueryP a)
_FormInputQuery = prism' FormInputQuery case _ of
  FormInputQuery q → Just q
  _ → Nothing

_TabsQuery ∷ ∀ a. Prism' (AnyCardQuery a) (Tabs.QueryP a)
_TabsQuery = prism' TabsQuery case _ of
  TabsQuery q → Just q
  _ → Nothing
