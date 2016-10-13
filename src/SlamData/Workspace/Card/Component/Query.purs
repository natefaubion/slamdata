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
  , _NextQuery
  , _CacheQuery
  , _OpenQuery
  , _DownloadOptionsQuery
  , _DraftboardQuery
  , _ErrorQuery
  , _PendingQuery
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
  , module SlamData.Workspace.Card.Common.EvalQuery
  ) where

import SlamData.Prelude

import Data.Lens (PrismP, prism')
import Data.Lens.Prism.Coproduct (_Left, _Right)

import DOM.HTML.Types (HTMLElement)

import Halogen (ChildF)

import SlamData.Workspace.Card.Ace.Component.Query as Ace
import SlamData.Workspace.Card.Variables.Component.Query as Variables
import SlamData.Workspace.Card.Troubleshoot.Component.Query as Troubleshoot
import SlamData.Workspace.Card.Cache.Component.Query as Cache
import SlamData.Workspace.Card.CardId (CardId)
import SlamData.Workspace.Card.CardType (CardType)
import SlamData.Workspace.Card.Chart.Component.Query as Chart
import SlamData.Workspace.Card.Common.EvalQuery (CardEvalInput, CardEvalQuery(..), ModelUpdateType(..), raiseUpdatedC, raiseUpdatedC', raiseUpdatedP, raiseUpdatedP')
import SlamData.Workspace.Card.Download.Component.Query as Download
import SlamData.Workspace.Card.DownloadOptions.Component.Query as DOpts
import SlamData.Workspace.Card.Draftboard.Component.Query as Draftboard
import SlamData.Workspace.Card.Error.Component.Query as Error
import SlamData.Workspace.Card.Table.Component.Query as Table
import SlamData.Workspace.Card.Markdown.Component.Query as Markdown
import SlamData.Workspace.Card.Model as Card
import SlamData.Workspace.Card.Next.Component.Query as Next
import SlamData.Workspace.Card.Open.Component.Query as Open
import SlamData.Workspace.Card.Pending.Component.Query as Pending
import SlamData.Workspace.Card.Port (Port)
import SlamData.Workspace.Card.Search.Component.Query as Search
import SlamData.Workspace.Card.BuildChart.Metric.Component.Query as BuildMetric
import SlamData.Workspace.Card.BuildChart.Sankey.Component.Query as BuildSankey
import SlamData.Workspace.Card.BuildChart.Gauge.Component.Query as BuildGauge
import SlamData.Workspace.Card.BuildChart.Graph.Component.Query as BuildGraph
import SlamData.Workspace.Card.BuildChart.Pie.Component.Query as BuildPie
import SlamData.Workspace.Card.BuildChart.Bar.Component.Query as BuildBar
import SlamData.Workspace.Card.BuildChart.Line.Component.Query as BuildLine
import SlamData.Workspace.Card.BuildChart.Area.Component.Query as BuildArea
import SlamData.Workspace.Card.BuildChart.Scatter.Component.Query as BuildScatter
import SlamData.Workspace.Card.BuildChart.Radar.Component.Query as BuildRadar
import SlamData.Workspace.Card.BuildChart.PivotTable.Component.Query as BuildPivotTable
import SlamData.Workspace.Card.BuildChart.Funnel.Component.Query as BuildFunnel
import SlamData.Workspace.Card.BuildChart.Boxplot.Component.Query as BuildBoxplot
import SlamData.Workspace.Card.BuildChart.Heatmap.Component.Query as BuildHeatmap

-- | The common query algebra for a card.
-- |
-- | - `UpdateCard` accepts an input value from a parent card if one is
-- |   required, performs any necessary actions to evalute the card and update
-- |   its state, and then returns its own output value.
-- | - `RefreshCard` is captured by the deck and goes to the root of the
-- |   current card's dependencies and updates the cards downwards from there.
data CardQuery a
  = UpdateCard CardEvalInput (Maybe Port) a
  | SaveCard CardId CardType (Card.Model → a)
  | ActivateCard a
  | DeactivateCard a
  | LoadCard Card.Model a
  | UpdateDimensions a
  | SetHTMLElement (Maybe HTMLElement) a

type CardQueryP = Coproduct CardQuery (ChildF Unit InnerCardQuery)

type InnerCardQuery = Coproduct CardEvalQuery AnyCardQuery

_CardEvalQuery ∷ ∀ a. PrismP (InnerCardQuery a) (CardEvalQuery a)
_CardEvalQuery = _Left

_AnyCardQuery ∷ ∀ a. PrismP (InnerCardQuery a) (AnyCardQuery a)
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
  | NextQuery (Next.QueryP a)
  | CacheQuery (Cache.QueryP a)
  | OpenQuery (Open.QueryP a)
  | DownloadOptionsQuery (DOpts.QueryP a)
  | DraftboardQuery (Draftboard.QueryP a)
  | ErrorQuery (Error.QueryP a)
  | PendingQuery (Pending.QueryP a)
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


_AceQuery ∷ ∀ a. PrismP (AnyCardQuery a) (Ace.QueryP a)
_AceQuery = prism' AceQuery case _ of
  AceQuery q → Just q
  _ → Nothing

_MarkdownQuery ∷ ∀ a. PrismP (AnyCardQuery a) (Markdown.QueryP a)
_MarkdownQuery = prism' MarkdownQuery case _ of
  MarkdownQuery q → Just q
  _ → Nothing

_SearchQuery ∷ ∀ a. PrismP (AnyCardQuery a) (Search.Query a)
_SearchQuery = prism' SearchQuery case _ of
  SearchQuery q → Just q
  _ → Nothing

_TableQuery ∷ ∀ a. PrismP (AnyCardQuery a) (Table.QueryP a)
_TableQuery = prism' TableQuery case _ of
  TableQuery q → Just q
  _ → Nothing

_ChartQuery ∷ ∀ a. PrismP (AnyCardQuery a) (Chart.QueryP a)
_ChartQuery = prism' ChartQuery case _ of
  ChartQuery q → Just q
  _ → Nothing

_DownloadQuery ∷ ∀ a. PrismP (AnyCardQuery a) (Download.QueryP a)
_DownloadQuery = prism' DownloadQuery case _ of
  DownloadQuery q → Just q
  _ → Nothing

_VariablesQuery ∷ ∀ a. PrismP (AnyCardQuery a) (Variables.QueryP a)
_VariablesQuery = prism' VariablesQuery case _ of
  VariablesQuery q → Just q
  _ → Nothing

_TroubleshootQuery ∷ ∀ a. PrismP (AnyCardQuery a) (Troubleshoot.QueryP a)
_TroubleshootQuery = prism' TroubleshootQuery case _ of
  TroubleshootQuery q → Just q
  _ → Nothing

_NextQuery ∷ ∀ a. PrismP (AnyCardQuery a) (Next.QueryP a)
_NextQuery = prism' NextQuery case _ of
  NextQuery q → Just q
  _ → Nothing

_CacheQuery ∷ ∀ a. PrismP (AnyCardQuery a) (Cache.QueryP a)
_CacheQuery = prism' CacheQuery case _ of
  CacheQuery q → Just q
  _ → Nothing

_OpenQuery ∷ ∀ a. PrismP (AnyCardQuery a) (Open.QueryP a)
_OpenQuery = prism' OpenQuery case _ of
  OpenQuery q → Just q
  _ → Nothing

_DownloadOptionsQuery ∷ ∀ a. PrismP (AnyCardQuery a) (DOpts.QueryP a)
_DownloadOptionsQuery = prism' DownloadOptionsQuery case _ of
  DownloadOptionsQuery q → Just q
  _ → Nothing

_DraftboardQuery ∷ ∀ a. PrismP (AnyCardQuery a) (Draftboard.QueryP a)
_DraftboardQuery = prism' DraftboardQuery case _ of
  DraftboardQuery q → Just q
  _ → Nothing

_ErrorQuery ∷ ∀ a. PrismP (AnyCardQuery a) (Error.QueryP a)
_ErrorQuery = prism' ErrorQuery case _ of
  ErrorQuery q → Just q
  _ → Nothing

_PendingQuery ∷ ∀ a. PrismP (AnyCardQuery a) (Pending.QueryP a)
_PendingQuery = prism' PendingQuery case _ of
  PendingQuery q → Just q
  _ → Nothing

_BuildMetricQuery ∷ ∀ a. PrismP (AnyCardQuery a) (BuildMetric.QueryP a)
_BuildMetricQuery = prism' BuildMetricQuery case _ of
  BuildMetricQuery q → Just q
  _ → Nothing

_BuildSankeyQuery ∷ ∀ a. PrismP (AnyCardQuery a) (BuildSankey.QueryP a)
_BuildSankeyQuery = prism' BuildSankeyQuery case _ of
  BuildSankeyQuery q → Just q
  _ → Nothing

_BuildGaugeQuery ∷ ∀ a. PrismP (AnyCardQuery a) (BuildGauge.QueryP a)
_BuildGaugeQuery = prism' BuildGaugeQuery case _ of
  BuildGaugeQuery q → Just q
  _ → Nothing

_BuildGraphQuery ∷ ∀ a. PrismP (AnyCardQuery a) (BuildGraph.QueryP a)
_BuildGraphQuery = prism' BuildGraphQuery case _ of
  BuildGraphQuery q → Just q
  _ → Nothing

_BuildPieQuery ∷ ∀ a. PrismP (AnyCardQuery a) (BuildPie.QueryP a)
_BuildPieQuery = prism' BuildPieQuery case _ of
  BuildPieQuery q → Just q
  _ → Nothing

_BuildBarQuery ∷ ∀ a. PrismP (AnyCardQuery a) (BuildBar.QueryP a)
_BuildBarQuery = prism' BuildBarQuery case _ of
  BuildBarQuery q → Just q
  _ → Nothing

_BuildLineQuery ∷ ∀ a. PrismP (AnyCardQuery a) (BuildLine.QueryP a)
_BuildLineQuery = prism' BuildLineQuery case _ of
  BuildLineQuery q → Just q
  _ → Nothing

_BuildAreaQuery ∷ ∀ a. PrismP (AnyCardQuery a) (BuildArea.QueryP a)
_BuildAreaQuery = prism' BuildAreaQuery case _ of
  BuildAreaQuery q → Just q
  _ → Nothing

_BuildScatterQuery ∷ ∀ a. PrismP (AnyCardQuery a) (BuildScatter.QueryP a)
_BuildScatterQuery = prism' BuildScatterQuery case _ of
  BuildScatterQuery q → Just q
  _ → Nothing

_BuildRadarQuery ∷ ∀ a. PrismP (AnyCardQuery a) (BuildRadar.QueryP a)
_BuildRadarQuery = prism' BuildRadarQuery case _ of
  BuildRadarQuery q → Just q
  _ → Nothing

_BuildPivotTableQuery ∷ ∀ a. PrismP (AnyCardQuery a) (BuildPivotTable.QueryP a)
_BuildPivotTableQuery = prism' BuildPivotTableQuery case _ of
  BuildPivotTableQuery q → Just q
  _ → Nothing

_BuildFunnelQuery ∷ ∀ a. PrismP (AnyCardQuery a) (BuildFunnel.QueryP a)
_BuildFunnelQuery = prism' BuildFunnelQuery case _ of
  BuildFunnelQuery q → Just q
  _ → Nothing

_BuildBoxplotQuery ∷ ∀ a. PrismP (AnyCardQuery a) (BuildBoxplot.QueryP a)
_BuildBoxplotQuery = prism' BuildBoxplotQuery case _ of
  BuildBoxplotQuery q → Just q
  _ → Nothing

_BuildHeatmapQuery ∷ ∀ a. PrismP (AnyCardQuery a) (BuildHeatmap.QueryP a)
_BuildHeatmapQuery = prism' BuildHeatmapQuery case _ of
  BuildHeatmapQuery q → Just q
  _ → Nothing
