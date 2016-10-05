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

module SlamData.Workspace.Card.Component.State
  ( CardState(..)
  , CardStateP
  , initialCardState
  , _element
  , AnyCardState
  , _AceState
  , _MarkdownState
  , _SearchState
  , _TableState
  , _ChartState
  , _DownloadState
  , _VariablesState
  , _TroubleshootState
  , _NextState
  , _CacheState
  , _OpenState
  , _DownloadOptionsState
  , _DraftboardState
  , _ErrorState
  , _PendingState
  , _BuildMetricState
  , _BuildSankeyState
  , _BuildGaugeState
  , _BuildGraphState
  , _BuildPieState
  , _BuildBarState
  , _BuildLineState
  , _BuildAreaState
  , _BuildScatterState
  , _BuildRadarState
  , _BuildPivotTableState
  , _BuildFunnelState
  , _BuildBoxplotState
  , _BuildHeatmapState
  ) where

import SlamData.Prelude

import Data.Lens (LensP, lens, PrismP, prism')

import DOM.HTML.Types (HTMLElement)

import Halogen (ParentState)

import SlamData.Monad (Slam)
import SlamData.Workspace.Card.Ace.Component.State as Ace
import SlamData.Workspace.Card.Variables.Component.State as Variables
import SlamData.Workspace.Card.Troubleshoot.Component.State as Troubleshoot
import SlamData.Workspace.Card.Cache.Component.State as Cache
import SlamData.Workspace.Card.Chart.Component.State as Chart
import SlamData.Workspace.Card.Component.Query (CardQuery, InnerCardQuery)
import SlamData.Workspace.Card.Download.Component.State as Download
import SlamData.Workspace.Card.DownloadOptions.Component.State as DOpts
import SlamData.Workspace.Card.Draftboard.Component.State as Draftboard
import SlamData.Workspace.Card.Error.Component.State as Error
import SlamData.Workspace.Card.Pending.Component.State as Pending
import SlamData.Workspace.Card.Table.Component.State as Table
import SlamData.Workspace.Card.Markdown.Component.State as Markdown
import SlamData.Workspace.Card.Next.Component.State as Next
import SlamData.Workspace.Card.Open.Component.State as Open
import SlamData.Workspace.Card.Search.Component.State as Search
import SlamData.Workspace.Card.BuildChart.Metric.Component.State as BuildMetric
import SlamData.Workspace.Card.BuildChart.Sankey.Component.State as BuildSankey
import SlamData.Workspace.Card.BuildChart.Gauge.Component.State as BuildGauge
import SlamData.Workspace.Card.BuildChart.Graph.Component.State as BuildGraph
import SlamData.Workspace.Card.BuildChart.Pie.Component.State as BuildPie
import SlamData.Workspace.Card.BuildChart.Bar.Component.State as BuildBar
import SlamData.Workspace.Card.BuildChart.Line.Component.State as BuildLine
import SlamData.Workspace.Card.BuildChart.Area.Component.State as BuildArea
import SlamData.Workspace.Card.BuildChart.Scatter.Component.State as BuildScatter
import SlamData.Workspace.Card.BuildChart.Funnel.Component.State as BuildFunnel
import SlamData.Workspace.Card.BuildChart.Radar.Component.State as BuildRadar
import SlamData.Workspace.Card.BuildChart.PivotTable.Component.State as BuildPivotTable
import SlamData.Workspace.Card.BuildChart.Boxplot.Component.State as BuildBoxplot
import SlamData.Workspace.Card.BuildChart.Heatmap.Component.State as BuildHeatmap


-- | The common state value for deck cards.
type CardState =
  { element ∷ Maybe HTMLElement
  }

type CardStateP = ParentState CardState AnyCardState CardQuery InnerCardQuery Slam Unit

-- | Creates an initial `CardState` value for an editor card.
initialCardState ∷ CardState
initialCardState =
  { element: Nothing
  }

_element ∷ LensP CardState (Maybe HTMLElement)
_element = lens _.element _{element = _}

data AnyCardState
  = AceState Ace.StateP
  | MarkdownState Markdown.StateP
  | SearchState Search.State
  | TableState Table.State
  | ChartState Chart.StateP
  | DownloadState Download.State
  | VariablesState Variables.StateP
  | TroubleshootState Troubleshoot.State
  | NextState Next.State
  | CacheState Cache.State
  | OpenState Open.StateP
  | DownloadOptionsState DOpts.State
  | DraftboardState Draftboard.StateP
  | ErrorState Error.State
  | PendingState Pending.State
  | BuildMetricState BuildMetric.StateP
  | BuildSankeyState BuildSankey.StateP
  | BuildGaugeState BuildGauge.StateP
  | BuildGraphState BuildGraph.StateP
  | BuildPieState BuildPie.StateP
  | BuildBarState BuildBar.StateP
  | BuildLineState BuildLine.StateP
  | BuildAreaState BuildArea.StateP
  | BuildScatterState BuildScatter.StateP
  | BuildFunnelState BuildFunnel.StateP
  | BuildRadarState BuildRadar.StateP
  | BuildPivotTableState BuildPivotTable.StateP
  | BuildBoxplotState BuildBoxplot.StateP
  | BuildHeatmapState BuildHeatmap.StateP

_AceState ∷ PrismP AnyCardState Ace.StateP
_AceState = prism' AceState case _ of
  AceState s → Just s
  _ → Nothing

_MarkdownState ∷ PrismP AnyCardState Markdown.StateP
_MarkdownState = prism' MarkdownState case _ of
  MarkdownState s → Just s
  _ → Nothing

_SearchState ∷ PrismP AnyCardState Search.State
_SearchState = prism' SearchState case _ of
  SearchState s → Just s
  _ → Nothing

_TableState ∷ PrismP AnyCardState Table.State
_TableState = prism' TableState case _ of
  TableState s → Just s
  _ → Nothing

_ChartState ∷ PrismP AnyCardState Chart.StateP
_ChartState = prism' ChartState case _ of
  ChartState s → Just s
  _ → Nothing

_DownloadState ∷ PrismP AnyCardState Download.State
_DownloadState = prism' DownloadState case _ of
  DownloadState s → Just s
  _ → Nothing

_VariablesState ∷ PrismP AnyCardState Variables.StateP
_VariablesState = prism' VariablesState case _ of
  VariablesState s → Just s
  _ → Nothing

_TroubleshootState ∷ PrismP AnyCardState Troubleshoot.State
_TroubleshootState = prism' TroubleshootState case _ of
  TroubleshootState s → Just s
  _ → Nothing

_NextState ∷ PrismP AnyCardState Next.State
_NextState = prism' NextState case _ of
  NextState s → Just s
  _ → Nothing

_CacheState ∷ PrismP AnyCardState Cache.State
_CacheState = prism' CacheState case _ of
  CacheState s → Just s
  _ → Nothing

_OpenState ∷ PrismP AnyCardState Open.StateP
_OpenState = prism' OpenState case _ of
  OpenState s → Just s
  _ → Nothing

_DownloadOptionsState ∷ PrismP AnyCardState DOpts.State
_DownloadOptionsState = prism' DownloadOptionsState case _ of
  DownloadOptionsState s → Just s
  _ → Nothing

_DraftboardState ∷ PrismP AnyCardState Draftboard.StateP
_DraftboardState = prism' DraftboardState case _ of
  DraftboardState s → Just s
  _ → Nothing

_ErrorState ∷ PrismP AnyCardState Error.State
_ErrorState = prism' ErrorState case _ of
  ErrorState s → Just s
  _ → Nothing

_PendingState ∷ PrismP AnyCardState Pending.State
_PendingState = prism' PendingState case _ of
  PendingState s → Just s
  _ → Nothing

_BuildMetricState ∷ PrismP AnyCardState BuildMetric.StateP
_BuildMetricState = prism' BuildMetricState case _ of
  BuildMetricState s → Just s
  _ → Nothing

_BuildSankeyState ∷ PrismP AnyCardState BuildSankey.StateP
_BuildSankeyState = prism' BuildSankeyState case _ of
  BuildSankeyState s → Just s
  _ → Nothing

_BuildGaugeState ∷ PrismP AnyCardState BuildGauge.StateP
_BuildGaugeState = prism' BuildGaugeState case _ of
  BuildGaugeState s → Just s
  _ → Nothing

_BuildGraphState ∷ PrismP AnyCardState BuildGraph.StateP
_BuildGraphState = prism' BuildGraphState case _ of
  BuildGraphState s → Just s
  _ → Nothing

_BuildPieState ∷ PrismP AnyCardState BuildPie.StateP
_BuildPieState = prism' BuildPieState case _ of
  BuildPieState s → Just s
  _ → Nothing

_BuildBarState ∷ PrismP AnyCardState BuildBar.StateP
_BuildBarState = prism' BuildBarState case _ of
  BuildBarState s → Just s
  _ → Nothing

_BuildLineState ∷ PrismP AnyCardState BuildLine.StateP
_BuildLineState = prism' BuildLineState case _ of
  BuildLineState s → Just s
  _ → Nothing

_BuildAreaState ∷ PrismP AnyCardState BuildArea.StateP
_BuildAreaState = prism' BuildAreaState case _ of
  BuildAreaState s → Just s
  _ → Nothing

_BuildScatterState ∷ PrismP AnyCardState BuildScatter.StateP
_BuildScatterState = prism' BuildScatterState case _ of
  BuildScatterState s → Just s
  _ → Nothing

_BuildRadarState ∷ PrismP AnyCardState BuildRadar.StateP
_BuildRadarState = prism' BuildRadarState case _ of
  BuildRadarState s → Just s
  _ → Nothing

_BuildPivotTableState ∷ PrismP AnyCardState BuildPivotTable.StateP
_BuildPivotTableState = prism' BuildPivotTableState case _ of
  BuildPivotTableState s → Just s
  _ → Nothing

_BuildFunnelState ∷ PrismP AnyCardState BuildFunnel.StateP
_BuildFunnelState = prism' BuildFunnelState case _ of
  BuildFunnelState s → Just s
  _ → Nothing

_BuildBoxplotState ∷ PrismP AnyCardState BuildBoxplot.StateP
_BuildBoxplotState = prism' BuildBoxplotState case _ of
  BuildBoxplotState s → Just s
  _ → Nothing

_BuildHeatmapState ∷ PrismP AnyCardState BuildHeatmap.StateP
_BuildHeatmapState = prism' BuildHeatmapState case _ of
  BuildHeatmapState s → Just s
  _ → Nothing
