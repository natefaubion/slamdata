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

module SlamData.Workspace.Card.CardType.Chart where

import SlamData.Prelude

import Data.Variant (inj, on)
import Halogen.HTML as H
import SlamData.Render.Icon as I

_pie = SProxy ∷ SProxy "pie"
_line = SProxy ∷ SProxy "line"
_bar = SProxy ∷ SProxy "bar"
_area = SProxy ∷ SProxy "area"
_scatter = SProxy ∷ SProxy "scatter"
_radar = SProxy ∷ SProxy "radar"
_funnel = SProxy ∷ SProxy "funnel"
_graph = SProxy ∷ SProxy "graph"
_heatmap = SProxy ∷ SProxy "heatmap"
_sankey = SProxy ∷ SProxy "sankey"
_gauge = SProxy ∷ SProxy "gauge"
_boxplot = SProxy ∷ SProxy "boxplot"
_metric = SProxy ∷ SProxy "metric"
_punchCard = SProxy ∷ SProxy "punchCard"
_candlestick = SProxy ∷ SProxy "candlestick"
_parallel = SProxy ∷ SProxy "parallel"
_pivot = SProxy ∷ SProxy "pivot"

type ChartR r =
  ( pie ∷ Unit
  , line ∷ Unit
  , bar ∷ Unit
  , area ∷ Unit
  , scatter ∷ Unit
  , radar ∷ Unit
  , funnel ∷ Unit
  , graph ∷ Unit
  , heatmap ∷ Unit
  , sankey ∷ Unit
  , gauge ∷ Unit
  , boxplot ∷ Unit
  , metric ∷ Unit
  , punchCard ∷ Unit
  , candlestick ∷ Unit
  , parallel ∷ Unit
  , pivot ∷ Unit
  | r)

type Chart r = Variant (ChartR r)

all ∷ ∀ r. Array (Chart r)
all =
  [ pivot
  , pie
  , line
  , bar
  , area
  , scatter
  , radar
  , funnel
  , graph
  , heatmap
  , sankey
  , gauge
  , boxplot
  , metric
  , punchCard
  , candlestick
  , parallel
  ]

pivot ∷ ∀ r. Variant (pivot ∷ Unit|r)
pivot = inj _pivot unit

pie ∷ ∀ r. Variant (pie ∷ Unit|r)
pie = inj _pie unit

line ∷ ∀ r. Variant (line ∷ Unit|r)
line = inj _line unit

bar ∷ ∀ r. Variant (bar ∷ Unit|r)
bar = inj _bar unit

area ∷ ∀ r. Variant (area ∷ Unit|r)
area = inj _area unit

scatter ∷ ∀ r. Variant (scatter ∷ Unit|r)
scatter = inj _scatter unit

radar ∷ ∀ r. Variant (radar ∷ Unit|r)
radar = inj _radar unit

funnel ∷ ∀ r. Variant (funnel ∷ Unit|r)
funnel = inj _funnel unit

graph ∷ ∀ r. Variant (graph ∷ Unit|r)
graph = inj _graph unit

heatmap ∷ ∀ r. Variant (heatmap ∷ Unit|r)
heatmap = inj _heatmap unit

sankey ∷ ∀ r. Variant (sankey ∷ Unit|r)
sankey = inj _sankey unit

gauge ∷ ∀ r. Variant (gauge ∷ Unit|r)
gauge = inj _gauge unit

boxplot ∷ ∀ r. Variant (boxplot ∷ Unit|r)
boxplot = inj _boxplot unit

metric ∷ ∀ r. Variant (metric ∷ Unit|r)
metric = inj _metric unit

punchCard ∷ ∀ r. Variant (punchCard ∷ Unit|r)
punchCard = inj _punchCard unit

candlestick ∷ ∀ r. Variant (candlestick ∷ Unit|r)
candlestick = inj _candlestick unit

parallel ∷ ∀ r. Variant (parallel ∷ Unit|r)
parallel = inj _parallel unit

print ∷ ∀ r. (Variant r → String) → Chart r → String
print cb = cb
  # on _pie (const "pie")
  # on _line (const "line")
  # on _bar (const "bar")
  # on _area (const "area")
  # on _scatter (const "scatter")
  # on _radar (const "radar")
  # on _funnel (const "funnel")
  # on _graph (const "graph")
  # on _heatmap (const "heatmap")
  # on _sankey (const "sankey")
  # on _gauge (const "gauge")
  # on _boxplot (const "boxplot")
  # on _metric (const "metric")
  # on _punchCard (const "punch-card")
  # on _candlestick (const "candlestick")
  # on _parallel (const "parallel")
  # on _pivot (const "pivot")

encode ∷ ∀ r. (Variant r → String) → Chart r → String
encode cb = cb
  # on _pie (const "pie-options")
  # on _line (const "line-options")
  # on _bar (const "bar-options")
  # on _area (const "area-options")
  # on _scatter (const "scatter-options")
  # on _radar (const "radar-options")
  # on _funnel (const "funnel-options")
  # on _graph (const "graph-options")
  # on _heatmap (const "heatmap-options")
  # on _sankey (const "sankey-options")
  # on _gauge (const "gauge-options")
  # on _boxplot (const "boxplot-options")
  # on _metric (const "metric-options")
  # on _punchCard (const "punch-card-options")
  # on _candlestick (const "candlestick-options")
  # on _parallel (const "parallel-options")
  # on _pivot (const "pivot-options")

icon ∷ ∀ r. (Variant r → I.IconHTML) → Chart r → I.IconHTML
icon cb = cb
  # on _pie (const $ I.IconHTML I.buildChartPie)
  # on _line (const $ I.IconHTML I.buildChartLine)
  # on _bar (const $ I.IconHTML I.buildChartBar)
  # on _area (const $ I.IconHTML I.buildChartArea)
  # on _scatter (const $ I.IconHTML I.buildChartScatter)
  # on _radar (const $ I.IconHTML I.buildChartRadar)
  # on _funnel (const $ I.IconHTML I.buildChartFunnel)
  # on _graph (const $ I.IconHTML I.buildChartGraph)
  # on _heatmap (const $ I.IconHTML I.buildChartHeatmap)
  # on _sankey (const $ I.IconHTML I.buildChartSankey)
  # on _gauge (const $ I.IconHTML I.buildChartGauge)
  # on _boxplot (const $ I.IconHTML I.buildChartBoxplot)
  # on _metric (const $ I.IconHTML I.buildChartMetric)
  # on _punchCard (const $ I.IconHTML I.buildChartPunchCard)
  # on _candlestick (const $ I.IconHTML I.buildChartCandlestick)
  # on _parallel (const $ I.IconHTML I.buildChartParallel)
  # on _pivot (const $ I.IconHTML I.buildChartPivotTable)


parse ∷ ∀ r. String → String ⊹ Chart r
parse = case _ of
  "pie" → Right pie
  "line" → Right line
  "bar" → Right bar
  "area" → Right area
  "scatter" → Right scatter
  "radar" → Right radar
  "funnel" → Right funnel
  "graph" → Right graph
  "heatmap" → Right heatmap
  "sankey" → Right sankey
  "gauge" → Right gauge
  "boxplot" → Right boxplot
  "metric" → Right metric
  "pivot" → Right pivot
  "punch-card" → Right punchCard
  "candlestick" → Right candlestick
  "parallel" → Right parallel
  "pivot-table" → Right pivot
  ty → Left $ ty ⊕ " is unknown chart type"

name ∷ ∀ r. (Variant r → String) → Chart r → String
name cb = cb
  # on _pie (const "Pie")
  # on _line (const "Line")
  # on _bar (const "Bar")
  # on _area (const "Area")
  # on _scatter (const "Scatter")
  # on _radar (const "Radar")
  # on _funnel (const "Funnel")
  # on _graph (const "Graph")
  # on _heatmap (const "Heatmap")
  # on _sankey (const "Sankey")
  # on _gauge (const "Gauge")
  # on _boxplot (const "Boxplot")
  # on _metric (const "Metric")
  # on _punchCard (const "Punch-card")
  # on _candlestick (const "Candlestick")
  # on _parallel (const "Parallel")
  # on _pivot (const "Pivot Table")

consumerInteractable ∷ ∀ r. (Variant r → Boolean) → Chart r → Boolean
consumerInteractable cb = cb
  # on _pie ff
  # on _line ff
  # on _bar ff
  # on _area ff
  # on _scatter ff
  # on _radar ff
  # on _funnel ff
  # on _graph ff
  # on _heatmap ff
  # on _sankey ff
  # on _gauge ff
  # on _boxplot ff
  # on _metric ff
  # on _punchCard ff
  # on _candlestick ff
  # on _parallel ff
  # on _pivot ff

cardClasses ∷ ∀ r. (Variant r → Array H.ClassName) → Chart r → Array H.ClassName
cardClasses cb = cb
  # on _pie clss
  # on _line clss
  # on _bar clss
  # on _area clss
  # on _scatter clss
  # on _radar clss
  # on _funnel clss
  # on _graph clss
  # on _heatmap clss
  # on _sankey clss
  # on _gauge clss
  # on _boxplot clss
  # on _metric clss
  # on _punchCard clss
  # on _candlestick clss
  # on _parallel clss
  # on _pivot clss
  where
  clss _ = [ H.ClassName "sd-card-chart-options" ]

contractToChart ∷ ∀ r. Contractable r (ChartR ()) ⇒ Variant r → Maybe (Chart ())
contractToChart = contract
