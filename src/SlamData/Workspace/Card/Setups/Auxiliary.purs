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

module SlamData.Workspace.Card.Setups.Auxiliary where

import SlamData.Prelude

import Control.Alternative (class Alternative)

import Data.Argonaut as J
import Data.Codec as C
import Data.Codec.Argonaut as CA
import Data.Variant as V
import Data.Functor.Variant (VariantF, FProxy)
import Data.Newtype (under)
import Halogen as H
import Halogen.HTML as HH
import Halogen.Component.Profunctor as HPR
import Halogen.Component.Proxy as HCP
import SlamData.Workspace.Card.CardType as CT
import SlamData.Workspace.Card.CardType.VizType as VT
import SlamData.Workspace.Card.Setups.Auxiliary.Area as Area
import SlamData.Workspace.Card.Setups.Auxiliary.Bar as Bar
import SlamData.Workspace.Card.Setups.Auxiliary.Funnel as Funnel
import SlamData.Workspace.Card.Setups.Auxiliary.Gauge as Gauge
import SlamData.Workspace.Card.Setups.Auxiliary.Graph as Graph
import SlamData.Workspace.Card.Setups.Auxiliary.Heatmap as Heatmap
import SlamData.Workspace.Card.Setups.Auxiliary.Line as Line
import SlamData.Workspace.Card.Setups.Auxiliary.Metric as Metric
import SlamData.Workspace.Card.Setups.Auxiliary.PunchCard as PunchCard
import SlamData.Workspace.Card.Setups.Auxiliary.Scatter as Scatter
import SlamData.Workspace.Card.Setups.Auxiliary.GeoHeatmap as GeoHeatmap
import SlamData.Workspace.Card.Setups.Auxiliary.GeoMarker as GeoMarker
import SlamData.Workspace.Card.Setups.PivotTable.Model as PivotTable
import Test.StrongCheck.Gen as Gen

type State = Variant
  ( area ∷ Area.State
  , bar ∷ Bar.State
  , funnel ∷ Funnel.State
  , gauge ∷ Gauge.State
  , graph ∷ Graph.State
  , heatmap ∷ Heatmap.State
  , line ∷ Line.State
  , metric ∷ Metric.State
  , punchCard ∷ PunchCard.State
  , scatter ∷ Scatter.State
  , geoHeatmap ∷ GeoHeatmap.State
  , geoMarker ∷ GeoMarker.State
  , pivot ∷ PivotTable.Model
  )

eq_ ∷ State → State → Boolean
eq_ = V.case_
  # V.on CT._area (\r → V.on CT._area (Area.eq_ r) ff)
  # V.on CT._bar (\r → V.on CT._bar (Bar.eq_ r) ff)
  # V.on CT._funnel (\r → V.on CT._funnel (Funnel.eq_ r) ff)
  # V.on CT._gauge (\r → V.on CT._gauge (Gauge.eq_ r) ff)
  # V.on CT._graph (\r → V.on CT._graph (Graph.eq_ r) ff)
  # V.on CT._heatmap (\r → V.on CT._heatmap (Heatmap.eq_ r) ff)
  # V.on CT._line (\r → V.on CT._line (Line.eq_ r) ff)
  # V.on CT._metric (\r → V.on CT._metric (Metric.eq_ r) ff)
  # V.on CT._punchCard (\r → V.on CT._punchCard (PunchCard.eq_ r) ff)
  # V.on CT._scatter (\r → V.on CT._scatter (Scatter.eq_ r) ff)
  # V.on CT._geoHeatmap (\r → V.on CT._geoHeatmap (GeoHeatmap.eq_ r) ff)
  # V.on CT._geoMarker (\r → V.on CT._geoMarker (GeoMarker.eq_ r) ff)
  # V.on CT._pivot (\r → V.on CT._pivot (PivotTable.eqModel r) ff)

codec ∷ CA.JsonCodec State
codec = C.basicCodec (\j → lmap CA.TypeMismatch $ decode j) encode

encode ∷ State → J.Json
encode = V.match
  { area: Area.encode
  , bar: Bar.encode
  , funnel: Funnel.encode
  , gauge: Gauge.encode
  , graph: Graph.encode
  , heatmap: Heatmap.encode
  , line: Line.encode
  , metric: Metric.encode
  , punchCard: PunchCard.encode
  , scatter: Scatter.encode
  , geoHeatmap: GeoHeatmap.encode
  , geoMarker: GeoMarker.encode
  , pivot: PivotTable.encode
  }

decode ∷ J.Json → String ⊹ State
decode j =
  (map (V.inj CT._area) $ Area.decode j)
  <|> (map (V.inj CT._bar) $ Bar.decode j)
  <|> (map (V.inj CT._funnel) $ Funnel.decode j)
  <|> (map (V.inj CT._gauge) $ Gauge.decode j)
  <|> (map (V.inj CT._graph) $ Graph.decode j)
  <|> (map (V.inj CT._heatmap) $ Heatmap.decode j)
  <|> (map (V.inj CT._line) $ Line.decode j)
  <|> (map (V.inj CT._metric) $ Metric.decode j)
  <|> (map (V.inj CT._punchCard) $ PunchCard.decode j)
  <|> (map (V.inj CT._scatter) $ Scatter.decode j)
  <|> (map (V.inj CT._geoHeatmap) $ GeoHeatmap.decode j)
  <|> (map (V.inj CT._geoMarker) $ GeoMarker.decode j)
  <|> (map (V.inj CT._pivot) $ PivotTable.decode j)

gen ∷ Gen.Gen State
gen = Gen.oneOf ( map (V.inj CT._area) Area.gen )
  [ map (V.inj CT._bar) Bar.gen
  , map (V.inj CT._funnel) Funnel.gen
  , map (V.inj CT._graph) Graph.gen
  , map (V.inj CT._heatmap) Heatmap.gen
  , map (V.inj CT._line) Line.gen
  , map (V.inj CT._metric) Metric.gen
  , map (V.inj CT._punchCard) PunchCard.gen
  , map (V.inj CT._scatter) Scatter.gen
  , map (V.inj CT._geoHeatmap) GeoHeatmap.gen
  , map (V.inj CT._geoMarker) GeoMarker.gen
  , map (V.inj CT._pivot) PivotTable.genModel
  ]

type Query = VariantF
  ( area ∷ FProxy Area.Query
  , bar ∷ FProxy Bar.Query
  , funnel ∷ FProxy Funnel.Query
  , gauge ∷ FProxy Gauge.Query
  , graph ∷ FProxy Graph.Query
  , heatmap ∷ FProxy Heatmap.Query
  , line ∷ FProxy Line.Query
  , metric ∷ FProxy Metric.Query
  , punchCard ∷ FProxy PunchCard.Query
  , scatter ∷ FProxy Scatter.Query
  , geoHeatmap ∷ FProxy GeoHeatmap.Query
  , geoMarker ∷ FProxy GeoMarker.Query
  )

injAux
  ∷ ∀ q st m v1 v2 s
  . RowCons s st v1 v2
  ⇒ IsSymbol s
  ⇒ SProxy s
  → H.Component HH.HTML q (Maybe st) st m
  → H.Component HH.HTML q (Variant v2) (Variant v2) m
injAux proxy =
  under HPR.ProComponent
  $ dimap (V.prj proxy) (V.inj proxy)

type UnifiedAux m =
  H.Component HH.HTML (HCP.ProxyQ (Const Void) State State) State State m

area ∷ ∀ m. UnifiedAux m
area = HCP.proxy $ injAux CT._area Area.component

bar ∷ ∀ m. UnifiedAux m
bar = HCP.proxy $ injAux CT._bar Bar.component

funnel ∷ ∀ m. UnifiedAux m
funnel = HCP.proxy $ injAux CT._funnel Funnel.component

gauge ∷ ∀ m. UnifiedAux m
gauge = HCP.proxy $ injAux CT._gauge Gauge.component

graph ∷ ∀ m. UnifiedAux m
graph = HCP.proxy $ injAux CT._graph Graph.component

heatmap ∷ ∀ m. UnifiedAux m
heatmap = HCP.proxy $ injAux CT._heatmap Heatmap.component

line ∷ ∀ m. UnifiedAux m
line = HCP.proxy $ injAux CT._line Line.component

metric ∷ ∀ m. UnifiedAux m
metric = HCP.proxy $ injAux CT._metric Metric.component

punchCard ∷ ∀ m. UnifiedAux m
punchCard = HCP.proxy $ injAux CT._punchCard PunchCard.component

scatter ∷ ∀ m. UnifiedAux m
scatter = HCP.proxy $ injAux CT._scatter Scatter.component

geoHeatmap ∷ ∀ m. UnifiedAux m
geoHeatmap = HCP.proxy $ injAux CT._geoHeatmap GeoHeatmap.component

geoMarker ∷ ∀ m. UnifiedAux m
geoMarker = HCP.proxy $ injAux CT._geoMarker GeoMarker.component

vizTypeAux ∷ ∀ m f. Alternative f ⇒ VT.VizType → f (UnifiedAux m)
vizTypeAux = V.default empty
  # V.on CT._area (const $ pure area)
  # V.on CT._bar (const $ pure bar)
  # V.on CT._funnel (const $ pure funnel)
  # V.on CT._gauge (const $ pure gauge)
  # V.on CT._graph (const $ pure graph)
  # V.on CT._heatmap (const $ pure heatmap)
  # V.on CT._line (const $ pure line)
  # V.on CT._metric (const $ pure metric)
  # V.on CT._punchCard (const $ pure punchCard)
  # V.on CT._scatter (const $ pure scatter)
  # V.on CT._geoHeatmap (const $ pure geoHeatmap)
  # V.on CT._geoMarker (const $ pure geoMarker)
