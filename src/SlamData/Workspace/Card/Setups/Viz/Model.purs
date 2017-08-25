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

module SlamData.Workspace.Card.Setups.Viz.Model where

import SlamData.Prelude

import Control.Alternative (class Alternative)
import Data.Argonaut ((.?))
import Data.Argonaut as J
import Data.Array as A
import Data.Codec as C
import Data.Codec.Argonaut as CA
import Data.Lens ((.~), (%~), _Just)
import Data.ListMap as LM
import Data.String as Str
import Data.Variant as V
import SlamData.Workspace.Card.CardType as CT
import SlamData.Workspace.Card.CardType.Chart as Cht
import SlamData.Workspace.Card.CardType.Geo as Geo
import SlamData.Workspace.Card.CardType.Input as Inp
import SlamData.Workspace.Card.CardType.Select as Sel
import SlamData.Workspace.Card.CardType.Static as Sta
import SlamData.Workspace.Card.CardType.VizType as VT
import SlamData.Workspace.Card.Setups.Auxiliary as Aux
import SlamData.Workspace.Card.Setups.Auxiliary.Area as Area
import SlamData.Workspace.Card.Setups.Auxiliary.Bar as Bar
import SlamData.Workspace.Card.Setups.Auxiliary.Funnel as Funnel
import SlamData.Workspace.Card.Setups.Auxiliary.Gauge as Gauge
import SlamData.Workspace.Card.Setups.Auxiliary.GeoHeatmap as GeoHeatmap
import SlamData.Workspace.Card.Setups.Auxiliary.GeoMarker as GeoMarker
import SlamData.Workspace.Card.Setups.Auxiliary.Graph as Graph
import SlamData.Workspace.Card.Setups.Auxiliary.Heatmap as Heatmap
import SlamData.Workspace.Card.Setups.Auxiliary.Line as Line
import SlamData.Workspace.Card.Setups.Auxiliary.Metric as Metric
import SlamData.Workspace.Card.Setups.Auxiliary.PunchCard as PunchCard
import SlamData.Workspace.Card.Setups.Auxiliary.Scatter as Scatter
import SlamData.Workspace.Card.Setups.Dimension as D
import SlamData.Workspace.Card.Setups.DimensionMap.Projection as Pr
import SlamData.Workspace.Card.Setups.PivotTable.Model as PivotTable
import Test.StrongCheck.Gen as Gen

lm ∷ ∀ a. LM.Module VT.VizType a
lm = LM.openModule eq

type Model =
  { dimMaps ∷ LM.ListMap VT.VizType Pr.DimMap
  , vizType ∷ VT.VizType
  , auxes ∷ LM.ListMap VT.VizType Aux.State
  }

initial ∷ Model
initial =
  { dimMaps: lm.empty
  , vizType: CT.pie
  , auxes: LM.fromFoldable
      [ CT.area × V.inj CT._area Area.initial
      , CT.bar × V.inj CT._bar Bar.initial
      , CT.funnel × V.inj CT._funnel Funnel.initial
      , CT.gauge × V.inj CT._gauge Gauge.initial
      , CT.graph × V.inj CT._graph Graph.initial
      , CT.heatmap × V.inj CT._heatmap Heatmap.initial
      , CT.line × V.inj CT._line Line.initial
      , CT.metric × V.inj CT._metric Metric.initial
      , CT.punchCard × V.inj CT._punchCard PunchCard.initial
      , CT.scatter × V.inj CT._scatter Scatter.initial
      , CT.geoHeatmap × V.inj CT._geoHeatmap GeoHeatmap.initial
      , CT.geoMarker × V.inj CT._geoMarker GeoMarker.initial
      , CT.pivot × V.inj CT._pivot PivotTable.initialModel
      ]
  }

eq_ ∷ Model → Model → Boolean
eq_ r1 r2 =
  lm.eq_ eq r1.dimMaps r2.dimMaps
  ∧ lm.eq_ Aux.eq_ r1.auxes r2.auxes
  ∧ r1.vizType ≡ r2.vizType

gen ∷ Gen.Gen Model
gen = do
  vizType ← genVT
  auxes ← LM.gen genVT Aux.gen
  dimMaps ← LM.gen genVT Pr.genDimMap
  pure { vizType, auxes, dimMaps }
  where
  genVT = Gen.allInArray VT.all

legacyDecode ∷ String → J.Json → String ⊹ Model
legacyDecode cardTypeString json = do
  vizType ← decodeVizType cardTypeString
  dimMap ← decodeDimMap vizType json
  aux ← decodeAux vizType json
  pure initial
    { vizType = vizType
    , dimMaps =
        fromMaybe initial.dimMaps
        $ dimMap <#> \dm → lm.insert vizType dm initial.dimMaps
    , auxes =
        fromMaybe initial.auxes
        $ aux <#> \a → lm.insert vizType a initial.auxes
    }
  where
  decodeVizType s = do
    decodeChart s
      <|> decodeSelect s
      <|> decodeInput s
      <|> decodeStatic s
      <|> decodeGeo s

  decodeChart =
    Cht.parse ∘ fromMaybe "" ∘ Str.stripSuffix (Str.Pattern "-options")

  decodeSelect =
    Sel.parse ∘ fromMaybe "" ∘ Str.stripSuffix (Str.Pattern "-setup")

  decodeInput =
    Inp.parse ∘ fromMaybe "" ∘ Str.stripSuffix (Str.Pattern "-setup")

  decodeStatic =
    Sta.parse ∘ fromMaybe "" ∘ Str.stripSuffix (Str.Pattern "-setup")

  decodeGeo =
    Geo.parse ∘ fromMaybe "" ∘ Str.stripSuffix (Str.Pattern "-setup")

  decodeDimMap vt j = vt # V.match
    { area: const $ decodeArea j
    , bar: const $ decodeBar j
    , boxplot: const $ decodeBoxplot j
    , candlestick: const $ decodeK j
    , checkbox: const $ decodeSelectDM j
    , date: const $ decodeInputDM j
    , datetime: const $ decodeInputDM j
    , dropdown: const $ decodeSelectDM j
    , funnel: const $ decodeFunnel j
    , gauge: const $ decodeGauge j
    , geoHeatmap: const $ decodeGeoHeatmap j
    , geoMarker: const $ decodeGeoMarker j
    , graph: const $ decodeGraph j
    , heatmap: const $ decodeHeatmap j
    , line: const $ decodeLine j
    , metric: const $ decodeMetric j
    , numeric: const $ decodeInputDM j
    , parallel: const $ decodeParallel j
    , pie: const $ decodePie j
      -- There is no dimension map in pivot table -- whole model is aux
    , pivot: const $ Right Nothing
    , punchCard: const $ decodePunchCard j
    , radar: const $ decodeRadar j
    , radio: const $ decodeSelectDM j
    , sankey: const $ decodeSankey j
    , scatter: const $ decodeScatter j
    , static: const $ decodeStaticDM j
    , text: const $ decodeInputDM j
    , time: const $ decodeInputDM j
    }

  decodeGeoHeatmap = J.decodeJson >=> \obj → map Just do
    lat ← obj .? "lat"
    lng ← obj .? "lng"
    intensity ← obj .? "intensity"
    pure $ mkDimMap
      [ Pr.lat × lat
      , Pr.lng × lng
      , Pr.intensity × intensity
      ]

  decodeGeoMarker = J.decodeJson >=> \obj → map Just do
    dims ← obj .? "dims"
    lat ← obj .? "lat"
    lng ← obj .? "lng"
    series ← obj .? "series"
    size ← obj .? "size"
    pure $ mkDimMap
      $ [ Pr.lat × lat
        , Pr.lng × lng
        , Pr.series × series
        , Pr.size × size
        ]
      ⊕ ( A.mapWithIndex (\ix dim → Pr.dimIx ix × Just dim) dims )

  decodeSelectDM = J.decodeJson >=> \obj → map Just do
    value ← getDimension "value" obj
    name ← obj .? "name"
    label ← getDimension "label" obj
    selected ← getDimension "selected" obj
    pure $ mkDimMap
      [ Pr.formValue × ( value # _Just ∘ D._staticCategory .~ name )
      , Pr.formLabel × label
      , Pr.formSelected × selected
      ]

  decodeInputDM = J.decodeJson >=> \obj → map Just do
    value ← getDimension "value" obj
    name ← obj .? "name"
    pure $ mkDimMap [ Pr.formValue × ( value # _Just ∘ D._staticCategory .~ name ) ]

  decodeArea = J.decodeJson >=> \obj → map Just do
    dimension ← getDimension "dimension" obj
    value ← getMeasure { dim: "value", agg: "valueAggregation" } obj
    series ← getDimension "series" obj
    pure $ mkDimMap
      [ Pr.dimension × dimension
      , Pr.value × value
      , Pr.series × series
      ]

  decodeBar = J.decodeJson >=> \obj → map Just do
    category ← getDimension "category" obj
    value ← getMeasure { dim: "value", agg: "valueAggregation" } obj
    stack ← getDimension "stack" obj
    parallel ← getDimension "parallel" obj
    pure $ mkDimMap
      [ Pr.category × category
      , Pr.value × value
      , Pr.stack × stack
      , Pr.parallel × parallel
      ]

  decodeBoxplot = J.decodeJson >=> \obj → map Just do
    dimension ← getDimension "dimension" obj
    value ← getDimension "value" obj
    series ← getDimension "series" obj
    parallel ← getDimension "parallel" obj
    pure $ mkDimMap
      [ Pr.dimension × dimension
      , Pr.flatValue × value
      , Pr.series × series
      , Pr.parallel × parallel
      ]

  decodeK = J.decodeJson >=> \obj → map Just do
    dimension ← getDimension "dimension" obj
    parallel ← getDimension "parallel" obj
    high ← getMeasure { dim: "high", agg: "highAggregation" } obj
    low ← getMeasure { dim: "low", agg: "lowAggregation" } obj
    open ← getMeasure { dim: "open", agg: "openAggregation" } obj
    close ← getMeasure { dim: "close", agg: "closeAggregation" } obj
    pure $ mkDimMap
      [ Pr.dimension × dimension
      , Pr.parallel × parallel
      , Pr.high × high
      , Pr.low × low
      , Pr.open × open
      , Pr.close × close
      ]

  decodeFunnel = J.decodeJson >=> \obj → map Just do
    category ← getDimension "category" obj
    series ← getDimension "series" obj
    value ← getMeasure { dim: "value", agg: "valueAggregation" } obj
    pure $ mkDimMap
      [ Pr.category × category
      , Pr.series × series
      , Pr.value × value
      ]

  decodeGauge = J.decodeJson >=> \obj → map Just do
    value ← getMeasure { dim: "value", agg: "valueAggregation" } obj
    multiple ← getDimension "multiple" obj
    parallel ← getDimension "parallel" obj
    pure $ mkDimMap
      [ Pr.value × value
      , Pr.multiple × multiple
      , Pr.parallel × parallel
      ]

  decodeGraph = J.decodeJson >=> \obj → map Just do
    source ← getDimension "source" obj
    target ← getDimension "target" obj
    size ← getMeasure { dim: "size", agg: "sizeAggregation" } obj
    color ← getDimension "color" obj
    pure $ mkDimMap
      [ Pr.source × source
      , Pr.target × target
      , Pr.size × size
      , Pr.color × color
      ]

  decodeHeatmap = J.decodeJson >=> \obj → map Just do
    abscissa ← getDimension "abscissa" obj
    ordinate ← getDimension "ordinate" obj
    value ← getMeasure { dim: "value", agg: "valueAggregation" } obj
    series ← getDimension "series" obj
    pure $ mkDimMap
      [ Pr.abscissa × abscissa
      , Pr.ordinate × ordinate
      , Pr.value × value
      , Pr.series × series
      ]

  decodeLine = J.decodeJson >=> \obj → map Just do
    dimension ← getDimension "dimension" obj
    value ← getMeasure { dim: "value", agg: "valueAggregation" } obj
    secondValue ← getMeasure { dim: "secondValue", agg: "secondValueAggregation" } obj
    series ← getDimension "series" obj
    size ← getMeasure { dim: "size", agg: "sizeAggregation" } obj
    pure $ mkDimMap
      [ Pr.dimension × dimension
      , Pr.value × value
      , Pr.secondValue × secondValue
      , Pr.series × series
      , Pr.size × size
      ]

  decodeMetric = J.decodeJson >=> \obj → map Just do
    value ← getMeasure { dim: "value", agg: "valueAggregation" } obj
    label ← obj .? "label"
    pure $ mkDimMap
      [ Pr.value × ( value # _Just ∘ D._staticCategory %~ maybe id const label ) ]

  decodeParallel = J.decodeJson >=> \obj → map Just do
    series ← getDimension "series" obj
    dims ← obj .? "dims" <|> do
      dprs ← obj .? "dims"
      aggs ← obj .? "aggs"
      pure $ D.pairToDimension <$> dprs <*> aggs
    pure $ mkDimMap
      $ [ Pr.series × series ]
      ⊕ ( A.mapWithIndex (\ix dim → Pr.dimIx ix × Just dim) dims )

  decodePie = J.decodeJson >=> \obj → map Just do
    category ← getDimension "category" obj
    value ← getMeasure { dim: "value", agg: "valueAggregation" } obj
    donut ← getDimension "donut" obj
    parallel ← getDimension "parallel" obj
    pure $ mkDimMap
      [ Pr.category × category
      , Pr.value × value
      , Pr.donut × donut
      , Pr.parallel × parallel
      ]

  decodePunchCard = J.decodeJson >=> \obj → map Just do
    abscissa ← getDimension "abscissa" obj
    ordinate ← getDimension "ordinate" obj
    value ← getMeasure { dim: "value", agg: "valueAggregation" } obj
    pure $ mkDimMap
      [ Pr.abscissa × abscissa
      , Pr.ordinate × ordinate
      , Pr.value × value
      ]

  decodeRadar = J.decodeJson >=> \obj → map Just do
    category ← getDimension "category" obj
    value ← getMeasure { dim: "value", agg: "valueAggregation" } obj
    multiple ← getDimension "multiple" obj
    parallel ← getDimension "parallel" obj
    pure $ mkDimMap
      [ Pr.category × category
      , Pr.value × value
      , Pr.multiple × multiple
      , Pr.parallel × parallel
      ]

  decodeSankey = J.decodeJson >=> \obj → map Just do
    source ← getDimension "source" obj
    target ← getDimension "target" obj
    value ← getMeasure { dim: "value", agg: "valueAggregation" } obj
    pure $ mkDimMap
      [ Pr.source × source
      , Pr.target × target
      , Pr.value × value
      ]

  decodeScatter = J.decodeJson >=> \obj → map Just do
    abscissa ← getMeasure { dim: "abscissa", agg: "abscissaAggregation" } obj
    ordinate ← getMeasure { dim: "ordinate", agg: "ordinateAggregation" } obj
    size ← getMeasure { dim: "size", agg: "sizeAggregation" } obj
    series ← getDimension "series" obj
    parallel ← getDimension "parallel" obj
    pure $ mkDimMap
      [ Pr.abscissa × abscissa
      , Pr.scatterOrdinate × ordinate
      , Pr.scatterSize × size
      , Pr.series × series
      , Pr.parallel × parallel
      ]

  decodeStaticDM = J.decodeJson >=> \obj → map Just do
    value ← getDimension "value" obj
    pure $ mkDimMap [ Pr.value × value ]

  -- Previously (~4v) keys in models were `LabeledJCursor`s or `Maybe LabeledJCursor`s.
  -- Here the fact that `Just a` is encoded as `a` is exploited.
  getDimension ∷ String → J.JObject → String ⊹ Maybe D.LabeledJCursor
  getDimension key obj = obj .? key <|>
    ( map (Just ∘ D.defaultJCursorDimension) $ obj .? key )

  -- In <4v measure dimensions were encoded as a pair of `foo` and `fooAggregation`
  getMeasure ∷ { dim ∷ String, agg ∷ String } → J.JObject → String ⊹ Maybe D.LabeledJCursor
  getMeasure r obj = obj .? r.dim <|> do
    ( map Just $ D.pairToDimension <$> ( obj .? r.dim ) <*> ( obj .? r.agg ) )

  -- I don't think providing `fromFoldable` for `DimMap` is good idea /@cryogenian
  mkDimMap ∷ ∀ f. Foldable f ⇒ f (Pr.Projection × Maybe D.LabeledJCursor) → Pr.DimMap
  mkDimMap fs =
    foldl (\pm (pr × mv) → maybe pm (\v → Pr.insert pr v pm) mv) Pr.empty fs

  -- For setup viz models that had no any additional fields
  -- this returns `Right Nothing` ==> `auxes` fields isn't modified
  decodeAux vt j = vt # V.match
    { area: const $ map (prjInj CT._area) $ Aux.decode j
    , bar: const $ map (prjInj CT._bar) $ Aux.decode j
    , boxplot: const $ Right Nothing
    , candlestick: const $ Right Nothing
    , checkbox: const $ Right Nothing
    , date: const $ Right Nothing
    , datetime: const $ Right Nothing
    , dropdown: const $ Right Nothing
    , funnel: const $ map (prjInj CT._funnel) $ Aux.decode j
    , gauge: const $ map (prjInj CT._gauge) $ Aux.decode j
    , geoHeatmap: const $ Right Nothing
    , geoMarker: const $ Right Nothing
    , graph: const $ map (prjInj CT._graph) $ Aux.decode j
    , heatmap: const $ map (prjInj CT._heatmap) $ Aux.decode j
    , line: const $ map (prjInj CT._line) $ Aux.decode j
    , metric: const $ map (prjInj CT._metric) $ Aux.decode j
    , numeric: const $ Right Nothing
    , parallel: const $ Right Nothing
    , pie: const $ Right Nothing
    , pivot: const $ map (prjInj CT._pivot) $ Aux.decode j
    , punchCard: const $ Right Nothing
    , radar: const $ Right Nothing
    , radio: const $ Right Nothing
    , sankey: const $ Right Nothing
    , scatter: const $ Right Nothing
    , static: const $ Right Nothing
    , text: const $ Right Nothing
    , time: const $ Right Nothing
    }

  prjInj
    ∷ ∀ s f a r1 r2
    . IsSymbol s
    ⇒ Alternative f
    ⇒ RowCons s a r1 r2
    ⇒ SProxy s
    → Variant r2
    → f (Variant r2)
  prjInj p = map (V.inj p) ∘ V.prj p

actualCodec ∷ CA.JsonCodec Model
actualCodec = CA.object "Setups.Viz.Model" $ CA.record
  # CA.recordProp (SProxy ∷ SProxy "vizType")
      VT.codec
  # CA.recordProp (SProxy ∷ SProxy "auxes")
      (LM.listMapCodec VT.codec Aux.codec)
  # CA.recordProp (SProxy ∷ SProxy "dimMaps")
      (LM.listMapCodec VT.codec Pr.dimMapCodec)

codec ∷ String → CA.JsonCodec Model
codec str = C.basicCodec dec $ C.encode actualCodec
  where
  dec j =
    (C.decode actualCodec j)
    <|> (lmap CA.TypeMismatch $ legacyDecode str j)
