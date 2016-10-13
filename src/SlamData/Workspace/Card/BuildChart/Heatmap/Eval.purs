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

module SlamData.Workspace.Card.BuildChart.Heatmap.Eval
  ( eval
  , module SlamData.Workspace.Card.BuildChart.Heatmap.Model
  ) where

import SlamData.Prelude

import Color as C

import Data.Argonaut (JArray, Json)
import Data.Array as A
import Data.Lens ((^?))
import Data.Map as M
import Data.Int as Int
import Data.Set as Set

import ECharts.Monad (DSL)
import ECharts.Commands as E
import ECharts.Types as ET
import ECharts.Types.Phantom (OptionI)
import ECharts.Types.Phantom as ETP

import Quasar.Types (FilePath)

import SlamData.Workspace.Card.BuildChart.Common.Eval (type (>>))
import SlamData.Workspace.Card.BuildChart.Common.Eval as BCE
import SlamData.Workspace.Card.BuildChart.Common.Positioning (adjustRectangularPositions, rectangularGrids, rectangularTitles)
import SlamData.Workspace.Card.BuildChart.Heatmap.Model (Model, HeatmapR)
import SlamData.Workspace.Card.BuildChart.ColorScheme (colors, getColorScheme)
import SlamData.Workspace.Card.CardType.ChartType (ChartType(Heatmap))
import SlamData.Workspace.Card.BuildChart.Aggregation as Ag
import SlamData.Workspace.Card.BuildChart.Semantics (getMaybeString, getValues)
import SlamData.Workspace.Card.BuildChart.Axis as Ax
import SlamData.Workspace.Card.Eval.Monad as CEM
import SlamData.Workspace.Card.Port as Port

import Utils.Array (enumerate)

eval
  ∷ Model
  → FilePath
  → CEM.CardEval Port.Port
eval Nothing _ =
  CEM.throw "Please select axis to aggregate"
eval (Just conf) resource = do
  records ← BCE.records resource
  let axes = Ax.buildAxes (A.take 100 records)
  pure $ Port.ChartInstructions axes (buildHeatmap conf records axes) Heatmap

type HeatmapSeries =
  { x ∷ Maybe Number
  , y ∷ Maybe Number
  , w ∷ Maybe Number
  , h ∷ Maybe Number
  , name ∷ Maybe String
  , fontSize ∷ Maybe Int
  , items ∷ (String × String) >> Number
  }

buildHeatmapData ∷ HeatmapR → JArray → Array HeatmapSeries
buildHeatmapData r records = series
  where
  -- | maybe series >> pair of abscissa and ordinate >> values
  dataMap ∷ Maybe String >> (String × String) >> Array Number
  dataMap =
    foldl dataMapFoldFn M.empty records

  dataMapFoldFn
    ∷ Maybe String >> (String × String) >> Array Number
    → Json
    → Maybe String >> (String × String) >> Array Number
  dataMapFoldFn acc js =
    let
      getMaybeStringFromJson = getMaybeString js
      getValuesFromJson = getValues js
      mbAbscissa =
        getMaybeStringFromJson r.abscissa
      mbOrdinate =
        getMaybeStringFromJson r.ordinate
    in case mbAbscissa × mbOrdinate of
      (Just abscissaKey) × (Just ordinateKey) →
        let
          mbSeries =
            getMaybeStringFromJson =<< r.series
          values =
            getValuesFromJson $ pure r.value

          alterSeriesFn
            ∷ Maybe ((String × String) >> Array Number)
            → Maybe ((String × String) >> Array Number)
          alterSeriesFn Nothing =
            Just $ M.singleton (abscissaKey × ordinateKey) values
          alterSeriesFn (Just sers) =
            Just $ M.alter alterCoordFn (abscissaKey × ordinateKey) sers

          alterCoordFn
            ∷ Maybe (Array Number)
            → Maybe (Array Number)
          alterCoordFn Nothing = Just values
          alterCoordFn (Just arr) = Just $ arr ⊕ values
        in
          M.alter alterSeriesFn mbSeries acc
      _ → acc

  rawSeries ∷ Array HeatmapSeries
  rawSeries =
    foldMap mkOneSeries $ M.toList dataMap

  mkOneSeries
    ∷ Maybe String × ((String × String) >> Array Number)
    → Array HeatmapSeries
  mkOneSeries (name × items) =
    [{ name
     , x: Nothing
     , y: Nothing
     , w: Nothing
     , h: Nothing
     , fontSize: Nothing
     , items: map (Ag.runAggregation r.valueAggregation) items
     }]

  series ∷ Array HeatmapSeries
  series = adjustRectangularPositions rawSeries


buildHeatmap ∷ HeatmapR → JArray → Ax.Axes → DSL OptionI
buildHeatmap r records axes = do
  E.tooltip do
    E.triggerAxis
    E.textStyle do
      E.fontFamily "Ubuntu, sans"
      E.fontSize 12
    E.axisPointer do
      E.crossAxisPointer
      E.crossStyle do
        E.color $ C.rgba 170 170 170 0.6
        E.widthNum 0.2
        E.solidLine

  E.animationEnabled false

  rectangularTitles $ map snd heatmapData

  rectangularGrids $ map snd heatmapData

  E.xAxes xAxes

  E.yAxes yAxes

  E.colors colors

  E.visualMap $ E.continuous do
    E.min r.minValue
    E.max r.maxValue
    E.calculable true
    E.orient ET.Horizontal
    E.itemWidth 15.0
    E.leftCenter
    E.bottom $ ET.Percent zero
    E.padding zero
    E.inRange $ E.colors
      if r.isColorSchemeReversed
        then A.reverse $ getColorScheme r.colorScheme
        else getColorScheme r.colorScheme

  E.series series

  where
  heatmapData ∷ Array (Int × HeatmapSeries)
  heatmapData = enumerate $ buildHeatmapData r records

  xValues ∷ HeatmapSeries → Array String
  xValues serie =
    sortX $ A.fromFoldable $ Set.fromFoldable $ map fst $ M.keys serie.items

  yValues ∷ HeatmapSeries → Array String
  yValues serie =
    sortY $ A.fromFoldable $ Set.fromFoldable $ map snd $ M.keys serie.items

  series ∷ ∀ i. DSL (heatMap ∷ ETP.I|i)
  series = for_ heatmapData \(ix × serie) → E.heatMap do
    for_ serie.name E.name
    E.xAxisIndex ix
    E.yAxisIndex ix

    E.buildItems
      $ for_ (enumerate $ xValues serie) \(xIx × abscissa) →
          for_ (enumerate $ yValues serie) \(yIx × ordinate) →
            for_ (M.lookup (abscissa × ordinate) serie.items) \val → E.addItem $ E.buildValues do
              E.addValue $ Int.toNumber xIx
              E.addValue $ Int.toNumber yIx
              E.addValue val

  mkAxis ∷ ∀ i. Int → DSL (ETP.AxisI (gridIndex ∷ ETP.I|i))
  mkAxis ix = do
    E.axisType ET.Category
    E.gridIndex ix
    E.axisLabel do
      E.textStyle do
        E.fontFamily "Ubuntu, sans"
    E.axisLine $ E.lineStyle do
      E.width 1
    E.splitLine $ E.lineStyle do
      E.width 1
    E.splitArea E.hidden

  abscissaAxisType = Ax.axisType r.abscissa axes
  ordinateAxisType = Ax.axisType r.ordinate axes

  abscissaAxisCfg = Ax.axisConfiguration abscissaAxisType
  ordinateAxisCfg = Ax.axisConfiguration ordinateAxisType

  xAxes ∷ ∀ i. DSL (addXAxis ∷ ETP.I|i)
  xAxes = for_ heatmapData \(ix × serie) → E.addXAxis do
    mkAxis ix
    E.items $ map ET.strItem $ xValues serie

  yAxes ∷ ∀ i. DSL (addYAxis ∷ ETP.I|i)
  yAxes = for_ heatmapData \(ix × serie) → E.addYAxis do
    mkAxis ix
    E.items $ map ET.strItem $ yValues serie

  sortX ∷ Array String → Array String
  sortX = A.sortBy $ Ax.compareWithAxisType abscissaAxisType

  sortY ∷ Array String → Array String
  sortY = A.sortBy $ Ax.compareWithAxisType ordinateAxisType
