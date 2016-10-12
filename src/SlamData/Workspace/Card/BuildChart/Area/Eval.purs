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

module SlamData.Workspace.Card.BuildChart.Area.Eval
  ( eval
  , module SlamData.Workspace.Card.BuildChart.Area.Model
  ) where

import SlamData.Prelude

import Data.Argonaut (JArray, Json)
import Data.Array ((!!))
import Data.Array as A
import Data.Lens ((^?))
import Data.Map as M
import Data.Set as Set

import ECharts.Monad (DSL)
import ECharts.Commands as E
import ECharts.Types as ET
import ECharts.Types.Phantom (OptionI)
import ECharts.Types.Phantom as ETP

import Quasar.Types (FilePath)

import SlamData.Quasar.Class (class QuasarDSL)
import SlamData.Quasar.Error as QE
import SlamData.Workspace.Card.BuildChart.Common.Eval (type (>>))
import SlamData.Workspace.Card.BuildChart.Common.Eval as BCE
import SlamData.Workspace.Card.BuildChart.Area.Model (Model, AreaR)
import SlamData.Workspace.Card.CardType.ChartType (ChartType(Area))
import SlamData.Workspace.Card.BuildChart.Aggregation as Ag
import SlamData.Workspace.Card.BuildChart.Axis as Ax
import SlamData.Workspace.Card.BuildChart.Semantics (getMaybeString, getValues)
import SlamData.Workspace.Card.BuildChart.ColorScheme (colors, getShadeColor)
import SlamData.Workspace.Card.BuildChart.Common.Positioning as BCP
import SlamData.Workspace.Card.Eval.CardEvalT as CET
import SlamData.Workspace.Card.Port as Port

import Utils.Array (enumerate)

eval
  ∷ ∀ m
  . (Monad m, QuasarDSL m)
  ⇒ Model
  → FilePath
  → CET.CardEvalT m Port.Port
eval Nothing _ =
  QE.throw "Please select axis to aggregate"
eval (Just conf) resource = do
  records ← BCE.records resource
  let axes = Ax.buildAxes (A.take 100 records)
  pure $ Port.ChartInstructions axes (buildArea conf records axes) Area

type AreaSeries =
  { name ∷ Maybe String
  , items ∷ String >> Number
  }

buildAreaData ∷ AreaR → JArray → Array AreaSeries
buildAreaData r records = series
  where
  -- | maybe series >> dimension >> values
  dataMap ∷ Maybe String >> String >> Array Number
  dataMap =
    foldl dataMapFoldFn M.empty records

  dataMapFoldFn
    ∷ Maybe String >> String >> Array Number
    → Json
    → Maybe String >> String >> Array Number
  dataMapFoldFn acc js =
    let
      getValuesFromJson = getValues js
      getMaybeStringFromJson = getMaybeString js
    in case getMaybeStringFromJson r.dimension of
      Nothing → acc
      Just dimKey →
        let
          mbSeries =
            getMaybeStringFromJson =<< r.series
          values =
            getValuesFromJson $ pure r.value

          alterSeriesFn
            ∷ Maybe (String >> Array Number)
            → Maybe (String >> Array Number)
          alterSeriesFn Nothing =
            Just $ M.singleton dimKey values
          alterSeriesFn (Just dims) =
            Just $ M.alter alterDimFn dimKey dims

          alterDimFn
            ∷ Maybe (Array Number)
            → Maybe (Array Number)
          alterDimFn Nothing = Just values
          alterDimFn (Just arr) = Just $ arr ⊕ values
        in
          M.alter alterSeriesFn mbSeries acc

  series ∷ Array AreaSeries
  series =
    foldMap mkAreaSerie $ M.toList dataMap

  mkAreaSerie
    ∷ Maybe String × (String >> Array Number)
    → Array AreaSeries
  mkAreaSerie (name × items) =
    [{ name
     , items: map (Ag.runAggregation r.valueAggregation) items
     }]

buildArea ∷ AreaR → JArray → Ax.Axes → DSL OptionI
buildArea r records axes = do
  E.tooltip do
    E.triggerAxis
    E.textStyle do
      E.fontFamily "Ubuntu, sans"
      E.fontSize 12
    E.axisPointer do
      E.lineAxisPointer
      E.lineStyle do
        E.width 1
        E.solidLine

  E.yAxis do
    E.axisType ET.Value
    E.axisLabel $ E.textStyle do
      E.fontFamily "Ubuntu, sans"
    E.axisLine $ E.lineStyle do
      E.width 1
    E.splitLine $ E.lineStyle do
      E.width 1

  E.xAxis do
    E.axisType xAxisConfig.axisType
    traverse_ E.interval $ xAxisConfig.interval
    case xAxisConfig.axisType of
      ET.Category →
        E.items $ map ET.strItem xValues
      _ → pure unit
    E.disabledBoundaryGap
    E.axisTick do
      E.length 2
      E.lineStyle do
        E.width 1
    E.splitLine $ E.lineStyle do
      E.width 1
    E.axisLine $ E.lineStyle do
      E.width 1
    E.axisLabel do
      E.rotate r.axisLabelAngle
      E.textStyle do
        E.fontFamily "Ubuntu, sans"

  E.colors colors

  E.grid BCP.cartesian

  E.legend do
    E.textStyle $ E.fontFamily "Ubuntu, sans"
    E.items $ map ET.strItem seriesNames

  E.series series

  where
  areaData ∷ Array (Int × AreaSeries)
  areaData = enumerate $ buildAreaData r records

  xAxisConfig ∷ Ax.EChartsAxisConfiguration
  xAxisConfig = Ax.axisConfiguration $ Ax.axisType r.dimension axes

  xSortFn ∷ String → String → Ordering
  xSortFn = Ax.compareWithAxisType $ Ax.axisType r.dimension axes

  xValues ∷ Array String
  xValues =
    A.sortBy xSortFn
      $ A.fromFoldable
      $ foldMap (snd ⋙_.items ⋙ M.keys ⋙ Set.fromFoldable)
        areaData

  seriesNames ∷ Array String
  seriesNames =
    A.fromFoldable
      $ foldMap (snd ⋙ _.name ⋙ Set.fromFoldable)
        areaData

  series ∷ ∀ i. DSL (line ∷ ETP.I|i)
  series = for_ areaData \(ix × serie) → E.line do
    E.buildItems $ for_ xValues \key → do
      case M.lookup key serie.items of
        Nothing → E.missingItem
        Just v → E.addItem do
          E.name key
          E.buildValues do
            E.addStringValue key
            E.addValue v
    for_ serie.name E.name
    for_ (colors !! ix) \color → do
      E.itemStyle $ E.normal $ E.color color
      E.areaStyle $ E.normal $ E.color $ getShadeColor color (if r.isStacked then 1.0 else 0.5)
    E.lineStyle $ E.normal $ E.width 2
    E.symbol ET.Circle
    E.symbolSize 0
    E.smooth r.isSmooth
    when r.isStacked $ E.stack "stack"
