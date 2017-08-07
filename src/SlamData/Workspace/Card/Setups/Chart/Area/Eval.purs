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

module SlamData.Workspace.Card.Setups.Chart.Area.Eval
  ( eval
  , module SlamData.Workspace.Card.Setups.Chart.Area.Model
  ) where

import SlamData.Prelude

import Data.Argonaut (Json, decodeJson, (.?))
import Data.Array ((!!))
import Data.Array as A
import Data.Int as Int
import Data.List as L
import Data.Map as M
import Data.Set as Set

import ECharts.Monad (DSL)
import ECharts.Commands as E
import ECharts.Types as ET
import ECharts.Types.Phantom (OptionI)
import ECharts.Types.Phantom as ETP

import SlamData.Workspace.Card.Setups.Common.Eval (type (>>))
import SlamData.Workspace.Card.Setups.Common.Eval as BCE
import SlamData.Workspace.Card.Setups.Chart.Area.Model (Model, ModelR)
import SlamData.Workspace.Card.Setups.Chart.Common as SCC
import SlamData.Workspace.Card.CardType.ChartType (ChartType(Area))
import SlamData.Workspace.Card.Setups.Axis as Ax
import SlamData.Workspace.Card.Setups.Semantics as Sem
import SlamData.Workspace.Card.Setups.Chart.ColorScheme (colors, getShadeColor)
import SlamData.Workspace.Card.Setups.Chart.Common.Positioning as BCP
import SlamData.Workspace.Card.Setups.Chart.Common.Tooltip as CCT
import SlamData.Workspace.Card.Setups.Dimension as D
import SlamData.Workspace.Card.Port as Port

import SqlSquared as Sql

import Utils.Foldable (enumeratedFor_)

type Item =
  { dimension ∷ String
  , measure ∷ Number
  , series ∷ Maybe String
  }

type AreaSeries =
  { name ∷ Maybe String
  , items ∷ String >> Number
  }

decodeItem ∷ Json → String ⊹ Item
decodeItem = decodeJson >=> \obj → do
  measure ← map (fromMaybe zero ∘ Sem.maybeNumber) $ obj .? "measure"
  dimension ← map (fromMaybe "" ∘ Sem.maybeString) $ obj .? "dimension"
  series ← map Sem.maybeString $ obj .? "series"
  pure { measure
       , dimension
       , series
       }

eval ∷ ∀ m v. BCE.ChartSetupEval ModelR m v
eval = BCE.chartSetupEval (SCC.buildBasicSql buildProjections buildGroupBy) buildArea

buildProjections ∷ ModelR → L.List (Sql.Projection Sql.Sql)
buildProjections r = L.fromFoldable
  [ r.value # SCC.jcursorPrj # Sql.as "measure" # SCC.applyTransform r.value
  , r.dimension # SCC.jcursorPrj # Sql.as "dimension"
  , r.series # maybe SCC.nullPrj SCC.jcursorPrj # Sql.as "series"
  ]

buildGroupBy ∷ ModelR → Maybe (Sql.GroupBy Sql.Sql)
buildGroupBy r =
  SCC.groupBy $ L.fromFoldable $ A.catMaybes
    [ r.series <#> SCC.jcursorSql
    , Just r.dimension <#> SCC.jcursorSql
    ]

buildArea ∷ ModelR → Ax.Axes → Port.Port
buildArea m axes =
  Port.ChartInstructions
    { options: areaOptions axes m ∘ buildAreaData
    , chartType: Area
    }

buildAreaData ∷ Array Json → Array AreaSeries
buildAreaData =
  series ∘ foldMap (foldMap A.singleton ∘ decodeItem)
  where
  series ∷ Array Item → Array AreaSeries
  series =
    BCE.groupOn _.series
      ⋙ map \(name × items) →
          { name
          , items: M.fromFoldable $ map toPoint items
          }
  toPoint ∷ Item → String × Number
  toPoint { dimension, measure } = dimension × measure

areaOptions ∷ Ax.Axes → ModelR → Array AreaSeries → DSL OptionI
areaOptions axes r areaData = do
  let
    cols =
      [ { label: D.jcursorLabel r.dimension, value: CCT.formatValueIx 0 }
      , { label: D.jcursorLabel r.value, value: CCT.formatValueIx 1 }
      ]
    opts = flip foldMap r.series \dim →
      [ { label: D.jcursorLabel dim, value: _.seriesName
        } ]

  CCT.tooltip do
    E.formatterItem (CCT.tableFormatter (pure ∘ _.color) (cols <> opts) ∘ pure)
    E.triggerItem
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
  xAxisType ∷ Ax.AxisType
  xAxisType = D.axisType r.dimension axes

  xAxisConfig ∷ Ax.EChartsAxisConfiguration
  xAxisConfig = Ax.axisConfiguration xAxisType

  xSortFn ∷ String → String → Ordering
  xSortFn = Ax.compareWithAxisType xAxisType

  xValues ∷ Array String
  xValues =
    A.sortBy xSortFn
      $ A.fromFoldable
      $ foldMap (_.items ⋙ M.keys ⋙ Set.fromFoldable)
        areaData

  seriesNames ∷ Array String
  seriesNames =
    A.fromFoldable
      $ foldMap (_.name ⋙ Set.fromFoldable)
        areaData

  series ∷ ∀ i. DSL (line ∷ ETP.I|i)
  series = enumeratedFor_ areaData \(ix × serie) → E.line do
    E.buildItems $ for_ xValues \key → do
      case M.lookup key serie.items of
        Nothing → E.missingItem
        Just v → E.addItem do
          E.name key
          E.buildValues do
            E.addStringValue key
            E.addValue v
          E.symbolSize $ Int.floor r.size
    for_ serie.name E.name
    for_ (colors !! ix) \color → do
      E.itemStyle $ E.normal $ E.color color
      E.areaStyle $ E.normal $ E.color $ getShadeColor color (if r.isStacked then 1.0 else 0.5)
    E.lineStyle $ E.normal $ E.width 2
    E.symbol ET.Circle
    E.smooth r.isSmooth
    when r.isStacked $ E.stack "stack"
