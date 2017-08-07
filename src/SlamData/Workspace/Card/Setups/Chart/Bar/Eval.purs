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

module SlamData.Workspace.Card.Setups.Chart.Bar.Eval
  ( eval
  , module SlamData.Workspace.Card.Setups.Chart.Bar.Model
  ) where

import SlamData.Prelude

import Data.Argonaut (Json, decodeJson, (.?))
import Data.Array as A
import Data.Map as M
import Data.Set as Set
import Data.List as L

import ECharts.Monad (DSL)
import ECharts.Commands as E
import ECharts.Types as ET
import ECharts.Types.Phantom (OptionI)
import ECharts.Types.Phantom as ETP

import SlamData.Workspace.Card.CardType.ChartType (ChartType(Bar))
import SlamData.Workspace.Card.Port as Port
import SlamData.Workspace.Card.Setups.Axis (Axes)
import SlamData.Workspace.Card.Setups.Axis as Ax
import SlamData.Workspace.Card.Setups.Chart.Bar.Model (Model, ModelR)
import SlamData.Workspace.Card.Setups.Chart.ColorScheme (colors)
import SlamData.Workspace.Card.Setups.Chart.Common as SCC
import SlamData.Workspace.Card.Setups.Chart.Common.Positioning as BCP
import SlamData.Workspace.Card.Setups.Chart.Common.Tooltip as CCT
import SlamData.Workspace.Card.Setups.Common.Eval (type (>>))
import SlamData.Workspace.Card.Setups.Common.Eval as BCE
import SlamData.Workspace.Card.Setups.Dimension as D
import SlamData.Workspace.Card.Setups.Semantics as Sem

import SqlSquared as Sql

type BarSeries =
  { name ∷ Maybe String
  , items ∷ String >> Number
  }

type BarStacks =
  { stack ∷ Maybe String
  , series ∷ Array BarSeries
  }

type Item =
  { measure ∷ Number
  , category ∷ String
  , parallel ∷ Maybe String
  , stack ∷ Maybe String
  }

decodeItem ∷ Json → Either String Item
decodeItem = decodeJson >=> \obj → do
  measure ← map (fromMaybe zero ∘ Sem.maybeNumber) $ obj .? "measure"
  category ← map (fromMaybe "" ∘ Sem.maybeString) $ obj .? "category"
  parallel ← map Sem.maybeString $ obj .? "parallel"
  stack ← map Sem.maybeString $ obj .? "stack"
  pure { measure
       , category
       , parallel
       , stack
       }

eval ∷ ∀ m v. BCE.ChartSetupEval ModelR m v
eval = BCE.chartSetupEval (SCC.buildBasicSql buildProjections buildGroupBy) buildBar

buildProjections ∷ ModelR → L.List (Sql.Projection Sql.Sql)
buildProjections r = L.fromFoldable
  [ r.value # SCC.jcursorPrj # Sql.as "measure" # SCC.applyTransform r.value
  , r.category # SCC.jcursorPrj # Sql.as "category"
  , r.stack # maybe SCC.nullPrj SCC.jcursorPrj # Sql.as "stack"
  , r.parallel # maybe SCC.nullPrj SCC.jcursorPrj # Sql.as "parallel"
  ]

buildGroupBy ∷ ModelR → Maybe (Sql.GroupBy Sql.Sql)
buildGroupBy r =
  SCC.groupBy $ L.fromFoldable $ A.catMaybes
    [ r.parallel <#> SCC.jcursorSql
    , r.stack <#> SCC.jcursorSql
    , Just r.category <#> SCC.jcursorSql
    ]

buildBar ∷ ModelR → Axes → Port.Port
buildBar m axes =
  Port.ChartInstructions
    { options: barOptions axes m ∘ buildBarData
    , chartType: Bar
    }

buildBarData ∷ Array Json → Array BarStacks
buildBarData =
  stacks ∘ foldMap (foldMap A.singleton ∘ decodeItem)
  where
  stacks ∷ Array Item → Array BarStacks
  stacks =
    BCE.groupOn _.stack
      ⋙ map \(stack × items) →
          { stack
          , series: series items
          }
  series ∷ Array Item → Array BarSeries
  series =
    BCE.groupOn _.parallel
      ⋙ map \(name × items) →
          { name
          , items: M.fromFoldable $ map toPoint items
          }
  toPoint ∷ Item → String × Number
  toPoint { measure, category } = category × measure

barOptions ∷ Axes → ModelR → Array BarStacks → DSL OptionI
barOptions axes r barData = do
  let
    cols =
      [ { label: D.jcursorLabel r.category, value: CCT.formatValueIx 0 }
      , { label: D.jcursorLabel r.value, value: CCT.formatValueIx 1 }
      ]
    seriesFn ix dim =
      { label: D.jcursorLabel dim, value: CCT.formatNameIx ix }
    opts =
      A.catMaybes
      [ seriesFn 0 <$> r.stack
      , seriesFn (fromMaybe 0 $ r.stack $> 1) <$> r.parallel
      ]

  CCT.tooltip do
    E.formatterItem (CCT.tableFormatter (pure ∘ _.color) (cols <> opts) ∘ pure)
    E.triggerItem

  E.colors colors

  E.xAxis do
    E.axisType ET.Category
    E.enabledBoundaryGap
    E.items $ map ET.strItem xValues
    E.axisLabel do
      traverse_ E.interval xAxisConfig.interval
      E.rotate r.axisLabelAngle
      E.textStyle do
        E.fontFamily "Ubuntu, sans"

  E.yAxis do
    E.axisType ET.Value
    E.axisLabel $ E.textStyle do
      E.fontFamily "Ubuntu, sans"
    E.axisLine $ E.lineStyle $ E.width 1
    E.splitLine $ E.lineStyle $ E.width 1

  E.legend do
    E.textStyle $ E.fontFamily "Ubuntu, sans"
    case xAxisConfig.axisType of
      ET.Category | A.length seriesNames > 40 → E.hidden
      _ → pure unit
    E.items $ map ET.strItem seriesNames
    E.leftLeft
    E.topBottom

  E.grid BCP.cartesian

  E.series series

  where
  xAxisType ∷ Ax.AxisType
  xAxisType = D.axisType r.category axes

  xAxisConfig ∷ Ax.EChartsAxisConfiguration
  xAxisConfig = Ax.axisConfiguration xAxisType

  seriesNames ∷ Array String
  seriesNames = case r.stack, r.parallel of
    Nothing, Nothing →
      [ ]
    Nothing, Just _ →
      A.fromFoldable
      $ flip foldMap barData
      $ foldMap (Set.fromFoldable ∘ _.name)
      ∘ _.series
    _, _ →
      A.catMaybes $ map _.stack barData

  xValues ∷ Array String
  xValues =
    A.sortBy xSortFn
      $ A.fromFoldable
      $ flip foldMap barData
      $ foldMap (Set.fromFoldable ∘ M.keys ∘ _.items)
      ∘ _.series

  xSortFn ∷ String → String → Ordering
  xSortFn = Ax.compareWithAxisType xAxisType

  series ∷ ∀ i. DSL (bar ∷ ETP.I|i)
  series = for_ barData \stacked →
    for_ stacked.series \serie → E.bar do
      E.buildItems $ for_ xValues \key →
        case M.lookup key serie.items of
          Nothing → E.missingItem
          Just v → E.addItem do
            E.buildNames do
              for_ stacked.stack $ E.addName
              for_ serie.name $ E.addName
              E.addName $ "key:" ⊕ key
            E.buildValues do
              E.addStringValue key
              E.addValue v
      for_ stacked.stack E.name
      case r.parallel, r.stack of
        Just _, Nothing →
          for_ serie.name E.name
        Just _, Just _ →
          for_ serie.name E.stack
        _, _ →
          E.stack "default stack"
