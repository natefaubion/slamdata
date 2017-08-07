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

module SlamData.Workspace.Card.Setups.Chart.Line.Eval
  ( eval
  , module SlamData.Workspace.Card.Setups.Chart.Line.Model
  ) where

import SlamData.Prelude

import Data.Argonaut (Json, decodeJson, (.?))
import Data.Array as A
import Data.Foldable as F
import Data.Int as Int
import Data.List as L
import Data.Map as M
import Data.Set as Set

import ECharts.Monad (DSL)
import ECharts.Commands as E
import ECharts.Types as ET
import ECharts.Types.Phantom (OptionI)
import ECharts.Types.Phantom as ETP

import SlamData.Workspace.Card.CardType.ChartType (ChartType(Line))
import SlamData.Workspace.Card.Port as Port
import SlamData.Workspace.Card.Setups.Axis (Axes)
import SlamData.Workspace.Card.Setups.Axis as Ax
import SlamData.Workspace.Card.Setups.Chart.ColorScheme (colors)
import SlamData.Workspace.Card.Setups.Chart.Common as SCC
import SlamData.Workspace.Card.Setups.Chart.Common.Positioning as BCP
import SlamData.Workspace.Card.Setups.Chart.Common.Tooltip as CCT
import SlamData.Workspace.Card.Setups.Chart.Line.Model (Model, ModelR)
import SlamData.Workspace.Card.Setups.Common.Eval (type (>>))
import SlamData.Workspace.Card.Setups.Common.Eval as BCE
import SlamData.Workspace.Card.Setups.Dimension as D
import SlamData.Workspace.Card.Setups.Semantics as Sem
import SqlSquared as Sql

eval ∷ ∀ m v. BCE.ChartSetupEval ModelR m v
eval = BCE.chartSetupEval (SCC.buildBasicSql buildProjections buildGroupBy) buildLine

type LineSerie =
  { name ∷ Maybe String
  , leftItems ∷ String >> LineItem
  , rightItems ∷ String >> LineItem
  }

type LineItem =
  { value ∷ Number
  , symbolSize ∷ Int
  }

type Item =
  { category ∷ String
  , measure1 ∷ Number
  , measure2 ∷ Maybe Number
  , size ∷ Maybe Number
  , series ∷ Maybe String
  }

decodeItem ∷ Json → Either String Item
decodeItem = decodeJson >=> \obj → do
  category ← Sem.requiredString "" <$> obj .? "category"
  measure1 ← Sem.requiredNumber zero <$> obj .? "measure1"
  measure2 ← Sem.maybeNumber <$> obj .? "measure2"
  size ← Sem.maybeNumber <$> obj .? "size"
  series ← Sem.maybeString <$> obj .? "series"
  pure { category, measure1, measure2, size, series }

buildProjections ∷ ModelR → L.List (Sql.Projection Sql.Sql)
buildProjections r = L.fromFoldable
  [ r.dimension # SCC.jcursorPrj # Sql.as "category"
  , r.value # SCC.jcursorPrj # Sql.as "measure1" # SCC.applyTransform r.value
  , r.size # maybe SCC.nullPrj SCC.jcursorPrj # Sql.as "size"
  , r.series # maybe SCC.nullPrj SCC.jcursorPrj # Sql.as "series"
  , secondValueF
  ]
  where
  secondValueF = case r.secondValue of
    Just sv → sv # SCC.jcursorPrj # Sql.as "measure2" # SCC.applyTransform sv
    Nothing → SCC.nullPrj # Sql.as "measure2"

buildGroupBy ∷ ModelR → Maybe (Sql.GroupBy Sql.Sql)
buildGroupBy r =
  SCC.groupBy $ L.fromFoldable $ A.catMaybes
    [ r.series <#> SCC.jcursorSql
    , Just r.dimension <#> SCC.jcursorSql
    ]

buildLine ∷ ModelR → Axes → Port.Port
buildLine m axes =
  Port.ChartInstructions
    { options: lineOptions axes m ∘ buildLineData m
    , chartType: Line
    }

buildLineData ∷ ModelR → Array Json → Array LineSerie
buildLineData r =
  foldMap (foldMap A.singleton ∘ decodeItem)
    >>> lineSeries

  where
  lineSeries ∷ Array Item → Array LineSerie
  lineSeries =
    BCE.groupOn _.series
      >>> map \(name × is) →
            { name
            , leftItems: adjustSymbolSizes $ M.fromFoldable (items (pure ∘ _.measure1) is)
            , rightItems: adjustSymbolSizes $ M.fromFoldable (items _.measure2 is)
            }

  items ∷ (Item → Maybe Number) → Array Item → Array (String × LineItem)
  items f = A.mapMaybe \i →
    Tuple i.category <$> case f i, i.size of
      Nothing, _ → Nothing
      Just value, _ | r.optionalMarkers → Just { value, symbolSize: Int.floor r.minSize }
      Just value, Nothing → Just { value, symbolSize: Int.floor r.minSize }
      Just value, Just size → Just { value, symbolSize: Int.floor size }

  adjustSymbolSizes ∷ ∀ f. Functor f ⇒ Foldable f ⇒ f LineItem → f LineItem
  adjustSymbolSizes is
    | r.optionalMarkers = is
    | otherwise =
      let
        minValue ∷ Number
        minValue =
          Int.toNumber
            $ fromMaybe bottom
            $ map _.symbolSize
            $ F.minimumBy (\a b → compare a.symbolSize b.symbolSize) is

        maxValue ∷ Number
        maxValue =
          Int.toNumber
            $ fromMaybe top
            $ map _.symbolSize
            $ F.maximumBy (\a b → compare a.symbolSize b.symbolSize) is

        distance ∷ Number
        distance =
          maxValue - minValue

        sizeDistance ∷ Number
        sizeDistance =
          r.maxSize - r.minSize

        relativeSize ∷ Int → Int
        relativeSize val
          | distance ≡ 0.0 = val
          | otherwise =
            Int.floor
            $ r.maxSize
            - sizeDistance / distance * (maxValue - Int.toNumber val)
      in
        map (\x → x{symbolSize = relativeSize x.symbolSize}) is

lineOptions ∷ Axes → ModelR → Array LineSerie → DSL OptionI
lineOptions axes r lineData = do
  let
    cols =
      [ { label: D.jcursorLabel r.dimension, value: CCT.formatValueIx 0 }
      , { label: D.jcursorLabel r.value, value: \x →
           if isNothing r.secondValue ∨ x.seriesIndex `mod` 2 ≡ 0
           then CCT.formatValueIx 1 x
           else ""
        }
      ]
    opts = A.catMaybes
      [ r.secondValue <#> \dim → { label: D.jcursorLabel dim, value: \x →
                                    if x.seriesIndex `mod` 2 ≡ 0
                                    then ""
                                    else CCT.formatValueIx 1 x
                                 }
      , r.size <#> \dim → { label: D.jcursorLabel dim, value: CCT.formatSymbolSize }
      , r.series <#> \dim → { label: D.jcursorLabel dim, value: _.seriesName }
      ]
  CCT.tooltip do
    E.triggerItem
    E.formatterItem (CCT.tableFormatter (pure ∘ _.color) (cols <> opts) ∘ pure)

  E.colors colors
  E.grid BCP.cartesian
  E.series series

  E.xAxis do
    E.axisType xAxisConfig.axisType
    case xAxisConfig.axisType of
      ET.Category →
        E.items $ map ET.strItem xValues
      _ → pure unit
    E.axisLabel do
      E.rotate r.axisLabelAngle
      traverse_ E.interval xAxisConfig.interval
      E.textStyle do
        E.fontFamily "Ubuntu, sans"

  E.yAxes do
    E.addYAxis yAxis
    when needTwoAxes $ E.addYAxis yAxis

  E.legend do
    E.items $ map ET.strItem seriesNames
    E.textStyle $ E.fontFamily "Ubuntu, sans"

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
      $ foldMap (\x → Set.fromFoldable $ M.keys x.leftItems ⊕ M.keys x.rightItems)
        lineData

  seriesNames ∷ Array String
  seriesNames = case r.series of
    Just _ → A.fromFoldable $ foldMap (_.name ⋙ foldMap Set.singleton) lineData
    Nothing →
      A.catMaybes
        [ Just $ D.jcursorLabel r.value
        , D.jcursorLabel <$> r.secondValue
        ]

  needTwoAxes ∷ Boolean
  needTwoAxes = isJust r.secondValue

  series ∷ ∀ i. DSL (line ∷ ETP.I|i)
  series = for_ lineData \lineSerie → do
    E.line do
      E.buildItems $ for_ xValues \key →
        case M.lookup key lineSerie.leftItems of
          Nothing → E.missingItem
          Just {value, symbolSize} → E.addItem do
            E.name key
            E.buildValues do
              E.addStringValue key
              E.addValue value
            E.symbolSize symbolSize
      E.yAxisIndex 0
      case r.series of
        Just _ →
          for_ lineSerie.name E.name
        Nothing →
          E.name $ D.jcursorLabel r.value

    when needTwoAxes $ E.line do
      E.buildItems $ for_ xValues \key →
        case M.lookup key lineSerie.rightItems of
          Nothing → E.missingItem
          Just {value, symbolSize} → E.addItem do
            E.name key
            E.buildValues do
              E.addStringValue key
              E.addValue value
            E.symbolSize symbolSize
      E.yAxisIndex 1
      case r.series of
        Just _ →
          for_ lineSerie.name E.name
        Nothing →
          for_ r.secondValue (E.name ∘ D.jcursorLabel)

  yAxis ∷ DSL ETP.YAxisI
  yAxis = do
    E.axisType ET.Value
    E.axisLabel $ E.textStyle do
      E.fontFamily "Ubuntu, sans"
