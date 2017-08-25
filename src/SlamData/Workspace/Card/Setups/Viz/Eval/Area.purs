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

module SlamData.Workspace.Card.Setups.Viz.Eval.Area where

import SlamData.Prelude

import Data.Argonaut (Json, decodeJson, (.?))
import Data.Array ((!!))
import Data.Array as A
import Data.Int as Int
import Data.Lens ((^?))
import Data.List as L
import Data.Map as M
import Data.Set as Set
import Data.Variant (prj)
import ECharts.Commands as E
import ECharts.Monad (DSL)
import ECharts.Types as ET
import ECharts.Types.Phantom (OptionI)
import ECharts.Types.Phantom as ETP
import SlamData.Workspace.Card.CardType as CT
import SlamData.Workspace.Card.Port as Port
import SlamData.Workspace.Card.Setups.Auxiliary as Aux
import SlamData.Workspace.Card.Setups.Auxiliary.Area as Area
import SlamData.Workspace.Card.Setups.Axis as Ax
import SlamData.Workspace.Card.Setups.ColorScheme (colors, getShadeColor)
import SlamData.Workspace.Card.Setups.Common.Positioning as BCP
import SlamData.Workspace.Card.Setups.Common.Tooltip as CCT
import SlamData.Workspace.Card.Setups.Common as SC
import SlamData.Workspace.Card.Setups.Common.Eval (type (>>))
import SlamData.Workspace.Card.Setups.Common.Eval as BCE
import SlamData.Workspace.Card.Setups.Dimension as D
import SlamData.Workspace.Card.Setups.DimensionMap.Projection as P
import SlamData.Workspace.Card.Setups.Semantics as Sem
import SlamData.Workspace.Card.Setups.Viz.Eval.Common (VizEval)
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

eval ∷ ∀ m. VizEval m (P.DimMap → Aux.State → Port.Port → m Port.Out)
eval dimMap aux =
  BCE.chartSetupEval buildSql buildPort aux'
  where
  aux' = prj CT._area aux
  buildSql = SC.buildBasicSql (buildProjections dimMap) (buildGroupBy dimMap)
  buildPort r axes = Port.ChartInstructions
    { options: options dimMap axes r ∘ buildData
    , chartType: CT.area
    }

buildProjections ∷ ∀ a. P.DimMap → a → L.List (Sql.Projection Sql.Sql)
buildProjections dimMap _ = L.fromFoldable $ A.concat
  [ SC.measureProjection P.value dimMap "measure"
  , SC.dimensionProjection P.dimension dimMap "dimension"
  , SC.dimensionProjection P.series dimMap "series"
  ]

buildGroupBy ∷ ∀ a. P.DimMap → a → Maybe (Sql.GroupBy Sql.Sql)
buildGroupBy dimMap _ = SC.groupBy
  $ SC.sqlProjection P.series dimMap
  <|> SC.sqlProjection P.dimension dimMap


buildData ∷ Array Json → Array AreaSeries
buildData =
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

options ∷ P.DimMap → Ax.Axes → Area.State → Array AreaSeries → DSL OptionI
options dimMap axes r areaData = do
  let
    mkRow prj value  = P.lookup prj dimMap # foldMap \dim →
      [ { label: D.jcursorLabel dim, value } ]

    cols = A.fold
      [ mkRow P.dimension (CCT.formatValueIx 0)
      , mkRow P.value (CCT.formatValueIx 1)
      , mkRow P.series _.seriesName
      ]
  CCT.tooltip do
    E.formatterItem (CCT.tableFormatter (pure ∘ _.color) cols ∘ pure)
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
  xAxisType = fromMaybe Ax.Category do
    ljc ← P.lookup P.dimension dimMap
    cursor ← ljc ^? D._value ∘ D._projection
    pure $ Ax.axisType cursor axes

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
