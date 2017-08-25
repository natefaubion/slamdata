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

module SlamData.Workspace.Card.Setups.Viz.Eval.Heatmap where

import SlamData.Prelude

import Color as C
import Data.Argonaut (Json, decodeJson, (.?))
import Data.Array as A
import Data.Foreign as Frn
import Data.Lens ((^?))
import Data.List as L
import Data.Map as M
import Data.Int as Int
import Data.Set as Set
import Data.Variant (prj)
import ECharts.Monad (DSL)
import ECharts.Commands as E
import ECharts.Types as ET
import ECharts.Types.Phantom (OptionI)
import ECharts.Types.Phantom as ETP
import SlamData.Workspace.Card.CardType as CT
import SlamData.Workspace.Card.Port as Port
import SlamData.Workspace.Card.Setups.Axis as Ax
import SlamData.Workspace.Card.Setups.ColorScheme (colors, getColorScheme)
import SlamData.Workspace.Card.Setups.Common as SC
import SlamData.Workspace.Card.Setups.Common.Positioning as BCP
import SlamData.Workspace.Card.Setups.Common.Tooltip as CCT
import SlamData.Workspace.Card.Setups.Common.Eval (type (>>))
import SlamData.Workspace.Card.Setups.Common.Eval as BCE
import SlamData.Workspace.Card.Setups.Dimension as D
import SlamData.Workspace.Card.Setups.Semantics as Sem
import SlamData.Workspace.Card.Setups.Viz.Eval.Common (VizEval)
import SlamData.Workspace.Card.Setups.DimensionMap.Projection as P
import SlamData.Workspace.Card.Setups.Auxiliary as Aux
import SlamData.Workspace.Card.Setups.Auxiliary.Heatmap as Heatmap
import SqlSquared as Sql
import Utils.Foldable (enumeratedFor_)

eval ∷ ∀ m. VizEval m (P.DimMap → Aux.State → Port.Port → m Port.Out)
eval dimMap aux =
  BCE.chartSetupEval buildSql buildPort aux'
  where
  aux' = prj CT._heatmap aux
  buildSql = SC.buildBasicSql (buildProjections dimMap) (buildGroupBy dimMap)
  buildPort r axes = Port.ChartInstructions
    { options: options dimMap axes r ∘ buildData
    , chartType: CT.heatmap
    }

type HeatmapSeries =
  { x ∷ Maybe Number
  , y ∷ Maybe Number
  , w ∷ Maybe Number
  , h ∷ Maybe Number
  , name ∷ Maybe String
  , fontSize ∷ Maybe Int
  , items ∷ (String × String) >> Number
  }

type Item =
  { abscissa ∷ String
  , ordinate ∷ String
  , measure ∷ Number
  , series ∷ Maybe String
  }

decodeItem ∷ Json → Either String Item
decodeItem = decodeJson >=> \obj → do
  abscissa ← CCT.formatForeign ∘ jsonToForeign <$> obj .? "abscissa"
  ordinate ← CCT.formatForeign ∘ jsonToForeign <$> obj .? "ordinate"
  measure ← Sem.requiredNumber zero <$> obj .? "measure"
  series ← Sem.maybeString <$> obj .? "series"
  pure { abscissa, ordinate, measure, series }
  where
  jsonToForeign ∷ Json → Frn.Foreign
  jsonToForeign = Frn.toForeign

buildProjections ∷ ∀ a. P.DimMap → a → L.List (Sql.Projection Sql.Sql)
buildProjections dimMap _ = L.fromFoldable $ A.concat
  [ SC.dimensionProjection P.abscissa dimMap "abscissa"
  , SC.dimensionProjection P.ordinate dimMap "ordinate"
  , SC.measureProjection P.value dimMap "measure"
  , SC.dimensionProjection P.series dimMap "series"
  ]

buildGroupBy ∷ ∀ a. P.DimMap → a → Maybe (Sql.GroupBy Sql.Sql)
buildGroupBy dimMap _ = SC.groupBy
  $ SC.sqlProjection P.series dimMap
  <|> SC.sqlProjection P.abscissa dimMap
  <|> SC.sqlProjection P.ordinate dimMap

buildData ∷ Array Json → Array HeatmapSeries
buildData =
  BCP.adjustRectangularPositions
  ∘ series
  ∘ foldMap (foldMap A.singleton ∘ decodeItem)

  where
  series ∷ Array Item → Array HeatmapSeries
  series =
    BCE.groupOn _.series
      >>> map \(name × items) →
            { name
            , x: Nothing
            , y: Nothing
            , w: Nothing
            , h: Nothing
            , fontSize: Nothing
            , items: M.fromFoldable $ toPoint <$> items
            }

  toPoint ∷ Item → (String × String) × Number
  toPoint item = (item.abscissa × item.ordinate) × item.measure

options ∷ P.DimMap → Ax.Axes → Heatmap.State → Array HeatmapSeries → DSL OptionI
options dimMap axes r heatmapData = do
  CCT.tooltip do
    E.triggerItem
    E.axisPointer do
      E.crossAxisPointer
      E.crossStyle do
        E.color $ C.rgba 170 170 170 0.6
        E.widthNum 0.2
        E.solidLine
    E.formatterItem \item →
      let
        mkRow prj val = P.lookup prj dimMap # foldMap \dim →
            [ D.jcursorLabel dim × val ]
      in CCT.tableRows $ A.concat
        [ mkRow P.abscissa $ CCT.formatAssocProp "abscissa" item
        , mkRow P.ordinate $ CCT.formatAssocProp "ordinate" item
        , mkRow P.value $ CCT.formatAssocProp "value" item
        ]

  E.animationEnabled false

  BCP.rectangularTitles heatmapData
    $ maybe "" D.jcursorLabel
    $ P.lookup P.series dimMap

  BCP.rectangularGrids heatmapData

  E.xAxes xAxes

  E.yAxes yAxes

  E.colors colors

  E.visualMap $ E.continuous do
    E.min r.val.min
    E.max r.val.max
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
  xValues ∷ HeatmapSeries → Array String
  xValues serie =
    sortX $ A.fromFoldable $ Set.fromFoldable $ map fst $ M.keys serie.items

  yValues ∷ HeatmapSeries → Array String
  yValues serie =
    sortY $ A.fromFoldable $ Set.fromFoldable $ map snd $ M.keys serie.items

  series ∷ ∀ i. DSL (heatMap ∷ ETP.I|i)
  series = enumeratedFor_ heatmapData \(ix × serie) → E.heatMap do
    for_ serie.name E.name
    E.xAxisIndex ix
    E.yAxisIndex ix

    E.buildItems
      -- That's interesting, for some reason item indices are reversed
      -- E.g. value `3` is 2nd of 7 values in x-axis, but the index here
      -- should be not `1` but `5` 0_o /@cryogenian
      $ enumeratedFor_ (A.reverse $ xValues serie)  \(xIx × abscissa) →
          enumeratedFor_ (A.reverse $ yValues serie) \(yIx × ordinate) →
            for_ (M.lookup (abscissa × ordinate) serie.items) \value → E.addItem do
              BCE.assoc { abscissa, ordinate, value }
              E.buildValues do
                E.addValue $ Int.toNumber xIx
                E.addValue $ Int.toNumber yIx
                E.addValue value

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

  abscissaAxisType = fromMaybe Ax.Category do
    ljc ← P.lookup P.abscissa dimMap
    cursor ← ljc ^? D._value ∘ D._projection
    pure $ Ax.axisType cursor axes
  ordinateAxisType = fromMaybe Ax.Category do
    ljc ← P.lookup P.ordinate dimMap
    cursor ← ljc ^? D._value ∘ D._projection
    pure $ Ax.axisType cursor axes

  abscissaAxisCfg = Ax.axisConfiguration abscissaAxisType
  ordinateAxisCfg = Ax.axisConfiguration ordinateAxisType

  xAxes ∷ ∀ i. DSL (addXAxis ∷ ETP.I|i)
  xAxes = enumeratedFor_ heatmapData \(ix × serie) → E.addXAxis do
    mkAxis ix
    E.items $ map ET.strItem $ xValues serie

  yAxes ∷ ∀ i. DSL (addYAxis ∷ ETP.I|i)
  yAxes = enumeratedFor_ heatmapData \(ix × serie) → E.addYAxis do
    mkAxis ix
    E.items $ map ET.strItem $ yValues serie

  sortX ∷ Array String → Array String
  sortX = A.sortBy $ Ax.compareWithAxisType abscissaAxisType

  sortY ∷ Array String → Array String
  sortY = A.sortBy $ Ax.compareWithAxisType ordinateAxisType
