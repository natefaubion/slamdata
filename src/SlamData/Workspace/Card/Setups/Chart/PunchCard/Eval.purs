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

module SlamData.Workspace.Card.Setups.Chart.PunchCard.Eval
  ( eval
  , module SlamData.Workspace.Card.Setups.Chart.PunchCard.Model
  ) where

import SlamData.Prelude

import Color as C

import Data.Argonaut (Json, decodeJson, (.?))
import Data.Array as A
import Data.Foldable as F
import Data.Foreign (readNumber)
import Data.Int as Int
import Data.List as L
import Data.Map as M
import Data.Set as Set

import ECharts.Monad (DSL)
import ECharts.Commands as E
import ECharts.Types as ET
import ECharts.Types.Phantom (OptionI)
import ECharts.Types.Phantom as ETP

import Global (infinity)

import SlamData.Workspace.Card.CardType.ChartType (ChartType(PunchCard))
import SlamData.Workspace.Card.Port as Port
import SlamData.Workspace.Card.Setups.Axis as Ax
import SlamData.Workspace.Card.Setups.Chart.ColorScheme (colors)
import SlamData.Workspace.Card.Setups.Chart.Common as SCC
import SlamData.Workspace.Card.Setups.Chart.Common.Tooltip as CCT
import SlamData.Workspace.Card.Setups.Chart.PunchCard.Model (ModelR, Model)
import SlamData.Workspace.Card.Setups.Common.Eval (type (>>))
import SlamData.Workspace.Card.Setups.Common.Eval as BCE
import SlamData.Workspace.Card.Setups.Dimension as D
import SlamData.Workspace.Card.Setups.Semantics as Sem

import SqlSquared as Sql

import Utils (hush')
import Utils.Array (enumerate)

eval ∷ ∀ m v. BCE.ChartSetupEval ModelR m v
eval = BCE.chartSetupEval (SCC.buildBasicSql buildProjections buildGroupBy) buildPort

type PunchCardData = (String × String) >> (Int × Number)

type Item =
  { abscissa ∷ String
  , ordinate ∷ String
  , measure ∷ Number
  }

decodeItem ∷ Json → Either String Item
decodeItem = decodeJson >=> \obj → do
  abscissa ← Sem.requiredString "" <$> obj .? "abscissa"
  ordinate ← Sem.requiredString "" <$> obj .? "ordinate"
  measure ← Sem.requiredNumber zero <$> obj .? "measure"
  pure { abscissa, ordinate, measure }

buildProjections ∷ ModelR → L.List (Sql.Projection Sql.Sql)
buildProjections r = L.fromFoldable
  [ r.abscissa # SCC.jcursorPrj # Sql.as "abscissa"
  , r.ordinate # SCC.jcursorPrj # Sql.as "ordinate"
  , r.value # SCC.jcursorPrj # Sql.as "measure" # SCC.applyTransform r.value
  ]

buildGroupBy ∷ ModelR → Maybe (Sql.GroupBy Sql.Sql)
buildGroupBy r =
  SCC.groupBy $ L.fromFoldable
    [ r.abscissa # SCC.jcursorSql
    , r.ordinate # SCC.jcursorSql
    ]

buildPort ∷ ModelR → Ax.Axes → Port.Port
buildPort m axes =
  Port.ChartInstructions
    { options: buildOptions axes m ∘ buildData m
    , chartType: PunchCard
    }

buildData ∷ ModelR → Array Json → PunchCardData
buildData r records = M.fromFoldable $ map addSymbolSize <$> items

  where
  items ∷ Array ((String × String) × Number)
  items =
    records
      # foldMap (foldMap A.singleton ∘ decodeItem)
      # map toPoint

  toPoint ∷ Item → (String × String) × Number
  toPoint item = (item.abscissa × item.ordinate) × item.measure

  aggregated ∷ Array Number
  aggregated = snd <$> items

  minValue ∷ Number
  minValue = fromMaybe (-1.0 * infinity) $ F.minimum aggregated

  maxValue ∷ Number
  maxValue = fromMaybe infinity $ F.maximum aggregated

  distance ∷ Number
  distance = maxValue - minValue

  sizeDistance ∷ Number
  sizeDistance = r.maxSize - r.minSize

  addSymbolSize ∷ Number → Int × Number
  addSymbolSize val
    | distance ≡ zero = (Int.ceil val) × val
    | otherwise =
        (Int.ceil (r.maxSize - sizeDistance / distance * (maxValue - val))) × val

buildOptions ∷ Ax.Axes → ModelR → PunchCardData → DSL OptionI
buildOptions axes r punchCardData = do
  CCT.tooltip do
    E.triggerItem
    E.formatterItemArrayValue \{value} →
      let
        xIx = (map Int.ceil ∘ hush' ∘ readNumber) =<< value A.!! 0
        yIx = (map Int.ceil ∘ hush' ∘ readNumber) =<< value A.!! 1
        val = CCT.formatForeign <$> value A.!! 2
      in
        CCT.tableRows $ A.catMaybes
          [ map (D.jcursorLabel r.abscissa × _) $ xIx >>= A.index abscissaValues
          , map (D.jcursorLabel r.ordinate × _) $ yIx >>= A.index ordinateValues
          , map (D.jcursorLabel r.value × _) val
          ]

  E.colors colors

  when r.circular do
    E.polar $ pure unit
    E.angleAxis abscissaAxis
    E.radiusAxis ordinateAxis

  when (not r.circular) do
    E.xAxis abscissaAxis
    E.yAxis ordinateAxis

    E.grid $ E.containLabel true


  E.series series

  where
  abscissaAxis ∷ ∀ i. DSL (ETP.AxisI i)
  abscissaAxis = do
    E.axisType $ ET.Category
    E.disabledBoundaryGap
    E.splitLine do
      E.shown
      E.lineStyle do
        E.dashedLine
        E.color $ C.rgba 9 9 9 1.0
    E.axisLine E.hidden
    E.items $ map ET.strItem abscissaValues

  ordinateAxis ∷ ∀ i. DSL (ETP.AxisI i)
  ordinateAxis = do
    E.axisType $ ET.Category
    E.axisLine E.hidden
    E.axisLabel $ E.margin $ margin + 2
    E.items $ map ET.strItem ordinateValues

  xAxisType ∷ Ax.AxisType
  xAxisType = D.axisType r.abscissa axes

  yAxisType ∷ Ax.AxisType
  yAxisType = D.axisType r.ordinate axes

  abscissaValues ∷ Array String
  abscissaValues =
    A.sortBy (Ax.compareWithAxisType xAxisType)
      $ A.fromFoldable
      $ Set.fromFoldable
      $ map fst
      $ M.keys punchCardData

  ordinateValues ∷ Array String
  ordinateValues =
    A.sortBy (Ax.compareWithAxisType yAxisType)
      $ A.fromFoldable
      $ Set.fromFoldable
      $ map snd
      $ M.keys punchCardData

  margin ∷ Int
  margin =
    fromMaybe 6
      $ F.maximum
      $ foldMap (\((a × o) × (v × _)) → if Just a ≡ A.head abscissaValues then [v / 2] else [])
      $ asList
      $ M.toUnfoldable punchCardData

  series = E.scatter do
    if r.circular
      then E.polarCoordinateSystem
      else E.cartesianCoordinateSystem
    E.buildItems
      $ for_ (enumerate abscissaValues) \(xIx × abscissa) →
          for_ (enumerate ordinateValues) \(yIx × ordinate) →
            for_ (M.lookup (abscissa × ordinate) punchCardData) \(symbolSize × val) → E.addItem do
              E.symbolSize symbolSize
              E.buildValues do
                E.addValue $ Int.toNumber xIx
                E.addValue $ Int.toNumber yIx
                E.addValue val
