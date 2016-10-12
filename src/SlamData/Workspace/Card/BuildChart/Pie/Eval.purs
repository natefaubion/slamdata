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

module SlamData.Workspace.Card.BuildChart.Pie.Eval
  ( eval
  , module SlamData.Workspace.Card.BuildChart.Pie.Model
  ) where

import SlamData.Prelude

import Data.Argonaut (JArray, Json)
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
import SlamData.Workspace.Card.BuildChart.Pie.Model (Model, PieR)
import SlamData.Workspace.Card.BuildChart.Common.Positioning (adjustRadialPositions, adjustDonutRadiuses, RadialPosition, WithDonutRadius, radialTitles)
import SlamData.Workspace.Card.CardType.ChartType (ChartType(Pie))
import SlamData.Workspace.Card.BuildChart.Aggregation as Ag
import SlamData.Workspace.Card.BuildChart.Axis as Ax
import SlamData.Workspace.Card.BuildChart.ColorScheme (colors)
import SlamData.Workspace.Card.BuildChart.Semantics (getMaybeString, getValues)
import SlamData.Workspace.Card.Eval.CardEvalT as CET
import SlamData.Workspace.Card.Port as Port


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
  pure $ Port.ChartInstructions axes (buildPie conf records) Pie


type OnePieSeries =
  RadialPosition
  ( series ∷ Array DonutSeries
  , name ∷ Maybe String
  )

type DonutSeries =
  WithDonutRadius
  ( name ∷ Maybe String
  , items ∷ String >> Number
  )

buildPieData ∷ PieR → JArray → Array OnePieSeries
buildPieData r records = series
  where
  -- | maybe parallel >> maybe donut >> category name >> values
  dataMap ∷ Maybe String >> Maybe String >> String >> Array Number
  dataMap =
    foldl dataMapFoldFn M.empty records

  dataMapFoldFn
    ∷ Maybe String >> Maybe String >> String >> Array Number
    → Json
    → Maybe String >> Maybe String >> String >> Array Number
  dataMapFoldFn acc js =
    let
      getValuesFromJson = getValues js
      getMaybeStringFromJson = getMaybeString js
    in case getMaybeStringFromJson r.category of
      Nothing → acc
      Just categoryKey →
        let
          mbParallel =
            getMaybeStringFromJson =<< r.parallel
          mbDonut =
            getMaybeStringFromJson =<< r.donut
          values =
            getValuesFromJson $ pure r.value

          alterParallelFn
            ∷ Maybe (Maybe String >> String >> Array Number)
            → Maybe (Maybe String >> String >> Array Number)
          alterParallelFn Nothing =
            Just $ M.singleton mbDonut $ M.singleton categoryKey values
          alterParallelFn (Just donut) =
            Just $ M.alter alterDonutFn mbDonut donut

          alterDonutFn
            ∷ Maybe (String >> Array Number)
            → Maybe (String >> Array Number)
          alterDonutFn Nothing =
            Just $ M.singleton categoryKey values
          alterDonutFn (Just category) =
            Just $ M.alter alterCategoryFn categoryKey category

          alterCategoryFn
            ∷ Maybe (Array Number)
            → Maybe (Array Number)
          alterCategoryFn Nothing = Just values
          alterCategoryFn (Just arr) = Just $ arr ⊕ values
        in
          M.alter alterParallelFn mbParallel acc


  rawSeries ∷ Array OnePieSeries
  rawSeries =
    foldMap mkOnePieSeries $ M.toList dataMap

  mkOnePieSeries
    ∷ Maybe String × (Maybe String >> String >> Array Number)
    → Array OnePieSeries
  mkOnePieSeries (name × donutSeries) =
    [{ name
     , x: Nothing
     , y: Nothing
     , radius: Nothing
     , series: foldMap mkDonutSeries $ M.toList donutSeries
     }]

  mkDonutSeries
    ∷ Maybe String × (String >> Array Number)
    → Array DonutSeries
  mkDonutSeries (name × items) =
    [{ name
     , radius: Nothing
     , items: map (Ag.runAggregation r.valueAggregation) items
     }]

  series ∷ Array OnePieSeries
  series = map (\x → x{series = adjustDonutRadiuses x.series}) $ adjustRadialPositions rawSeries

buildPie ∷ PieR → JArray → DSL OptionI
buildPie r records = do
  E.tooltip E.triggerItem

  E.colors colors

  E.legend do
    E.textStyle do
      E.fontSize 12
      E.fontFamily "Ubuntu, sans"
    E.items $ map ET.strItem legendNames
    E.orient ET.Vertical
    E.leftLeft

  E.series series

  radialTitles pieData

  where
  pieData ∷ Array OnePieSeries
  pieData = buildPieData r records

  itemNames ∷ Array String
  itemNames =
    A.fromFoldable
      $ foldMap (_.series
                 ⋙ foldMap (_.items
                            ⋙ M.keys
                            ⋙ Set.fromFoldable)
                )
        pieData

  seriesNames ∷ Array String
  seriesNames =
    A.fromFoldable
      $ foldMap (_.series ⋙ foldMap (_.name ⋙ Set.fromFoldable)) pieData

  legendNames ∷ Array String
  legendNames
    | A.null seriesNames = itemNames
    | otherwise = do
      s ← seriesNames
      i ← itemNames
      pure $ s ⊕ ":" ⊕ i

  series ∷ ∀ i. DSL (pie ∷ ETP.I|i)
  series = for_ pieData \{x, y, radius: parallelR, series} →
    for_ series \{radius, items, name} → E.pie do
      E.label do
        E.normal E.hidden
        E.emphasis E.hidden

      E.buildCenter do
        traverse_ (E.setX ∘ E.percents) x
        traverse_ (E.setY ∘ E.percents) y

      for_ parallelR \pR →
        for_ radius \{start, end} → E.buildRadius do
          E.setStart $ E.percents $ start * pR
          E.setEnd $ E.percents $ end * pR

      for_ name E.name

      E.buildItems $ for_ (M.toList $ items) \(key × value) →
        E.addItem do
          E.value value
          E.name $ foldMap (flip append ":") name ⊕ key
