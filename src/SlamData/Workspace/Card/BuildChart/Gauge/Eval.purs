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

module SlamData.Workspace.Card.BuildChart.Gauge.Eval
  ( eval
  , module SlamData.Workspace.Card.BuildChart.Gauge.Model
  ) where

import SlamData.Prelude

import Data.Argonaut (JArray, Json)
import Data.Array as A
import Data.Foldable as F
import Data.Lens ((^?))
import Data.Map as M

import ECharts.Monad (DSL)
import ECharts.Commands as E
import ECharts.Types.Phantom (OptionI)

import Quasar.Types (FilePath)

import SlamData.Quasar.Class (class QuasarDSL)
import SlamData.Quasar.Error as QE
import SlamData.Workspace.Card.BuildChart.Common.Eval (type (>>))
import SlamData.Workspace.Card.BuildChart.Common.Eval as BCE
import SlamData.Workspace.Card.BuildChart.Gauge.Model (Model, GaugeR)
import SlamData.Workspace.Card.CardType.ChartType (ChartType(Gauge))
import SlamData.Workspace.Card.BuildChart.Aggregation as Ag
import SlamData.Workspace.Card.BuildChart.Axis as Ax
import SlamData.Workspace.Card.BuildChart.ColorScheme (colors)
import SlamData.Workspace.Card.BuildChart.Semantics (getMaybeString, getValues)
import SlamData.Workspace.Card.Eval.CardEvalT as CET
import SlamData.Workspace.Card.Port as Port
import SlamData.Workspace.Card.BuildChart.Common.Positioning (RadialPosition, adjustRadialPositions)


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
  pure $ Port.ChartInstructions axes (buildGauge conf records) Gauge

----------------------------------------------------------------------
-- GAUGE BUILDER
----------------------------------------------------------------------

type GaugeItem =
  { name ∷ Maybe String
  , value ∷ Number
  }

type GaugeSerie =
  RadialPosition
  ( name ∷ Maybe String
  , items ∷ Array GaugeItem
  )

buildGaugeData ∷ GaugeR → JArray → Array GaugeSerie
buildGaugeData r records = series
  where
  valueAndSizeMap ∷ Maybe String >> Maybe String >> Array Number
  valueAndSizeMap =
    foldl foldFn M.empty records

  foldFn
    ∷ Maybe String >> Maybe String >> Array Number
    → Json
    → Maybe String >> Maybe String >> Array Number
  foldFn acc js =
    let
       getMaybeStringFromJson = getMaybeString js
       getValuesFromJson = getValues js

       mbParallel =
         getMaybeStringFromJson =<< r.parallel
       mbMultiple =
         getMaybeStringFromJson =<< r.multiple
       values =
         getValuesFromJson $ pure r.value

       alterFn
         ∷ Maybe (Maybe String >> Array Number)
         → Maybe (Maybe String >> Array Number)
       alterFn Nothing = Just $ M.singleton mbMultiple values
       alterFn (Just multiple) =
         Just $ M.alter alterMultiple mbMultiple multiple

       alterMultiple
         ∷ Maybe (Array Number)
         → Maybe (Array Number)
       alterMultiple Nothing = Just values
       alterMultiple (Just arr) = Just $ arr ⊕ values
    in
      M.alter alterFn mbParallel acc

  mkSerie ∷ Maybe String × (Maybe String >> Array Number) → Array GaugeSerie
  mkSerie (name × mp) =
    let
      pairs ∷ Array (Maybe String × Array Number)
      pairs = A.fromFoldable $ M.toList mp
      items = pairs <#> \(name × values) →
        { name
        , value: Ag.runAggregation r.valueAggregation values
        }

    in [ { radius: Nothing
         , items
         , x: Nothing
         , y: Nothing
         , name
         }
       ]

  unpositionedSeries ∷ Array GaugeSerie
  unpositionedSeries =
    foldMap mkSerie $ M.toList valueAndSizeMap

  series ∷ Array GaugeSerie
  series = adjustRadialPositions unpositionedSeries


buildGauge ∷ GaugeR → JArray → DSL OptionI
buildGauge r records = do
  E.tooltip do
    E.textStyle do
      E.fontFamily "Ubuntu, sans"
      E.fontSize 12

  E.colors colors

  E.series $ for_ series \serie → E.gauge do
    for_ serie.name E.name
    E.axisLine $ E.lineStyle $ E.setWidth $ E.pixels 10
    E.splitLine $ E.length 20
    traverse_ (E.buildGaugeRadius ∘ E.percents) serie.radius

    E.buildCenter do
      traverse_ (E.setX ∘ E.percents) serie.x
      traverse_ (E.setY ∘ E.percents) serie.y

    when (A.length serie.items > 1)
      $ E.title E.hidden

    E.detail do
      traverse_ E.formatterString serie.name
      when (A.length series < 2 ∧ A.length serie.items > 1) E.hidden
      E.buildOffsetCenter do
        E.setX $ E.percents zero
        E.setY $ E.percents 65.0
      E.textStyle do
        E.fontSize 16
        E.fontFamily "Ubuntu, sans"
        for_ (A.head colors) E.color


    if (A.length allValues > 1)
      then do
      for_ (F.minimum allValues) E.min
      for_ (F.maximum allValues) E.max
      else
      for_ (A.head allValues) \v → do
        E.min $ v / 2.0
        E.max $ v * 1.5

    E.buildItems
      $ for_ serie.items \item → E.addItem do
        E.value item.value
        traverse_ E.name item.name

  where
  series ∷ Array GaugeSerie
  series = buildGaugeData r records

  allValues ∷ Array Number
  allValues = map _.value $ A.concatMap _.items series
