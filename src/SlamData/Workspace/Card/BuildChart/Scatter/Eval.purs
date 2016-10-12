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

module SlamData.Workspace.Card.BuildChart.Scatter.Eval
  ( eval
  , module SlamData.Workspace.Card.BuildChart.Scatter.Model
  ) where

import SlamData.Prelude

import Color as C

import Data.Argonaut (JArray, Json)
import Data.Array as A
import Data.Foldable as F
import Data.Lens ((^?))
import Data.Map as M
import Data.Set as Set

import Global (infinity)

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
import SlamData.Workspace.Card.BuildChart.Scatter.Model (Model, ScatterR)
import SlamData.Workspace.Card.CardType.ChartType (ChartType(Scatter))
import SlamData.Workspace.Card.BuildChart.Aggregation as Ag
import SlamData.Workspace.Card.BuildChart.Axis as Ax
import SlamData.Workspace.Card.BuildChart.ColorScheme (colors, getTransparentColor)
import SlamData.Workspace.Card.BuildChart.Semantics (getMaybeString, getValues)
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
  pure $ Port.ChartInstructions axes (buildScatter conf records) Scatter


type ScatterSeries =
  { name ∷ Maybe String
  , items ∷ Array {x ∷ Number, y ∷ Number, r ∷ Number}
  }

buildScatterData ∷ ScatterR → JArray → Array ScatterSeries
buildScatterData r records = series
  where
  -- | maybe series >> array xs × array ys × array rs
  dataMap ∷ Maybe String >> (Array Number × Array Number × Array Number)
  dataMap =
    foldl dataMapFoldFn M.empty records

  dataMapFoldFn
    ∷ Maybe String >> (Array Number × Array Number × Array Number)
    → Json
    → Maybe String >> (Array Number × Array Number × Array Number)
  dataMapFoldFn acc js =
    let
      getValuesFromJson = getValues js
      getMaybeStringFromJson = getMaybeString js

      mbSeries =
        getMaybeStringFromJson =<< r.series
      xs =
        getValuesFromJson $ pure r.abscissa
      ys =
        getValuesFromJson $ pure r.ordinate
      rs =
        getValuesFromJson r.size

      alterSeriesFn
        ∷ Maybe (Array Number × Array Number × Array Number)
        → Maybe (Array Number × Array Number × Array Number)
      alterSeriesFn Nothing =
        Just $ xs × ys × rs
      alterSeriesFn (Just (xxs × yys × rrs)) =
        Just $ (xxs ⊕ xs) × (yys ⊕ ys) × (rrs ⊕ rs)
    in
      M.alter alterSeriesFn mbSeries acc

  series ∷ Array ScatterSeries
  series =
    foldMap mkScatterSeries $ M.toList dataMap

  mkScatterSeries
    ∷ Maybe String × (Array Number × Array Number × Array Number)
    → Array ScatterSeries
  mkScatterSeries (name × items) =
    [{ name
     , items: adjustSymbolSizes $ mkScatterItem items
     }]

  mkScatterItem
    ∷ (Array Number × Array Number × Array Number)
    → Array {x ∷ Number, y ∷ Number, r ∷ Number }
  mkScatterItem (xs × ys × rs)
    | A.null xs = []
    | A.null ys = []
    | otherwise =
      let
        len =
          max (A.length xs) $ max (A.length ys) (A.length rs)

        abscissas =
          case r.abscissaAggregation of
            Nothing → xs
            Just ag →
              let
                v = Ag.runAggregation ag xs
              in
                map (const v) $ A.range 0 $ len - 1
        ordinates =
          case r.ordinateAggregation of
            Nothing → ys
            Just ag →
              let
                v = Ag.runAggregation ag ys
              in
                map (const v) $ A.range 0 $ len - 1

        sizes =
          case join $ r.sizeAggregation of
            Just ag →
              let
                v = Ag.runAggregation ag rs
              in
                map (const v) $ A.range 0 $ len - 1
            Nothing
              | A.null rs →
                map (\_ → r.minSize) rs
            Nothing →
              rs
        zipped = A.zip abscissas $ A.zip ordinates sizes
      in
        zipped <#> \(x × y × r) → {x, y, r}

  adjustSymbolSizes
    ∷ Array {x ∷ Number, y ∷ Number, r ∷ Number}
    → Array {x ∷ Number, y ∷ Number, r ∷ Number}
  adjustSymbolSizes items =
    let
      minValue =
        fromMaybe (-1.0 * infinity) $ map _.r $ F.maximumBy (\a b → compare a.r b.r) items
      maxValue =
        fromMaybe infinity $ map _.r $ F.maximumBy (\a b → compare a.r b.r) items
      distance =
        maxValue - minValue
      sizeDistance =
        r.maxSize - r.minSize

      relativeSize ∷ Number → Number
      relativeSize val
        | distance ≡ zero = val
        | otherwise =
            r.maxSize - sizeDistance / distance * (maxValue - val)
    in
      map (\x → x{r = relativeSize x.r}) items


buildScatter ∷ ScatterR → JArray → DSL OptionI
buildScatter r records = do
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
  E.colors colors

  E.grid BCP.cartesian
  E.xAxis valueAxis
  E.yAxis valueAxis

  E.legend do
    E.textStyle $ E.fontFamily "Ubuntu, sans"
    E.items $ map ET.strItem seriesNames

  E.series series

  where
  scatterData ∷ Array (Int × ScatterSeries)
  scatterData = enumerate $ buildScatterData r records

  valueAxis ∷ ∀ i. DSL (ETP.AxisI i)
  valueAxis = do
    E.axisType ET.Value
    E.axisLabel $ E.textStyle $ E.fontFamily "Ubuntu, sans"
    E.axisLine $ E.lineStyle do
      E.color $ C.rgba 184 184 184 1.0
      E.width 1
    E.splitLine $ E.lineStyle do
      E.color $ C.rgba 204 204 204 0.2
      E.width 1

  seriesNames ∷ Array String
  seriesNames =
    A.fromFoldable $ foldMap (snd ⋙ _.name ⋙ Set.fromFoldable) scatterData

  series ∷ ∀ i. DSL (scatter ∷ ETP.I|i)
  series = for_ scatterData \(ix × serie) → E.scatter do
    for_ serie.name E.name
    for_ (A.index colors $ mod ix $ A.length colors) \color → do
      E.itemStyle $ E.normal $ E.color $ getTransparentColor color 0.5
    E.symbol ET.Circle
    E.buildItems $ for_ serie.items \item → E.addItem $ E.buildValues do
      E.addValue item.x
      E.addValue item.y
      E.addValue item.r
