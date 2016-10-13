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

module SlamData.Workspace.Card.BuildChart.Boxplot.Eval
  ( eval
  , module SlamData.Workspace.Card.BuildChart.Boxplot.Model
  ) where

import SlamData.Prelude

import Color as C

import Data.Argonaut (JArray, Json)
import Data.Array ((!!))
import Data.Array as A
import Data.Lens ((^?))
import Data.Map as M
import Data.Int as Int
import Data.Set as Set

import ECharts.Monad (DSL)
import ECharts.Commands as E
import ECharts.Types as ET
import ECharts.Types.Phantom (OptionI)
import ECharts.Types.Phantom as ETP

import Quasar.Types (FilePath)

import SlamData.Workspace.Card.BuildChart.Common.Eval (type (>>))
import SlamData.Workspace.Card.BuildChart.Common.Eval as BCE
import SlamData.Workspace.Card.BuildChart.Common.Positioning (rectangularGrids, rectangularTitles, adjustRectangularPositions)
import SlamData.Workspace.Card.BuildChart.Boxplot.Model (Model, BoxplotR)
import SlamData.Workspace.Card.CardType.ChartType (ChartType(Boxplot))
import SlamData.Workspace.Card.BuildChart.Axis as Ax
import SlamData.Workspace.Card.BuildChart.ColorScheme (colors)
import SlamData.Workspace.Card.BuildChart.Semantics (getMaybeString, getValues)
import SlamData.Workspace.Card.Eval.Monad as CEM
import SlamData.Workspace.Card.Port as Port

import Utils.Array (enumerate)

eval
  ∷ Model
  → FilePath
  → CEM.CardEval Port.Port
eval Nothing _ =
  CEM.throw "Please select axis to aggregate"
eval (Just conf) resource = do
  records ← BCE.records resource
  let axes = Ax.buildAxes (A.take 100 records)
  pure $ Port.ChartInstructions axes (buildBoxplot conf records) Boxplot

type OnOneBoxplot =
  { name ∷ Maybe String
  , w ∷ Maybe Number
  , h ∷ Maybe Number
  , x ∷ Maybe Number
  , y ∷ Maybe Number
  , fontSize ∷ Maybe Int
  , series ∷ Array BoxplotSeries
  }

type BoxplotSeries =
  { name ∷ Maybe String
  , items ∷ String >> (Array Number × BoxplotItem)
  }

type BoxplotItem = Maybe
  { low ∷ Number
  , q1 ∷ Number
  , q2 ∷ Number
  , q3 ∷ Number
  , high ∷ Number
  }

boundIQR ∷ Maybe Number
boundIQR = Just 1.5

buildBoxplotData ∷ BoxplotR → JArray → Array OnOneBoxplot
buildBoxplotData r records = series
  where
  -- | maybe parallel >> maybe series >> dimension >> values
  dataMap ∷ Maybe String >> Maybe String >> String >> Array Number
  dataMap =
    foldl dataMapFoldFn M.empty records

  dataMapFoldFn
    ∷ Maybe String >> Maybe String >> String >> Array Number
    → Json
    → Maybe String >> Maybe String >> String >> Array Number
  dataMapFoldFn acc js =
    let
      getMaybeStringFromJson = getMaybeString js
      getValuesFromJson = getValues js
    in case getMaybeStringFromJson r.dimension of
      Nothing → acc
      Just dimensionKey →
        let
          mbParallel =
            getMaybeStringFromJson =<< r.parallel
          mbSeries =
            getMaybeStringFromJson =<< r.series
          values =
            getValuesFromJson $ pure r.value

          alterParallelFn
            ∷ Maybe (Maybe String >> String >> Array Number)
            → Maybe (Maybe String >> String >> Array Number)
          alterParallelFn Nothing =
            Just $ M.singleton mbSeries $ M.singleton dimensionKey values
          alterParallelFn (Just parallel) =
            Just $ M.alter alterSeriesFn mbSeries parallel

          alterSeriesFn
            ∷ Maybe (String >> Array Number)
            → Maybe (String >> Array Number)
          alterSeriesFn Nothing =
            Just $ M.singleton dimensionKey values
          alterSeriesFn (Just dims) =
            Just $ M.alter alterDimFn dimensionKey dims

          alterDimFn
            ∷ Maybe (Array Number)
            → Maybe (Array Number)
          alterDimFn Nothing = Just values
          alterDimFn (Just arr) = Just $ arr ⊕ values
        in
          M.alter alterParallelFn mbParallel acc

  rawSeries ∷ Array OnOneBoxplot
  rawSeries =
    foldMap mkOneBoxplot $ M.toList dataMap

  mkOneBoxplot
    ∷ Maybe String × (Maybe String >> String >> Array Number)
    → Array OnOneBoxplot
  mkOneBoxplot (name × sers) =
    [{ name
     , x: Nothing
     , y: Nothing
     , w: Nothing
     , h: Nothing
     , fontSize: Nothing
     , series: foldMap mkBoxplotSeries $ M.toList sers
     }]

  mkBoxplotSeries
    ∷ Maybe String × (String >> Array Number)
    → Array BoxplotSeries
  mkBoxplotSeries (name × items) =
    [{ name
     , items: map mkItemAndOutliers items
     }]

  mkItemAndOutliers
    ∷ Array Number
    → Array Number × BoxplotItem
  mkItemAndOutliers arr
    | A.null arr = [] × Nothing
    | otherwise =
      let
        sortedArr ∷ Array Number
        sortedArr = A.sort arr

        quantile ∷ Array Number → Number → Maybe Number
        quantile inp p = do
          let
            h = Int.toNumber (A.length inp - one) * p + one
            h' = Int.floor h
            v = arr !! (h' - one)
            v' = inp !! h'
            e = h - Int.toNumber h'
          if e > 0.0
            then case v × v' of
              Just jv × Just jv' → Just $ jv + e * (jv' - jv)
              _ → Nothing
            else v

        q1 = fromMaybe zero $ quantile sortedArr 0.25
        q2 = fromMaybe zero $ quantile sortedArr 0.5
        q3 = fromMaybe zero $ quantile sortedArr 0.75
        iqr = q3 - q1
        low = case boundIQR of
          Just b → q1 - b * iqr
          _ → fromMaybe zero $ A.head sortedArr
        high = case boundIQR of
          Just b → q3 + b * iqr
          _ → fromMaybe zero $ A.last sortedArr

        outliers ∷ Array Number
        outliers = A.filter (\x → x < low ∨ x > high) sortedArr
      in
        outliers × Just {low, q1, q2, q3, high}

  series ∷ Array OnOneBoxplot
  series = adjustRectangularPositions rawSeries

buildBoxplot ∷ BoxplotR → JArray → DSL OptionI
buildBoxplot r records = do
  E.tooltip do
    E.triggerItem
    E.textStyle do
      E.fontFamily "Ubuntu, sans"
      E.fontSize 12

  rectangularTitles $ map snd boxplotData

  rectangularGrids $ map snd boxplotData


  E.legend do
    E.topBottom
    E.textStyle $ E.fontFamily "Ubuntu, sans"
    E.items $ map ET.strItem serieNames

  E.xAxes xAxes

  E.yAxes yAxes

  E.series series

  where
  boxplotData ∷ Array (Int × OnOneBoxplot)
  boxplotData = enumerate $ buildBoxplotData r records

  serieNames ∷ Array String
  serieNames =
    A.fromFoldable
      $ foldMap (snd
                 ⋙_.series
                 ⋙ foldMap (_.name
                            ⋙ foldMap Set.singleton))
      boxplotData

--  series ∷ ∀ i. DSL (scatter ∷ ETP.I, boxplot ∷ ETP.I|i)
  series = for_ boxplotData \(ix × onOnePlot) → for_ onOnePlot.series \serie → do
    E.boxPlot $ boxplotSerie $ ix × serie
    E.scatter $ scatterSerie $ ix × serie

--  boxplotSerie ∷ ∀ i. Int × _ → DSL (boxplot ∷ ETP.I|i)
  boxplotSerie (ix × serie) = do
    for_ serie.name E.name

    E.xAxisIndex ix
    E.yAxisIndex ix

    E.itemStyle $ E.normal do
      E.borderWidth 2
      E.borderColor
        $ fromMaybe (C.rgba 0 0 0 0.5)
        $ serie.name
        >>= flip A.elemIndex serieNames
        >>= (colors !! _)

    E.tooltip $ E.formatterItemArrayValue \param →
      param.name ⊕ "<br/>"
      ⊕ "Upper: " ⊕ show (fromMaybe zero $ param.value !! 4) ⊕ "<br/>"
      ⊕ "Q3: " ⊕ show (fromMaybe zero $ param.value !! 3) ⊕ "<br/>"
      ⊕ "Median: " ⊕ show (fromMaybe zero $ param.value !! 2) ⊕ "<br/>"
      ⊕ "Q2: " ⊕ show (fromMaybe zero $ param.value !! 1)⊕ "<br/>"
      ⊕ "Lower: " ⊕ show (fromMaybe zero $ param.value !! 0)

    E.buildItems
      $ for_ xAxisLabels \key → case M.lookup key serie.items of
        Nothing → E.missingItem
        Just (_ × mbBP) → for_ mbBP \item → E.addItem $ E.buildValues do
          E.addValue item.low
          E.addValue item.q1
          E.addValue item.q2
          E.addValue item.q3
          E.addValue item.high


--  scatterSerie ∷ ∀ i. Int × _ → DSL (scatter ∷ ETP.I|i)
  scatterSerie (ix × serie) = do
    for_ serie.name E.name
    E.xAxisIndex ix
    E.yAxisIndex ix
    E.symbolSize
      if isNothing serie.name ∨ serie.name ≡ Just ""
        then 5
        else 0
    E.itemStyle $ E.normal
      $ E.color
      $ fromMaybe (C.rgba 0 0 0 0.5)
      $ serie.name
      >>= flip A.elemIndex serieNames
      >>= (colors !! _)

    E.tooltip $ E.formatterItemArrayValue \param →
      param.name ⊕ "<br/>"
      ⊕ show (fromMaybe zero $ param.value !! 1)

    E.buildItems
      $ for_ (enumerate $ A.fromFoldable serie.items) \(ox × (outliers × _)) →
          for_ outliers \outlier → E.addItem $ E.buildValues do
            E.addValue $ Int.toNumber ox
            E.addValue outlier

  grids ∷ Array (DSL ETP.GridI)
  grids = boxplotData <#> \(_ × {x, y, w, h}) → do
    for_ x $ E.left ∘ ET.Percent
    for_ y $ E.top ∘ ET.Percent
    for_ w E.widthPct
    for_ h E.heightPct

  titles ∷ Array (DSL ETP.TitleI)
  titles = boxplotData <#> \(_ × {x, y, name, fontSize}) → do
    for_ name E.text
    E.textStyle do
      E.fontFamily "Ubuntu, sans"
      for_ fontSize E.fontSize
    for_ x $ E.left ∘ ET.Percent
    for_ y $ E.top ∘ ET.Percent
    E.textCenter
    E.textMiddle

  xAxisLabels ∷ Array String
  xAxisLabels =
    A.fromFoldable
      $ foldMap (snd ⋙
                 _.series
                 ⋙ foldMap (_.items
                            ⋙ M.keys
                            ⋙ Set.fromFoldable))
      boxplotData

  xAxes = for_ boxplotData \(ix × _) → E.addXAxis do
    E.gridIndex ix
    E.axisType ET.Category
    E.axisLabel do
      E.textStyle do
        E.fontFamily "Ubuntu, sans"
    E.axisLine $ E.lineStyle do
      E.width 1
    E.splitLine $ E.lineStyle do
      E.width 1
    E.splitArea E.hidden
    E.items $ map ET.strItem xAxisLabels


  yAxes = for_ boxplotData \(ix × _) → E.addYAxis do
    E.gridIndex ix
    E.axisType ET.Value
    E.axisLabel do
      E.textStyle do
        E.fontFamily "Ubuntu, sans"
    E.axisLine $ E.lineStyle do
      E.width 1
    E.splitLine $ E.lineStyle do
      E.width 1
    E.splitArea E.hidden
