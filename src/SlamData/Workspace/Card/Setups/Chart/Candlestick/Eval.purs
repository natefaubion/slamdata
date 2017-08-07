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

module SlamData.Workspace.Card.Setups.Chart.Candlestick.Eval
  ( eval
  , module SlamData.Workspace.Card.Setups.Chart.Candlestick.Model
  ) where

import SlamData.Prelude

import Data.Argonaut (JArray, Json, decodeJson, (.?))
import Data.Array as A
import Data.List as L
import Data.Map as M

import ECharts.Monad (DSL)
import ECharts.Commands as E
import ECharts.Types as ET
import ECharts.Types.Phantom (OptionI)

import SlamData.Workspace.Card.CardType.ChartType (ChartType(Candlestick))
import SlamData.Workspace.Card.Port as Port
import SlamData.Workspace.Card.Setups.Axis (Axes)
import SlamData.Workspace.Card.Setups.Axis as Ax
import SlamData.Workspace.Card.Setups.Chart.Candlestick.Model (ModelR, Model)
import SlamData.Workspace.Card.Setups.Chart.ColorScheme (colors)
import SlamData.Workspace.Card.Setups.Chart.Common as SCC
import SlamData.Workspace.Card.Setups.Chart.Common.Positioning as BCP
import SlamData.Workspace.Card.Setups.Chart.Common.Tooltip as CCT
import SlamData.Workspace.Card.Setups.Common.Eval (type (>>))
import SlamData.Workspace.Card.Setups.Common.Eval as BCE
import SlamData.Workspace.Card.Setups.Dimension as D
import SlamData.Workspace.Card.Setups.Semantics as Sem

import SqlSquared as Sql

import Utils.Foldable (enumeratedFor_)

type Item =
  { dimension ∷ String
  , high ∷ Number
  , low ∷ Number
  , open ∷ Number
  , close ∷ Number
  , parallel ∷ Maybe String
  }

decodeItem ∷ Json → String ⊹ Item
decodeItem = decodeJson >=> \obj → do
  dimension ← map (fromMaybe "" ∘ Sem.maybeString) $ obj .? "dimension"
  high ← map (fromMaybe zero ∘ Sem.maybeNumber) $ obj .? "high"
  low ← map (fromMaybe zero ∘ Sem.maybeNumber) $ obj .? "low"
  open ← map (fromMaybe zero ∘ Sem.maybeNumber) $ obj .? "open"
  close ← map (fromMaybe zero ∘ Sem.maybeNumber) $ obj .? "close"
  parallel ← map Sem.maybeString $ obj .? "parallel"
  pure { dimension, high, low, open, close, parallel }

eval ∷ ∀ m v. BCE.ChartSetupEval ModelR m v
eval = BCE.chartSetupEval (SCC.buildBasicSql buildProjections buildGroupBy) buildCandlestick

buildProjections ∷ ModelR → L.List (Sql.Projection Sql.Sql)
buildProjections r = L.fromFoldable
  [ r.dimension # SCC.jcursorPrj # Sql.as "dimension"
  , r.high # SCC.jcursorPrj # Sql.as "high" # SCC.applyTransform r.high
  , r.low # SCC.jcursorPrj # Sql.as "low" # SCC.applyTransform r.low
  , r.open # SCC.jcursorPrj # Sql.as "open" # SCC.applyTransform r.open
  , r.close # SCC.jcursorPrj # Sql.as "close" # SCC.applyTransform r.close
  , r.parallel # maybe SCC.nullPrj SCC.jcursorPrj # Sql.as "parallel"
  ]

buildGroupBy ∷ ModelR → Maybe (Sql.GroupBy Sql.Sql)
buildGroupBy r =
  SCC.groupBy $ L.fromFoldable $ A.catMaybes
    [ r.parallel <#> SCC.jcursorSql
    , Just $ r.dimension # SCC.jcursorSql
    ]

buildCandlestick ∷ ModelR → Axes → Port.Port
buildCandlestick m axes =
  Port.ChartInstructions
    { options: kOptions axes m ∘ buildKData
    , chartType: Candlestick
    }

type HLOC a =
  { low ∷ a
  , high ∷ a
  , open ∷ a
  , close ∷ a
  }

type Series = String >> HLOC Number

type OnOneGrid =
  { w ∷ Maybe Number
  , h ∷ Maybe Number
  , x ∷ Maybe Number
  , y ∷ Maybe Number
  , name ∷ Maybe String
  , fontSize ∷ Maybe Int
  , items ∷ Series
  }

buildKData ∷ JArray → Array OnOneGrid
buildKData =
  BCP.adjustRectangularPositions
  ∘ oneGrids
  ∘ foldMap (foldMap A.singleton ∘ decodeItem)
  where
  oneGrids ∷ Array Item → Array OnOneGrid
  oneGrids =
    BCE.groupOn _.parallel
      ⋙ map \(name × is) →
        { name
        , x: Nothing
        , y: Nothing
        , w: Nothing
        , h: Nothing
        , fontSize: Nothing
        , items: M.fromFoldable $ map toPoint is
        }
  toPoint ∷ Item → String × HLOC Number
  toPoint {dimension, high, low, open, close } =
    dimension × { high, low, open, close }


kOptions ∷ Axes → ModelR → Array OnOneGrid → DSL OptionI
kOptions axes r kData = do
  CCT.tooltip do
    E.triggerItem
    E.formatterItem \fmt →
      CCT.tableRows
        [ D.jcursorLabel r.dimension × fmt.name
        , D.jcursorLabel r.open × CCT.formatValueIx 0 fmt
        , D.jcursorLabel r.close × CCT.formatValueIx 1 fmt
        , D.jcursorLabel r.low × CCT.formatValueIx 2 fmt
        , D.jcursorLabel r.high × CCT.formatValueIx 3 fmt
        ]

  BCP.rectangularTitles kData
    $ maybe "" D.jcursorLabel r.parallel
  BCP.rectangularGrids kData

  E.colors colors

  E.xAxes xAxes
  E.yAxes yAxes
  E.series series

  where
  xValues ∷ OnOneGrid → Array String
  xValues  = sortX ∘ foldMap A.singleton ∘ M.keys ∘ _.items

  xAxisType ∷ Ax.AxisType
  xAxisType = D.axisType r.dimension axes

  sortX ∷ Array String → Array String
  sortX = A.sortBy $ Ax.compareWithAxisType xAxisType

  xAxes = enumeratedFor_ kData \(ix × serie) → E.addXAxis do
    E.gridIndex ix
    E.axisType ET.Category
    E.axisLabel $ E.textStyle $ E.fontFamily "Ubuntu, sans"
    E.items $ map ET.strItem $ xValues serie

  yAxes = enumeratedFor_ kData \(ix × _) → E.addYAxis do
    E.gridIndex ix
    E.axisType ET.Value

  series = enumeratedFor_  kData \(ix × serie) → E.candlestick do
    for_ serie.name E.name
    E.xAxisIndex ix
    E.yAxisIndex ix
    E.buildItems $ for_ (xValues serie) \dim →
      for_ (M.lookup dim serie.items) \{high, low, open, close} → E.addItem $ E.buildValues do
        E.addValue open
        E.addValue close
        E.addValue low
        E.addValue high
