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

module SlamData.Workspace.Card.Setups.Viz.Eval.Candlestick where

import SlamData.Prelude

import Data.Argonaut (JArray, Json, decodeJson, (.?))
import Data.Array as A
import Data.Lens ((^?))
import Data.List as L
import Data.Map as M
import ECharts.Commands as E
import ECharts.Monad (DSL)
import ECharts.Types as ET
import ECharts.Types.Phantom (OptionI)
import SlamData.Workspace.Card.CardType as CT
import SlamData.Workspace.Card.Port as Port
import SlamData.Workspace.Card.Setups.Axis (Axes)
import SlamData.Workspace.Card.Setups.Axis as Ax
import SlamData.Workspace.Card.Setups.ColorScheme (colors)
import SlamData.Workspace.Card.Setups.Common as SC
import SlamData.Workspace.Card.Setups.Common.Eval (type (>>))
import SlamData.Workspace.Card.Setups.Common.Eval as BCE
import SlamData.Workspace.Card.Setups.Common.Positioning as BCP
import SlamData.Workspace.Card.Setups.Common.Tooltip as CCT
import SlamData.Workspace.Card.Setups.Dimension as D
import SlamData.Workspace.Card.Setups.DimensionMap.Projection as P
import SlamData.Workspace.Card.Setups.Semantics as Sem
import SlamData.Workspace.Card.Setups.Viz.Eval.Common (VizEval)
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

eval ∷ ∀ m. VizEval m (P.DimMap → Port.Port → m Port.Out)
eval dimMap =
  BCE.chartSetupEval buildSql buildPort $ Just unit
  where
  buildPort r axes = Port.ChartInstructions
    { options: options dimMap axes r ∘ buildData
    , chartType: CT.candlestick
    }

  buildSql = SC.buildBasicSql (buildProjections dimMap) (buildGroupBy dimMap)

buildProjections ∷ ∀ a. P.DimMap → a → L.List (Sql.Projection Sql.Sql)
buildProjections dimMap _ = L.fromFoldable $ A.concat
  [ SC.dimensionProjection P.dimension dimMap "dimension"
  , SC.measureProjection P.high dimMap "high"
  , SC.measureProjection P.low dimMap "low"
  , SC.measureProjection P.open dimMap "open"
  , SC.measureProjection P.close dimMap "close"
  , SC.dimensionProjection P.parallel dimMap "parallel"
  ]

buildGroupBy ∷ ∀ a. P.DimMap → a → Maybe (Sql.GroupBy Sql.Sql)
buildGroupBy dimMap _ = SC.groupBy
  $ SC.sqlProjection P.parallel dimMap
  <|> SC.sqlProjection P.dimension dimMap

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

buildData ∷ JArray → Array OnOneGrid
buildData =

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


options ∷ P.DimMap → Axes → Unit → Array OnOneGrid → DSL OptionI
options dimMap axes r kData = do
  CCT.tooltip do
    E.triggerItem
    E.formatterItem \fmt →
      let
        mkRow prj val = P.lookup prj dimMap # foldMap \dim →
          [ D.jcursorLabel dim × val ]
      in CCT.tableRows $ A.concat
        [ mkRow P.dimension fmt.name
        , mkRow P.open $ CCT.formatValueIx 0 fmt
        , mkRow P.close $ CCT.formatValueIx 1 fmt
        , mkRow P.low $ CCT.formatValueIx 2 fmt
        , mkRow P.high $ CCT.formatValueIx 3 fmt
        ]
  BCP.rectangularGrids kData
  BCP.rectangularTitles kData
    $ maybe "" D.jcursorLabel
    $ P.lookup P.parallel dimMap


  E.colors colors
  E.xAxes xAxes
  E.yAxes yAxes
  E.series series

  where
  xValues ∷ OnOneGrid → Array String
  xValues  = sortX ∘ foldMap A.singleton ∘ M.keys ∘ _.items

  xAxisType ∷ Ax.AxisType
  xAxisType = fromMaybe Ax.Category do
    ljc ← P.lookup P.dimension dimMap
    cursor ← ljc ^? D._value ∘ D._projection
    pure $ Ax.axisType cursor axes

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
