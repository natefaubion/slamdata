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

module SlamData.Workspace.Card.Setups.Viz.Eval.Radar where

import SlamData.Prelude

import Data.Argonaut (Json, decodeJson, (.?))
import Data.Array as A
import Data.Foldable as F
import Data.Int as Int
import Data.List as L
import Data.Map as M
import Data.Set as Set
import ECharts.Commands as E
import ECharts.Monad (DSL)
import ECharts.Types as ET
import ECharts.Types.Phantom (OptionI)
import ECharts.Types.Phantom as ETP
import SlamData.Workspace.Card.CardType as CT
import SlamData.Workspace.Card.Port as Port
import SlamData.Workspace.Card.Setups.Axis (Axes)
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
import Utils.Array (enumerate)

eval ∷ ∀ m. VizEval m (P.DimMap → Port.Port → m Port.Out)
eval dimMap =
  BCE.chartSetupEval buildSql buildPort $ Just unit
  where
  buildPort r axes = Port.ChartInstructions
    { options: options dimMap axes r ∘ buildData
    , chartType: CT.radar
    }

  buildSql = SC.buildBasicSql (buildProjections dimMap) (buildGroupBy dimMap)

-- | One radar serie. Actually just data for echarts radar series
type RadarSerie =
  { name ∷ Maybe String
  , items ∷ String >> Number
  }

-- | All series that are drawn on one radar
type SeriesOnRadar =
  BCP.RadialPosition
    ( name ∷ Maybe String
    , series ∷ Array RadarSerie
    )

type Item =
  { category ∷ String
  , measure ∷ Number
  , multiple ∷ Maybe String
  , parallel ∷ Maybe String
  }

decodeItem ∷ Json → Either String Item
decodeItem = decodeJson >=> \obj → do
  category ← Sem.requiredString "" <$> obj .? "category"
  measure ← Sem.requiredNumber zero <$> obj .? "measure"
  multiple ← Sem.maybeString <$> obj .? "multiple"
  parallel ← Sem.maybeString <$> obj .? "parallel"
  pure { category, measure, multiple, parallel }

buildProjections ∷ ∀ a. P.DimMap → a → L.List (Sql.Projection Sql.Sql)
buildProjections dimMap a = L.fromFoldable $ A.concat
  [ SC.dimensionProjection P.category dimMap "category"
  , SC.measureProjection P.value dimMap "measure"
  , SC.dimensionProjection P.multiple dimMap "multiple"
  , SC.dimensionProjection P.parallel dimMap "parallel"
  ]

buildGroupBy ∷ ∀ a. P.DimMap → a → Maybe (Sql.GroupBy Sql.Sql)
buildGroupBy dimMap _ = SC.groupBy
  $ SC.sqlProjection P.parallel dimMap
  <|> SC.sqlProjection P.multiple dimMap
  <|> SC.sqlProjection P.category dimMap


buildData ∷ Array Json → Array SeriesOnRadar
buildData =
  foldMap (foldMap A.singleton ∘ decodeItem)
    >>> series
    >>> BCP.adjustRadialPositions

  where
  series ∷ Array Item → Array SeriesOnRadar
  series =
    BCE.groupOn _.parallel
      >>> map \(name × items) →
            { name
            , x: Nothing
            , y: Nothing
            , radius: Nothing
            , series: multiples items
            }

  multiples ∷ Array Item → Array RadarSerie
  multiples =
    BCE.groupOn _.multiple
      >>> map \(name × items) →
            { name
            , items: M.fromFoldable $ toPoint <$> items
            }

  toPoint ∷ Item → Tuple String Number
  toPoint item = item.category × item.measure

options ∷ P.DimMap → Axes → Unit → Array SeriesOnRadar  → DSL OptionI
options dimMap axes r radarData = do
  CCT.tooltip do
    E.triggerItem

  E.legend do
    E.items $ map ET.strItem serieNames
    E.textStyle do
      E.fontFamily "Ubuntu, sans"
      E.fontSize 12
    E.orient ET.Vertical
    E.leftLeft

  E.colors colors

  E.radars
    $ traverse_ E.radar radars

  E.series
    $ traverse_ E.radarSeries series

  BCP.radialTitles radarData
    $ maybe "" D.jcursorLabel
    $ P.lookup P.parallel dimMap

  where
  serieNames ∷ Array String
  serieNames =
    A.fromFoldable
      $ foldMap (_.series
                 ⋙ foldMap (_.name ⋙ Set.fromFoldable))
        radarData

  series ∷ Array (DSL ETP.RadarSeriesI)
  series = (enumerate radarData) <#> \(ix × {series: ss }) → do
    E.radarIndex $ Int.toNumber ix
    E.symbol ET.Circle
    let
      allKeys = foldMap (Set.fromFoldable ∘ M.keys ∘ _.items) ss
    E.buildItems $ for_ ss \serie → E.addItem do
      for_ serie.name E.name
      E.buildValues $ for_ allKeys \k →
        case M.lookup k serie.items of
          Nothing → E.missingValue
          Just val → E.addValue val

  minVal ∷ Maybe Number
  minVal =
    F.minimum
      $ map (fromMaybe zero
             ∘ F.minimum
             ∘ map (fromMaybe zero
                    ∘ F.minimum
                    ∘ M.values
                    ∘ _.items)
             ∘ _.series
            )
      radarData

  maxVal ∷ Maybe Number
  maxVal =
    F.maximum
      $ map (fromMaybe zero
             ∘ F.maximum
             ∘ map (fromMaybe zero
                    ∘ F.maximum
                    ∘ M.values
                    ∘ _.items)
             ∘ _.series
            )
      radarData

  radars ∷ Array (DSL ETP.RadarI)
  radars = radarData <#> \{name, series: ss, x, y, radius} → do
    let
      allKeys = foldMap (Set.fromFoldable ∘ M.keys ∘ _.items) ss

    E.indicators $ for_ allKeys \k → E.indicator do
      E.name k
      for_ minVal E.min
      for_ maxVal E.max

    E.nameGap 10.0

    E.buildCenter do
      traverse_ (E.setX ∘ E.percents) x
      traverse_ (E.setY ∘ E.percents) y

    traverse_
      (E.singleValueRadius
       ∘ ET.SingleValueRadius
       ∘ ET.Percent) radius
