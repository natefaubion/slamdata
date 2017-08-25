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

module SlamData.Workspace.Card.Setups.Viz.Eval.Gauge where

import SlamData.Prelude

import Data.Argonaut (Json, decodeJson, (.?))
import Data.Array as A
import Data.Foldable as F
import Data.List as L
import Data.Variant (prj)
import ECharts.Monad (DSL)
import ECharts.Commands as E
import ECharts.Types.Phantom (OptionI)
import SlamData.Workspace.Card.CardType as CT
import SlamData.Workspace.Card.Port as Port
import SlamData.Workspace.Card.Setups.ColorScheme (colors)
import SlamData.Workspace.Card.Setups.Common as SC
import SlamData.Workspace.Card.Setups.Common.Positioning as BCP
import SlamData.Workspace.Card.Setups.Common.Tooltip as CCT
import SlamData.Workspace.Card.Setups.Common.Eval as BCE
import SlamData.Workspace.Card.Setups.Dimension as D
import SlamData.Workspace.Card.Setups.Semantics as Sem
import SqlSquared as Sql
import SlamData.Workspace.Card.Setups.Viz.Eval.Common (VizEval)
import SlamData.Workspace.Card.Setups.DimensionMap.Projection as P
import SlamData.Workspace.Card.Setups.Auxiliary as Aux
import SlamData.Workspace.Card.Setups.Auxiliary.Gauge as Gauge


eval ∷ ∀ m. VizEval m (P.DimMap → Aux.State → Port.Port → m Port.Out)
eval dimMap aux =
  BCE.chartSetupEval buildSql buildPort aux'
  where
  aux' = prj CT._gauge aux
  buildSql = SC.buildBasicSql (buildProjections dimMap) (buildGroupBy dimMap)
  buildPort r axes = Port.ChartInstructions
    { options: options dimMap axes r ∘ buildData
    , chartType: CT.gauge
    }


type GaugeItem =
  { name ∷ Maybe String
  , value ∷ Number
  }

type GaugeSerie =
  BCP.RadialPosition
  ( name ∷ Maybe String
  , items ∷ Array GaugeItem
  )

type Item =
  { measure ∷ Number
  , parallel ∷ Maybe String
  , multiple ∷ Maybe String
  }

decodeItem ∷ Json → Either String Item
decodeItem = decodeJson >=> \obj → do
  parallel ← Sem.maybeString <$> obj .? "parallel"
  multiple ← Sem.maybeString <$> obj .? "multiple"
  measure ← Sem.requiredNumber zero <$> obj .? "measure"
  pure { measure, parallel, multiple }

buildProjections ∷ ∀ a. P.DimMap → a → L.List (Sql.Projection Sql.Sql)
buildProjections dimMap _ = L.fromFoldable $ A.concat
  [ SC.measureProjection P.value dimMap "measure"
  , SC.dimensionProjection P.multiple dimMap "multiple"
  , SC.dimensionProjection P.parallel dimMap "parallel"
  ]

buildGroupBy ∷ ∀ a. P.DimMap → a → Maybe (Sql.GroupBy Sql.Sql)
buildGroupBy dimMap _ = SC.groupBy
  $ SC.sqlProjection P.parallel dimMap
  <|> SC.sqlProjection P.multiple dimMap

buildData ∷ Array Json → Array GaugeSerie
buildData =
  foldMap (foldMap A.singleton ∘ decodeItem)
    >>> series
    >>> BCP.adjustRadialPositions

  where
  series ∷ Array Item → Array GaugeSerie
  series =
    BCE.groupOn _.parallel
      >>> map \(name × items) →
            { name
            , x: Nothing
            , y: Nothing
            , radius: Nothing
            , items: toPoint <$> items
            }

  toPoint ∷ Item → GaugeItem
  toPoint item = { name: item.multiple, value: item.measure }

options ∷ ∀ a. P.DimMap → a → Gauge.State → Array GaugeSerie → DSL OptionI
options dimMap _ r series = do
  let
    mkRow prj value  = P.lookup prj dimMap # foldMap \dim →
      [ { label: D.jcursorLabel dim, value } ]

    cols = A.fold
      [ mkRow P.value $ CCT.formatForeign ∘ _.value
      , mkRow P.parallel _.seriesName
      , mkRow P.multiple _.name
      ]

  CCT.tooltip do
    E.formatterItem (CCT.tableFormatter (const Nothing) cols ∘ pure)

  E.colors colors

  E.series $ for_ series \serie → E.gauge do
    E.min r.val.min
    E.max r.val.max
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
  allValues ∷ Array Number
  allValues = map _.value $ A.concatMap _.items series
