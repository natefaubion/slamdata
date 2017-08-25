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

module SlamData.Workspace.Card.Setups.Viz.Eval.Funnel where

import SlamData.Prelude

import Data.Argonaut (Json, decodeJson, (.?))
import Data.Array as A
import Data.List as L
import Data.Map as M
import Data.Set as Set
import Data.Variant (prj)
import ECharts.Monad (DSL)
import ECharts.Commands as E
import ECharts.Types as ET
import ECharts.Types.Phantom (OptionI)
import ECharts.Types.Phantom as ETP
import SlamData.Common.Align (Align(..))
import SlamData.Common.Sort (Sort(..))
import SlamData.Workspace.Card.CardType as CT
import SlamData.Workspace.Card.Port as Port
import SlamData.Workspace.Card.Setups.ColorScheme (colors)
import SlamData.Workspace.Card.Setups.Common as SC
import SlamData.Workspace.Card.Setups.Common.Positioning as BCP
import SlamData.Workspace.Card.Setups.Common.Tooltip as CCT
import SlamData.Workspace.Card.Setups.Common.Eval (type (>>))
import SlamData.Workspace.Card.Setups.Common.Eval as BCE
import SlamData.Workspace.Card.Setups.Dimension as D
import SlamData.Workspace.Card.Setups.Semantics as Sem
import SqlSquared as Sql
import SlamData.Workspace.Card.Setups.Viz.Eval.Common (VizEval)
import SlamData.Workspace.Card.Setups.DimensionMap.Projection as P
import SlamData.Workspace.Card.Setups.Auxiliary as Aux
import SlamData.Workspace.Card.Setups.Auxiliary.Funnel as Funnel

eval ∷ ∀ m. VizEval m (P.DimMap → Aux.State → Port.Port → m Port.Out)
eval dimMap aux =
  BCE.chartSetupEval buildSql buildPort aux'
  where
  aux' = prj CT._funnel aux
  buildSql = SC.buildBasicSql (buildProjections dimMap) (buildGroupBy dimMap)
  buildPort r axes = Port.ChartInstructions
    { options: options dimMap axes r ∘ buildData
    , chartType: CT.funnel
    }

type FunnelSeries =
  { name ∷ Maybe String
  , items ∷ String >> Number
  , x ∷ Maybe Number
  , y ∷ Maybe Number
  , w ∷ Maybe Number
  , h ∷ Maybe Number
  , fontSize ∷ Maybe Int
  }

type Item =
  { category ∷ String
  , measure ∷ Number
  , series ∷ Maybe String
  }

decodeItem ∷ Json → Either String Item
decodeItem = decodeJson >=> \obj → do
  category ← Sem.requiredString "" <$> obj .? "category"
  measure ← Sem.requiredNumber zero <$> obj .? "measure"
  series ← Sem.maybeString <$> obj .? "series"
  pure { category, measure, series }

buildProjections ∷ ∀ a. P.DimMap → a → L.List (Sql.Projection Sql.Sql)
buildProjections dimMap _ = L.fromFoldable $ A.concat
  [ SC.dimensionProjection P.category dimMap "category"
  , SC.measureProjection P.value dimMap "measure"
  , SC.dimensionProjection P.series dimMap "series"
  ]

buildGroupBy ∷ ∀ a. P.DimMap → a → Maybe (Sql.GroupBy Sql.Sql)
buildGroupBy dimMap _ = SC.groupBy
  $ SC.sqlProjection P.series dimMap
  <|> SC.sqlProjection P.category dimMap

buildData ∷ Array Json → Array FunnelSeries
buildData =
  foldMap (foldMap A.singleton ∘ decodeItem)
    >>> series
    >>> BCP.adjustRectangularPositions

  where
  series ∷ Array Item → Array FunnelSeries
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

  toPoint ∷ Item → String × Number
  toPoint item = item.category × item.measure

options ∷ ∀ ax. P.DimMap → ax → Funnel.State → Array FunnelSeries → DSL OptionI
options dimMap _ r funnelData = do
  let
    mkRow prj value  = P.lookup prj dimMap # foldMap \dim →
      [ { label: D.jcursorLabel dim, value } ]

    cols = A.fold
      [ mkRow P.category _.name
      , mkRow P.value $ CCT.formatForeign ∘ _.value
      , mkRow P.series _.seriesName
      ]

  CCT.tooltip do
    E.formatterItem (CCT.tableFormatter (pure ∘ _.color) cols ∘ pure)
    E.triggerItem

  E.legend do
    E.items $ map ET.strItem legendNames
    E.topBottom
    E.textStyle do
      E.fontFamily "Ubuntu, sans"

  E.colors colors

  BCP.rectangularTitles funnelData
    $ maybe "" D.jcursorLabel
    $ P.lookup P.series dimMap
  E.series series

  where
  legendNames ∷ Array String
  legendNames =
    A.fromFoldable
      $ foldMap (_.items ⋙ M.keys ⋙ Set.fromFoldable) funnelData

  series ∷ ∀ i. DSL (funnel ∷ ETP.I|i)
  series = for_ funnelData \{x, y, w, h, items, name: serieName} → E.funnel do
    traverse_ E.widthPct w
    traverse_ E.heightPct h
    for_ serieName E.name
    case r.order of
      Asc → E.ascending
      Desc → E.descending
    case r.align of
      LeftAlign → E.funnelLeft
      RightAlign → E.funnelRight
      CenterAlign → E.funnelCenter
    E.label do
      E.normal do
        E.textStyle $ E.fontFamily "Ubuntu, sans"
        E.positionInside
      E.emphasis do
        E.textStyle $ E.fontFamily "Ubuntu, sans"
        E.positionInside
    E.buildItems $ for_ (asList $ M.toUnfoldable items) \(name × value) → E.addItem do
      E.name name
      E.value value
    traverse_ (E.top ∘ ET.Percent) y
    traverse_ (E.left ∘ ET.Percent) x
