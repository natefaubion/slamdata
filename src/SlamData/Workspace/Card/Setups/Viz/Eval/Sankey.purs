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

module SlamData.Workspace.Card.Setups.Viz.Eval.Sankey where

import SlamData.Prelude

import Data.Argonaut (JArray, Json, decodeJson, (.?))
import Data.Array as A
import Data.List as L
import ECharts.Commands as E
import ECharts.Monad (DSL)
import ECharts.Types.Phantom (OptionI)
import ECharts.Types.Phantom as ETP
import SlamData.Workspace.Card.CardType as CT
import SlamData.Workspace.Card.Port as Port
import SlamData.Workspace.Card.Setups.ColorScheme (colors)
import SlamData.Workspace.Card.Setups.Common as SC
import SlamData.Workspace.Card.Setups.Common.Eval as BCE
import SlamData.Workspace.Card.Setups.Common.Tooltip as CCT
import SlamData.Workspace.Card.Setups.Dimension as D
import SlamData.Workspace.Card.Setups.DimensionMap.Projection as P
import SlamData.Workspace.Card.Setups.Semantics as Sem
import SlamData.Workspace.Card.Setups.Viz.Eval.Common (VizEval)
import SqlSquared as Sql

type Item =
  { source ∷ String
  , target ∷ String
  , weight ∷ Number
  }

decodeItem ∷ Json → String ⊹ Item
decodeItem = decodeJson >=> \obj → do
  source ← map (fromMaybe "" ∘ Sem.maybeString) $ obj .? "source"
  target ← map (fromMaybe "" ∘ Sem.maybeString) $ obj .? "target"
  weight ← map (fromMaybe zero ∘ Sem.maybeNumber) $ obj .? "weight"
  pure { source, target, weight }

eval ∷ ∀ m. VizEval m (P.DimMap → Port.Port → m Port.Out)
eval dimMap =
  BCE.chartSetupEval buildSql buildPort $ Just unit
  where
  buildPort r axes = Port.ChartInstructions
    { options: options dimMap axes r ∘ buildData
    , chartType: CT.sankey
    }

  buildSql = SC.buildBasicSql (buildProjections dimMap) (buildGroupBy dimMap)

buildProjections ∷ ∀ a. P.DimMap → a → L.List (Sql.Projection Sql.Sql)
buildProjections dimMap _ = L.fromFoldable $ A.concat
  [ SC.dimensionProjection P.source dimMap "source"
  , SC.dimensionProjection P.target dimMap "target"
  , SC.measureProjection P.value dimMap "weight"
  ]

buildGroupBy ∷ ∀ a. P.DimMap → a → Maybe (Sql.GroupBy Sql.Sql)
buildGroupBy dimMap _ = SC.groupBy
  $ SC.sqlProjection P.source dimMap
  <|> SC.sqlProjection P.target dimMap



buildData ∷ JArray → Array Item
buildData =
  foldMap $ foldMap A.singleton ∘ decodeItem

options ∷ ∀ ax a. P.DimMap → ax → a → Array Item → DSL OptionI
options dimMap _ _ sankeyData = do
  let
    mkRow prj value  = P.lookup prj dimMap # foldMap \dim →
      [ { label: D.jcursorLabel dim, value } ]
    cols = A.fold
      [ mkRow P.source $ CCT.formatDataProp "source"
      , mkRow P.target $ CCT.formatDataProp "target"
      , mkRow P.value $ CCT.formatDataProp "value"
      ]
  CCT.tooltip do
    E.formatterItem (CCT.tableFormatter (const Nothing) cols ∘ pure)
    E.triggerItem

  E.colors colors

  E.series $ E.sankey do
    E.buildItems items
    E.buildLinks links

    E.lineStyle $ E.normal $ E.curveness 0.3
  where
  links ∷ DSL ETP.LinksI
  links = for_ sankeyData \item → E.addLink do
    E.sourceName item.source
    E.targetName item.target
    E.value item.weight

  items ∷ DSL ETP.ItemsI
  items =
    for_
      (A.nub $ (_.source <$> sankeyData) ⊕ (_.target <$> sankeyData))
      (E.addItem ∘ E.name)
