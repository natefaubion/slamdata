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

module SlamData.Workspace.Card.BuildChart.Sankey.Eval
  ( eval
  , module SlamData.Workspace.Card.BuildChart.Sankey.Model
  ) where

import SlamData.Prelude

import Data.Argonaut (JArray, Json)
import Data.Array as A
import Data.Lens ((^?))
import Data.Map as M

import ECharts.Monad (DSL)
import ECharts.Commands as E
import ECharts.Types.Phantom (OptionI)
import ECharts.Types.Phantom as ETP

import Quasar.Types (FilePath)

import SlamData.Workspace.Card.BuildChart.Common.Eval (type (>>))
import SlamData.Workspace.Card.BuildChart.Common.Eval as BCE
import SlamData.Workspace.Card.BuildChart.Sankey.Model (Model, SankeyR)
import SlamData.Workspace.Card.CardType.ChartType (ChartType(Sankey))
import SlamData.Workspace.Card.BuildChart.Aggregation as Ag
import SlamData.Workspace.Card.BuildChart.Axis as Ax
import SlamData.Workspace.Card.BuildChart.ColorScheme (colors)
import SlamData.Workspace.Card.BuildChart.Semantics as Sem
import SlamData.Workspace.Card.Eval.Monad as CEM
import SlamData.Workspace.Card.Port as Port

eval
  ∷ Model
  → FilePath
  → CEM.CardEval Port.Port
eval Nothing _ =
  CEM.throw "Please select axis to aggregate"
eval (Just conf) resource = do
  records ← BCE.records resource
  let axes = Ax.buildAxes (A.take 100 records)
  pure $ Port.ChartInstructions axes (buildSankey conf records) Sankey

----------------------------------------------------------------------
-- SANKEY BUILDER
----------------------------------------------------------------------

type SankeyItem =
  { source ∷ String
  , target ∷ String
  , weight ∷ Number
  }

type SankeyData = Array SankeyItem

buildSankey ∷ SankeyR → JArray → DSL OptionI
buildSankey r records = do
  E.tooltip do
    E.triggerItem
    E.textStyle do
      E.fontFamily "Ubuntu, sans"
      E.fontSize 12

  E.colors colors

  E.series $ E.sankey do
    E.buildItems items
    E.buildLinks links

    E.lineStyle $ E.normal $ E.curveness 0.3
  where
  sankeyData ∷ SankeyData
  sankeyData = buildSankeyData records r

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


buildSankeyData ∷ JArray → SankeyR → SankeyData
buildSankeyData records r = items
  where
  --| source × target >> values
  dataMap ∷ (String × String) >> Array Number
  dataMap =
    foldl dataMapFoldFn M.empty records

  dataMapFoldFn
    ∷ M.Map (String × String) (Array Number)
    → Json
    → M.Map (String × String) (Array Number)
  dataMapFoldFn acc js =
    let
      getValuesFromJson = Sem.getValues js
      getMaybeStringFromJson = Sem.getMaybeString js

      mbSource =
        getMaybeStringFromJson r.source
      mbTarget =
        getMaybeStringFromJson r.target
      values =
        getValuesFromJson $ pure r.value

      alterFn ∷ Maybe (Array Number) → Maybe (Array Number)
      alterFn Nothing = Just values
      alterFn (Just arr) = Just $ arr ⊕ values
    in
      case mbSource × mbTarget of
        Just source × Just target →
          M.alter alterFn (source × target) acc
        _ → acc

  items ∷ SankeyData
  items =
    foldMap mkItem $ M.toList dataMap

  mkItem ∷ (String × String) × Array Number → Array SankeyItem
  mkItem ((source × target) × values) =
    [ { source
      , target
      , weight: Ag.runAggregation r.valueAggregation values
      } ]
