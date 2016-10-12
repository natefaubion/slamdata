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

module SlamData.Workspace.Card.BuildChart.Graph.Eval
  ( eval
  , module SlamData.Workspace.Card.BuildChart.Graph.Model
  ) where

import SlamData.Prelude

import Data.Argonaut (JArray, Json)
import Data.Array as A
import Data.Foldable as F
import Data.Foreign as FR
import Data.Foreign.Class (readProp)
import Data.Int as Int
import Data.Lens ((^?))
import Data.Map as M
import Data.String as Str
import Data.String.Regex as Rgx

import ECharts.Monad (DSL)
import ECharts.Commands as E
import ECharts.Types.Phantom (OptionI)
import ECharts.Types as ET
import ECharts.Types.Phantom as ETP

import Global (infinity)

import Quasar.Types (FilePath)

import SlamData.Quasar.Class (class QuasarDSL)
import SlamData.Quasar.Error as QE
import SlamData.Workspace.Card.BuildChart.Common.Eval (type (>>))
import SlamData.Workspace.Card.BuildChart.Common.Eval as BCE
import SlamData.Workspace.Card.BuildChart.Graph.Model (Model, GraphR)
import SlamData.Workspace.Card.CardType.ChartType (ChartType(Graph))
import SlamData.Workspace.Card.BuildChart.Axis as Ax
import SlamData.Workspace.Card.BuildChart.Aggregation as Ag
import SlamData.Workspace.Card.BuildChart.ColorScheme (colors)
import SlamData.Workspace.Card.BuildChart.Semantics as Sem
import SlamData.Workspace.Card.Eval.CardEvalT as CET
import SlamData.Workspace.Card.Port as Port


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
  pure $ Port.ChartInstructions axes (buildGraph conf records) Graph


type EdgeItem =
  { source ∷ String
  , target ∷  String
  }

type GraphItem =
  { size ∷ Maybe Number
  , category ∷ Maybe String
  , value ∷ Maybe Number
  , source ∷ Maybe String
  , target ∷ Maybe String
  , name ∷ Maybe String
  }

type GraphData = Array GraphItem × Array EdgeItem

buildGraphData ∷ JArray → GraphR → GraphData
buildGraphData records r =
  nodes × edges
  where
  -- | maybe color >> maybe source × maybe target >> values
  dataMap ∷ Maybe String >> Maybe String × Maybe String >> Array Number
  dataMap =
    foldl dataMapFoldFn M.empty records

  dataMapFoldFn
    ∷ Maybe String >> Maybe String × Maybe String >> Array Number
    → Json
    → Maybe String >> Maybe String × Maybe String >> Array Number
  dataMapFoldFn acc js =
    let
      getMaybeStringFromJson = Sem.getMaybeString js
      getValuesFromJson = Sem.getValues js
      mbSource =
        getMaybeStringFromJson r.source
      mbTarget =
        getMaybeStringFromJson r.target
      mbColor =
        getMaybeStringFromJson =<< r.color
      values =
        getValuesFromJson r.size

      colorAlterFn
        ∷ Maybe (Maybe String × Maybe String >> Array Number)
        → Maybe (Maybe String × Maybe String >> Array Number)
      colorAlterFn Nothing =
        Just $ M.singleton (mbSource × mbTarget) values
      colorAlterFn (Just color) =
        Just $ M.alter alterSourceTargetFn (mbSource × mbTarget) color

      alterSourceTargetFn
        ∷ Maybe (Array Number)
        → Maybe (Array Number)
      alterSourceTargetFn Nothing = Just values
      alterSourceTargetFn (Just arr) = Just $ arr ⊕ values
    in
     M.alter colorAlterFn mbColor acc

  rawNodes ∷ Array GraphItem
  rawNodes =
    foldMap mkNodes $ M.toList dataMap

  mkNodes
    ∷ Maybe String × ((Maybe String × Maybe String) >> Array Number)
    → Array GraphItem
  mkNodes (color × stMap) =
    foldMap (mkNode color) $ M.toList stMap

  mkNode
    ∷ Maybe String
    → (Maybe String × Maybe String) × Array Number
    → Array GraphItem
  mkNode category ((source × target) × values) =
    [ { size: Nothing
      , source
      , target
      , value: map (\ag → Ag.runAggregation ag values) r.sizeAggregation
      , category
      , name: mkName source target category
      } ]

  mkName ∷ Maybe String → Maybe String → Maybe String → Maybe String
  mkName source target category =
    (map (\s → "source:" ⊕ s) source)
    ⊕ (pure $ foldMap (\t → ":target:" ⊕ t) target)
    ⊕ (pure $ foldMap (\c → ":category:" ⊕ c) category)

  edges ∷ Array EdgeItem
  edges =
    foldMap mkEdges $ M.toList dataMap

  mkEdges
    ∷ Maybe String × ((Maybe String × Maybe String) >> Array Number)
    → Array EdgeItem
  mkEdges (color × stMap) =
    foldMap (mkEdge color) $ M.keys stMap

  mkEdge
    ∷ Maybe String
    → Maybe String × Maybe String
    → Array EdgeItem
  mkEdge category (mbSource × mbTarget) =
    foldMap A.singleton do
      source ← mkName mbSource mbTarget category
      target ← mkName mbTarget mbSource category
      pure { source, target }

  minimumValue ∷ Number
  minimumValue =
    fromMaybe (-1.0 * infinity) $ F.minimum $ A.catMaybes $ map _.value rawNodes

  maximumValue ∷ Number
  maximumValue =
    fromMaybe infinity $ F.maximum $ A.catMaybes $ map _.value rawNodes

  distance ∷ Number
  distance = maximumValue - minimumValue

  sizeDistance ∷ Number
  sizeDistance = r.maxSize - r.minSize

  relativeSize ∷ Number → Number
  relativeSize val
    | distance ≡ zero = val
    | val < 0.0 = 0.0
    | otherwise =
      r.maxSize - sizeDistance / distance * (maximumValue - val)

  nodes ∷ Array GraphItem
  nodes = rawNodes <#> \r → r{size = map relativeSize r.value}

sourceRgx ∷ Rgx.Regex
sourceRgx = unsafePartial fromRight $ Rgx.regex "source:([^:]+)" Rgx.noFlags

categoryRgx ∷ Rgx.Regex
categoryRgx = unsafePartial fromRight $ Rgx.regex "category:([^:]+)" Rgx.noFlags

buildGraph ∷ GraphR → JArray → DSL OptionI
buildGraph r records = do
  E.tooltip do
    E.triggerItem
    E.textStyle do
      E.fontFamily "Ubuntu, sans"
      E.fontSize 12
    E.formatterItem \{name, value, "data": item, dataType} →
      let
        fItem ∷ FR.Foreign
        fItem = FR.toForeign item

        mbSource ∷ Maybe String
        mbSource = join $ Rgx.match sourceRgx name >>= flip A.index 1

        mbCat ∷ Maybe String
        mbCat = join $ Rgx.match categoryRgx name >>= flip A.index 1

        mbVal ∷ Maybe Number
        mbVal = if FR.isUndefined $ FR.toForeign value then Nothing else Just value

        itemTooltip ∷ String
        itemTooltip =
          (foldMap (\s → "name: " ⊕ s) mbSource)
          ⊕ (foldMap (\c → "<br /> category: " ⊕ c) mbCat)
          ⊕ (foldMap (\v → "<br /> value: " ⊕ show v) mbVal)
          ⊕ (foldMap (\a → "<br /> value aggregation: "
                           ⊕ (Str.toLower $ Ag.printAggregation a))
             r.sizeAggregation)
      in fromMaybe itemTooltip do
        guard $ dataType ≡ "edge"
        source ← either (const Nothing) Just $ FR.readString =<< readProp "source" fItem
        target ← either (const Nothing) Just $ FR.readString =<< readProp "target" fItem
        sourceName ← Str.stripPrefix "edge " source
        targetName ← Str.stripPrefix "edge " target
        pure $ sourceName ⊕ " > " ⊕ targetName

  E.legend do
    E.orient ET.Vertical
    E.leftLeft
    E.topTop
    E.textStyle $ E.fontFamily "Ubuntu, sans"
    E.items $ map ET.strItem legendNames

  E.colors colors

  E.series $ E.graph do
    if r.circular
      then E.layoutCircular
      else E.layoutForce

    E.force do
      E.edgeLength 120.0
      E.layoutAnimation true

    E.buildItems items
    E.links $ snd graphData

    E.buildCategories $ for_ legendNames $ E.addCategory ∘ E.name
    E.lineStyle $ E.normal $ E.colorSource

  where
  graphData ∷ GraphData
  graphData = buildGraphData records r

  legendNames ∷ Array String
  legendNames = A.nub $ A.catMaybes $ map _.category $ fst graphData

  items ∷ DSL ETP.ItemsI
  items = for_ (fst graphData) \item → E.addItem do
    for_ (item.category >>= flip A.elemIndex legendNames) E.category
    traverse_ E.symbolSize $ map Int.floor item.size
    E.value $ fromMaybe zero item.value
    traverse_ E.name item.name
    E.itemStyle $ E.normal do
      E.borderWidth 1
    E.label do
      E.normal E.hidden
      E.emphasis E.hidden
