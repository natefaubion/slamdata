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

module SlamData.Workspace.Card.BuildChart.PivotTable.Model where

import SlamData.Prelude

import Data.Argonaut (JCursor, Json, class EncodeJson, class DecodeJson, decodeJson, (~>), (:=), isNull, (.?), jsonEmptyObject)
import Data.Array as Array
import Data.Foldable as F
import Data.Lens (PrismP, prism')

import SlamData.Workspace.Card.BuildChart.Aggregation as Ag

import Test.StrongCheck.Arbitrary (class Arbitrary, arbitrary)
import Test.StrongCheck.Gen as Gen
import Test.Property.ArbJson (runArbJCursor)

type Model =
  { dimensions ∷ Array JCursor
  , columns ∷ Array Column
  }

data Column
  = Count
  | Column
      { value ∷ JCursor
      , valueAggregation ∷ Maybe (Ag.Aggregation)
      }

_Column
  ∷ PrismP Column
      { value ∷ JCursor
      , valueAggregation ∷ Maybe (Ag.Aggregation)
      }
_Column = prism' Column case _ of
  Column r → Just r
  _ → Nothing

initialModel ∷ Model
initialModel =
  { dimensions: []
  , columns: []
  }

eqModel ∷ Model → Model → Boolean
eqModel r1 r2 = r1.dimensions == r2.dimensions && r1.columns == r2.columns

genModel ∷ Gen.Gen Model
genModel = do
  dimensions ← map runArbJCursor <$> arbitrary
  columns ← arbitrary
  pure { dimensions, columns }

encode ∷ Model → Json
encode r =
  "configType" := "pivot"
  ~> "dimensions" := r.dimensions
  ~> "columns" := r.columns
  ~> jsonEmptyObject

decode ∷ Json → Either String Model
decode js
  | isNull js = pure initialModel
  | otherwise = do
      obj ← decodeJson js
      configType ← obj .? "configType"
      unless (configType ≡ "pivot") do
        throwError "Not a valid pivot table configuration"
      dimensions ← obj .? "dimensions"
      columns ← obj .? "columns"
      pure { dimensions, columns }

derive instance eqColumn ∷ Eq Column
derive instance ordColumn ∷ Ord Column

instance arbitraryColumn ∷ Arbitrary Column where
  arbitrary = do
    value ← runArbJCursor <$> arbitrary
    valueAggregation ← arbitrary
    pure $ Column { value, valueAggregation }

instance encodeJsonColumn ∷ EncodeJson Column where
  encodeJson (Column c) =
    "columnType" := "value"
    ~> "value" := c.value
    ~> "valueAggregation" := c.valueAggregation
    ~> jsonEmptyObject
  encodeJson Count =
    "columnType" := "count"
    ~> jsonEmptyObject

instance decodeJsonColumn ∷ DecodeJson Column where
  decodeJson json = do
    obj ← decodeJson json
    columnType ← obj .? "columnType"
    case columnType of
      "value" → do
        value ← obj .? "value"
        valueAggregation ← obj .? "valueAggregation"
        pure $ Column { value, valueAggregation }
      "count" → do
        pure Count
      _ →
        Left $ "Not a valid column type: " <> columnType

isSimple ∷ Model → Boolean
isSimple { dimensions, columns } =
  Array.null dimensions && F.all simpleCol columns
  where
  simpleCol (Column { valueAggregation: Nothing }) = true
  simpleCol _ = false
