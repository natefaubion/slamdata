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

module SlamData.Workspace.Card.Setups.PivotTable.Model where

import SlamData.Prelude

import Control.Monad.Gen as Gen
import Data.Argonaut (JCursor, Json, class EncodeJson, class DecodeJson, decodeJson, (~>), (:=), isNull, (.?), jsonEmptyObject)
import Data.Array as Array
import Data.Bitraversable (ltraverse)
import Data.Codec.Argonaut as CA
import Data.Foldable as F
import Data.Newtype (un)
import SlamData.Workspace.Card.Setups.Dimension as D
import SlamData.Workspace.Card.Setups.DisplayOptions.Model as Display
import SlamData.Workspace.Card.Setups.Transform as T
import Test.StrongCheck.Arbitrary (class Arbitrary, arbitrary)
import Test.StrongCheck.Data.Argonaut (ArbJCursor(..))
import Test.StrongCheck.Gen as SCG
import Utils (decodec)

type Model =
  { dimensions ∷ Array GroupByDimension
  , columns ∷ Array ColumnDimension
  }

data Column = All | Column JCursor

type ColumnDimension = Display.DisplayOptions × D.Dimension Void Column

type GroupByDimension = D.Dimension Void JCursor

initialModel ∷ Model
initialModel =
  { dimensions: []
  , columns: []
  }

eqModel ∷ Model → Model → Boolean
eqModel r1 r2 = r1.dimensions == r2.dimensions && r1.columns == r2.columns

genModel ∷ SCG.Gen Model
genModel = do
  displayOptions ← Gen.unfoldable Display.genDisplayOptions
  dimensions ← map (map (un ArbJCursor) ∘ un D.DimensionWithStaticCategory) <$> arbitrary
  columns ← map (un D.DimensionWithStaticCategory) <$> arbitrary
  pure { dimensions, columns: Array.zip displayOptions columns }

encode ∷ Model → Json
encode r =
  "configType" := "pivot"
  ~> "dimensions" := r.dimensions
  ~> "columns" := (lmap (CA.encode Display.codecDisplayOptions) <$> r.columns)
  ~> jsonEmptyObject

decode ∷ Json → Either String Model
decode js
  | isNull js = pure initialModel
  | otherwise = do
      obj ← decodeJson js
      configType ← obj .? "configType"
      unless (configType ≡ "pivot") do
        throwError "Not a valid pivot table configuration"
      dimensions ← traverse decodeDimension =<< obj .? "dimensions"
      columns ← traverse decodeColumn =<< obj .? "columns"
      pure { dimensions, columns }
  where
  decodeDimension ∷ Json → Either String GroupByDimension
  decodeDimension json = decodeJson json <|> decodeLegacyDimension json

  decodeLegacyDimension ∷ Json → Either String GroupByDimension
  decodeLegacyDimension json = do
    map D.defaultJCursorDimension $ decodeJson json

  decodeColumn ∷ Json → Either String ColumnDimension
  decodeColumn json
    = decodeCurrentColumn json
    <|> map (Tuple Display.initialDisplayOptions)
          (decodeLegacyColumn json <|> decodeAncientLegacyColumn json)

  decodeCurrentColumn ∷ Json → Either String ColumnDimension
  decodeCurrentColumn =
    ltraverse (decodec Display.codecDisplayOptions) <=< decodeJson

  decodeLegacyColumn ∷ Json → Either String (D.Dimension Void Column)
  decodeLegacyColumn = decodeJson

  decodeAncientLegacyColumn ∷ Json → Either String (D.Dimension Void Column)
  decodeAncientLegacyColumn json = do
    obj ← decodeJson json
    obj .? "columnType" >>= case _ of
      "value" → do
        value ← Column <$> obj .? "value"
        valueAggregation ← map T.Aggregation <$> obj .? "valueAggregation"
        pure $ D.Dimension
          (Just (defaultColumnCategory value))
          (D.Projection valueAggregation value)
      "count" → do
        pure $ D.Dimension
          (Just (D.Static "count"))
          (D.Projection (Just T.Count) All)
      ty → throwError $ "Invalid column type: " <> ty

derive instance eqColumn ∷ Eq Column
derive instance ordColumn ∷ Ord Column

instance arbitraryColumn ∷ Arbitrary Column where
  arbitrary = arbitrary >>= if _
    then pure All
    else Column ∘ un ArbJCursor <$> arbitrary

instance encodeJsonColumn ∷ EncodeJson Column where
  encodeJson = case _ of
    All → "type" := "all" ~> jsonEmptyObject
    Column j → "type" := "column" ~> "value" := j ~> jsonEmptyObject

instance decodeJsonColumn ∷ DecodeJson Column where
  decodeJson json = do
    obj ← decodeJson json
    obj .? "type" >>= case _ of
      "all" → pure All
      "column" → Column <$> obj .? "value"
      ty → throwError $ "Invalid column type: " <> ty

isSimple ∷ Model → Boolean
isSimple { dimensions, columns } =
  Array.null dimensions && F.all simpleCol columns
  where
  simpleCol = case _ of
    _ × D.Dimension _ (D.Projection (Just (T.Aggregation _)) _) → false
    _ → true

defaultColumnCategory ∷ ∀ a. Column → D.Category a
defaultColumnCategory = case _ of
  All → D.Static "all"
  Column j → D.defaultJCursorCategory j