{-
Copyright 2017 SlamData, Inc.

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

module SlamData.Workspace.Card.Setups.Transform where

import SlamData.Prelude

import Data.Argonaut (class EncodeJson, class DecodeJson, decodeJson, (~>), (:=), (.?), jsonEmptyObject)
import Data.Array as Array
import Data.List as L
import Data.Lens (Prism', prism')

import SlamData.Workspace.Card.Setups.Axis as Ax
import SlamData.Workspace.Card.Setups.Transform.Aggregation as Ag
import SlamData.Workspace.Card.Setups.Transform.DatePart as DP
import SlamData.Workspace.Card.Setups.Transform.Numeric as N
import SlamData.Workspace.Card.Setups.Transform.String as S

import SqlSquared (Sql)
import SqlSquared as Sql

import Test.StrongCheck.Arbitrary (class Arbitrary, arbitrary)
import Test.StrongCheck.Gen as Gen

data Transform
  = DatePart DP.DatePart
  | TimePart DP.TimePart
  | Aggregation Ag.Aggregation
  | String S.StringOperation
  | Numeric N.NumericOperation
  | Count

_Aggregation ∷ Prism' Transform Ag.Aggregation
_Aggregation = prism' Aggregation case _ of
  Aggregation a → Just a
  _ → Nothing

_Numeric ∷ Prism' Transform N.NumericOperation
_Numeric = prism' Numeric case _ of
  Numeric a → Just a
  _ → Nothing

foldTransform
  ∷ ∀ r
  . (DP.DatePart → r)
  → (DP.TimePart → r)
  → (Ag.Aggregation → r)
  → (S.StringOperation → r)
  → (N.NumericOperation → r)
  → (Unit → r)
  → Transform
  → r
foldTransform a b c d e f = case _ of
  DatePart z → a z
  TimePart z → b z
  Aggregation z → c z
  String z → d z
  Numeric z → e z
  Count → f unit

prettyPrintTransform ∷ Transform → String
prettyPrintTransform =
  foldTransform
    DP.prettyPrintDate
    DP.prettyPrintTime
    Ag.printAggregation
    S.prettyPrintStringOperation
    N.prettyPrintNumericOperation
    \_ → "Count"

applyTransform ∷ Transform → Sql.Projection Sql → Sql.Projection Sql
applyTransform =
  foldTransform
    DP.applyDateTransform
    DP.applyTimeTransform
    aggregation
    S.applyStringOperation
    N.applyNumericOperation
    (const count)
  where
  count (Sql.Projection {alias, expr}) =
    Sql.Projection { alias, expr: Sql.invokeFunction "COUNT" $ L.singleton expr }
  aggregation ag (Sql.Projection {alias, expr}) =
    let
      funcName = case ag of
        Ag.Minimum → "MIN"
        Ag.Maximum → "MAX"
        Ag.Average → "AVG"
        Ag.Sum → "SUM"
    in Sql.Projection { alias, expr: Sql.invokeFunction funcName $ L.singleton expr }


transformSql ∷ Transform → Sql.Sql → Sql.Sql
transformSql = foldTransform
  DP.transformDateSql
  DP.transformTimeSql
  (\_ p → p)
  S.transformStringSql
  N.transformNumericSql
  (\_ s → Sql.invokeFunction "COUNT" $ pure s)


dateTransforms ∷ Array Transform
dateTransforms = DatePart <$> DP.dateParts

timeTransforms ∷ Array Transform
timeTransforms = TimePart <$> DP.timeParts

dateTimeTransforms ∷ Array Transform
dateTimeTransforms = dateTransforms <> timeTransforms

aggregationTransforms ∷ Array Transform
aggregationTransforms = Aggregation <$> Ag.allAggregations

stringTransforms ∷ Array Transform
stringTransforms = String <$> S.stringOperations

numericTransforms ∷ Maybe Transform → Array Transform
numericTransforms = case _ of
  Just (Numeric p) →
    N.numericOperations <#> case p, _ of
      N.Floor _, N.Floor _ → Numeric p
      N.Round _, N.Round _ → Numeric p
      N.Ceil _, N.Ceil _ → Numeric p
      _, a → Numeric a
  _ → Numeric <$> N.numericOperations

axisTransforms ∷ Ax.AxisType → Maybe Transform → Array Transform
axisTransforms axis prev = Array.cons Count case axis of
  Ax.Measure → aggregationTransforms <> numericTransforms prev
  Ax.Category → stringTransforms
  Ax.Date → dateTransforms
  Ax.Time → timeTransforms
  Ax.DateTime → dateTimeTransforms

derive instance eqTransform ∷ Eq Transform
derive instance ordTransform ∷ Ord Transform

instance encodeJsonTransform ∷ EncodeJson Transform where
  encodeJson = case _ of
    DatePart value → "type" := "date" ~> "value" := value ~> jsonEmptyObject
    TimePart value → "type" := "time" ~> "value" := value ~> jsonEmptyObject
    Aggregation value → "type" := "aggregation" ~> "value" := value ~> jsonEmptyObject
    String value → "type" := "string" ~> "value" := value ~> jsonEmptyObject
    Numeric value → "type" := "numeric" ~> "value" := value ~> jsonEmptyObject
    Count → "type" := "count" ~> jsonEmptyObject

instance decodeJsonTransform ∷ DecodeJson Transform where
  decodeJson json = do
    obj ← decodeJson json
    obj .? "type" >>= case _ of
      "date" → DatePart <$> obj .? "value"
      "time" → TimePart <$> obj .? "value"
      "aggregation" → Aggregation <$> obj .? "value"
      "string" → String <$> obj .? "value"
      "numeric" → Numeric <$> obj .? "value"
      "count" → pure Count
      ty → throwError $ "Invalid transformation type: " <> ty

instance arbitraryTransform ∷ Arbitrary Transform where
  arbitrary = Gen.chooseInt 1 6 >>= case _ of
    1 → DatePart <$> arbitrary
    2 → TimePart <$> arbitrary
    3 → Aggregation <$> arbitrary
    4 → String <$> arbitrary
    5 → Numeric <$> arbitrary
    _ → pure Count
