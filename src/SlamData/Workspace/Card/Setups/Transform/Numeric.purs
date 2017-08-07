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

module SlamData.Workspace.Card.Setups.Transform.Numeric where

import SlamData.Prelude
import Data.Argonaut as J
import Data.Argonaut ((:=), (~>), (.?))
import Data.Lens (Prism', prism')
import Data.List as L
import SqlSquared as Sql
import Test.StrongCheck.Arbitrary (class Arbitrary, arbitrary)
import Test.StrongCheck.Gen as Gen

newtype Place = Place Int

data NumericOperation
  = Floor Place
  | Round Place
  | Ceil Place

numericOperations ∷ Array NumericOperation
numericOperations =
  [ Floor (Place 0)
  , Round (Place 0)
  , Ceil (Place 0)
  ]

applyNumericOperation ∷ NumericOperation → Sql.Projection Sql.Sql → Sql.Projection Sql.Sql
applyNumericOperation no (Sql.Projection { expr, alias }) =
  Sql.Projection
    { alias, expr: transformNumericSql no expr }

transformNumericSql ∷ NumericOperation → Sql.Sql → Sql.Sql
transformNumericSql = case _ of
  Floor (Place n) → \sql → Sql.invokeFunction "FLOOR" $ L.fromFoldable [ sql, Sql.int $ negate n ]
  Round (Place n) → \sql → Sql.invokeFunction "ROUND" $ L.fromFoldable [ sql, Sql.int $ negate n ]
  Ceil (Place n) → \sql → Sql.invokeFunction "CEIL" $ L.fromFoldable [ sql, Sql.int $ negate n ]

prettyPrintNumericOperation ∷ NumericOperation → String
prettyPrintNumericOperation = case _ of
  Floor a → "Floor"
  Round a → "Round"
  Ceil a → "Ceil"

prettyPrintNumericOperation' ∷ (Place → String) → NumericOperation → String
prettyPrintNumericOperation' print = case _ of
  Floor a → "Floor " <> print a
  Round a → "Round " <> print a
  Ceil a → "Ceil " <> print a

_Floor ∷ Prism' NumericOperation Place
_Floor = prism' Floor case _ of
  Floor p → Just p
  _ → Nothing

_Round ∷ Prism' NumericOperation Place
_Round = prism' Round case _ of
  Round p → Just p
  _ → Nothing

_Ceil ∷ Prism' NumericOperation Place
_Ceil = prism' Ceil case _ of
  Ceil p → Just p
  _ → Nothing

derive instance newtypePlace ∷ Newtype Place _
derive newtype instance eqPlace ∷ Eq Place
derive newtype instance ordPlace ∷ Ord Place
derive newtype instance encodePlace ∷ J.EncodeJson Place
derive newtype instance decodePlace ∷ J.DecodeJson Place
derive newtype instance arbitraryPlace ∷ Arbitrary Place

derive instance eqNumericOperation ∷ Eq NumericOperation
derive instance ordNumericOperation ∷ Ord NumericOperation

instance encodeNumericOperation ∷ J.EncodeJson NumericOperation where
  encodeJson = case _ of
    Floor a → "type" := "floor" ~> "value" := a ~> J.jsonEmptyObject
    Round a → "type" := "round" ~> "value" := a ~> J.jsonEmptyObject
    Ceil a → "type" := "ceil" ~> "value" := a ~> J.jsonEmptyObject

instance decodeNumericOperation ∷ J.DecodeJson NumericOperation where
  decodeJson json = do
    obj ← J.decodeJson json
    val ← obj .? "value"
    obj .? "type" >>= case _ of
      "floor" → pure $ Floor val
      "round" → pure $ Round val
      "ceil" → pure $ Ceil val
      ty → throwError $ "Invalid numeric operation: " <> ty

instance arbitraryNumericOperation ∷ Arbitrary NumericOperation where
  arbitrary = Gen.chooseInt 1 3 >>= case _ of
    1 → Floor <$> arbitrary
    2 → Round <$> arbitrary
    _ → Ceil <$> arbitrary
