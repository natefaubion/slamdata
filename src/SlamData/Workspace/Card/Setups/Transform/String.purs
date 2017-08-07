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

module SlamData.Workspace.Card.Setups.Transform.String where

import SlamData.Prelude
import SqlSquared as Sql
import Data.Argonaut as J
import Test.StrongCheck.Arbitrary (class Arbitrary)
import Test.StrongCheck.Gen as Gen

data StringOperation
  = Lower
  | Upper
  | Length

transformStringSql ∷ StringOperation → Sql.Sql → Sql.Sql
transformStringSql op expr =
  Sql.invokeFunction (printStringOperation op) $ pure expr

applyStringOperation ∷ StringOperation → Sql.Projection Sql.Sql → Sql.Projection Sql.Sql
applyStringOperation op (Sql.Projection { expr, alias }) =
  Sql.Projection
  { alias
  , expr: transformStringSql op expr
  }


stringOperations ∷ Array StringOperation
stringOperations =
  [ Lower
  , Upper
  , Length
  ]

printStringOperation ∷ StringOperation → String
printStringOperation = case _ of
  Lower → "LOWER"
  Upper → "UPPER"
  Length → "LENGTH"

prettyPrintStringOperation ∷ StringOperation → String
prettyPrintStringOperation = case _ of
  Lower → "Lower"
  Upper → "Upper"
  Length → "Length"

parseFromString ∷ String → Either String StringOperation
parseFromString = case _ of
  "LOWER" → pure Lower
  "UPPER" → pure Upper
  "LENGTH" → pure Length
  str → throwError $ "Invalid string operation: " <> str

derive instance eqStringOperation ∷ Eq StringOperation
derive instance ordStringOperation ∷ Ord StringOperation

instance encodeStringOperation ∷ J.EncodeJson StringOperation where
  encodeJson = J.encodeJson ∘ printStringOperation

instance decodeStringOperation ∷ J.DecodeJson StringOperation where
  decodeJson = J.decodeJson >=> parseFromString

instance arbitraryStringOperation ∷ Arbitrary StringOperation where
  arbitrary = Gen.chooseInt 1 3 <#> case _ of
    1 → Upper
    2 → Lower
    _ → Length
