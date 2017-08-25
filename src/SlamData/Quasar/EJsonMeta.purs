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

module SlamData.Quasar.EJsonMeta where

import SlamData.Prelude

import Data.Json.Extended.Type (EJsonType)
import Data.Json.Extended.Type as EJT
import Data.Json.Extended.Signature as EJS
import Data.Lens.Fold (preview)
import Data.List.NonEmpty as NEL
import Data.List.Safe ((:))
import Data.List.Safe as SL
import Data.String as S
import SlamData.Workspace.FormBuilder.Item.FieldType (FieldType)
import SlamData.Workspace.FormBuilder.Item.FieldType as FB
import SqlSquared as Sql

data EJsonMeta = Literal EJsonType | FunctionName String | Ident | Expr

instance showEJsonMeta ∷ Show EJsonMeta where
  show (Literal eJsonType) =
    "(Literal " <> show eJsonType <> ")"
  show (FunctionName functionName) =
    "(FunctionName " <> show functionName <> ")"
  show Ident =
    "Ident"
  show Expr =
    "Expr"

derive instance eqEJsonMeta ∷ Eq EJsonMeta

sqlToEJsonMeta :: Sql.Sql → EJsonMeta
sqlToEJsonMeta sql =
  fromMaybe Expr $ tryLiteral <|> tryFunctionName <|> tryIdent
  where
  tryLiteral ∷ Maybe EJsonMeta
  tryLiteral =
    Literal ∘ EJS.getType <$> preview Sql._Literal sql

  tryFunctionName ∷ Maybe EJsonMeta
  tryFunctionName =
    FunctionName ∘ S.toUpper ∘ _.name <$> preview Sql._InvokeFunction sql

  tryIdent ∷ Maybe EJsonMeta
  tryIdent =
    const Ident <$> preview Sql._Ident sql

printEJsonMeta ∷ EJsonMeta → String
printEJsonMeta = case _ of
  Literal ejt → show ejt
  FunctionName "OID" → "Object ID"
  FunctionName "DATE" → "Date"
  FunctionName "TIME" → "Time"
  FunctionName "TIMESTAMP" → "Timestamp"
  FunctionName "INTERVAL" → "Interval"
  FunctionName s → s
  Ident → "SQL² Identifier"
  Expr → "SQL² Expression"

ejsonTypes ∷ FieldType → NEL.NonEmptyList EJsonMeta
ejsonTypes = SL.toNEL ∘ case _ of
  FB.StringFieldType →
    Literal EJT.String : SL.nil
  FB.BooleanFieldType →
    Literal EJT.Boolean : SL.nil
  FB.NumericFieldType →
    Literal EJT.Integer : Literal EJT.Decimal : SL.nil
  FB.ArrayFieldType →
    Literal EJT.Array : SL.nil
  FB.ObjectFieldType →
    Literal EJT.Map : SL.nil
  FB.ObjectIdFieldType →
    FunctionName "OID" : SL.nil
  FB.IntervalFieldType →
    FunctionName "INTERVAL" : SL.nil
  -- TODO: Verify that these all take TIMESTAMP
  FB.TimeFieldType →
    FunctionName "TIME" : FunctionName "TIMESTAMP" : SL.nil
  FB.DateFieldType →
    FunctionName "DATE" : FunctionName "TIMESTAMP" : SL.nil
  FB.DateTimeFieldType →
    FunctionName "TIMESTAMP" : SL.nil
  FB.SqlIdentifierFieldType →
    Ident : SL.nil
  FB.SqlExprFieldType →
    Expr : SL.nil
