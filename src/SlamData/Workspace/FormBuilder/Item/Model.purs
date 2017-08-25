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

module SlamData.Workspace.FormBuilder.Item.Model
  ( Model
  , FieldName(..)
  , initialModel
  , genModel
  , encode
  , decode
  , sanitiseValueFromForm
  , sanitiseValueForForm
  , EqModel(..)
  , runEqModel
  , defaultValueToVarMapValue
  , urlVarMapValueToVarMapValue
  , module SlamData.Workspace.FormBuilder.Item.FieldType
  ) where

import SlamData.Prelude

import Data.Argonaut ((~>), (:=), (.?))
import Data.Argonaut as J
import Data.Foldable as F
import Data.String as Str
import SlamData.Quasar.EJsonMeta as EJM
import SlamData.SqlSquared.Tagged as SqlT
import SlamData.Workspace.Card.Port.VarMap (URLVarMapValue, unURLVarMapValue)
import SlamData.Workspace.Card.Port.VarMap as Port
import SlamData.Workspace.Card.Variables.Error.TypeMismatchError (TypeMismatchError(..))
import SlamData.Workspace.Card.Port.VarMap as VM
import SlamData.Workspace.FormBuilder.Item.FieldType (FieldType(..), _FieldTypeDisplayName, allFieldTypes, fieldTypeToInputType)
import SqlSquared as Sql
import Test.StrongCheck.Arbitrary as SC
import Test.StrongCheck.Gen as Gen
import Text.Parsing.Parser as P

newtype FieldName = FieldName String

derive newtype instance eqFieldName :: Eq FieldName
derive newtype instance ordFieldName :: Ord FieldName
derive instance newtypeFieldName :: Newtype FieldName _

instance showFieldName ∷ Show FieldName where
  show (FieldName name) = "(FieldName " <> show name <> ")"

type Model =
  { name ∷ FieldName
  , fieldType ∷ FieldType
  , defaultValue ∷ Maybe String
  }

genModel ∷ Gen.Gen Model
genModel = do
  name ← FieldName <$> SC.arbitrary
  fieldType ← SC.arbitrary
  defaultValue ← SC.arbitrary
  pure { name, fieldType, defaultValue }

newtype EqModel = EqModel Model

runEqModel
  ∷ EqModel
  → Model
runEqModel (EqModel m) =
  m

derive instance eqEqModel ∷ Eq EqModel

initialModel ∷ Model
initialModel =
  { name: FieldName ""
  , fieldType: StringFieldType
  , defaultValue: Nothing
  }

encode
  ∷ Model
  → J.Json
encode st =
  "name" := unwrap st.name
  ~> "fieldType" := st.fieldType
  ~> "defaultValue" := st.defaultValue
  ~> J.jsonEmptyObject

decode
  ∷ J.Json
  → Either String Model
decode =
  J.decodeJson >=> \obj → do
    name ← FieldName <$> obj .? "name"
    fieldType ← obj .? "fieldType"
    defaultValue ← obj .? "defaultValue"
    pure { name, fieldType, defaultValue }

-- | This takes the HTML-produced form values and tweaks the date/time values
-- | to match the acceptable `YYYY-MM-DDTHH:mm:ssZ` / `YYYY-MM-DD` / `HH:mm:ss`
-- | forms.
sanitiseValueFromForm ∷ FieldType → String → String
sanitiseValueFromForm ty s = case ty of
  DateTimeFieldType
    | Str.charAt 10 s ≡ Just ' ' →
        sanitiseValueFromForm ty (Str.take 10 s <> "T" <> Str.drop 11 s)
    | Str.length s ≡ 19 → s <> "Z"
  DateFieldType → Str.take 10 s
  TimeFieldType
    | Str.length s == 5 → s <> ":00"
    | otherwise → Str.take 8 s
  _ → s

-- | This takes values produced by `sanitiseValueFromForm` and formats them back
-- | into values acceptable for populating HTML forms.
sanitiseValueForForm ∷ FieldType → String → String
sanitiseValueForForm ty s = case ty of
  DateTimeFieldType → Str.take 19 s
  DateFieldType → Str.take 10 s
  TimeFieldType → Str.take 8 s
  _ → s

defaultValueToVarMapValue
  ∷ FieldType
  → String
  → Either (Either SqlT.ParseError TypeMismatchError) Port.VarMapValue
defaultValueToVarMapValue ty str =
  map VM.Expr case ty of
    StringFieldType →
      pure $ Sql.string str
    DateTimeFieldType →
      lmap Left $ SqlT.datetimeSql str
    DateFieldType →
      lmap Left $ SqlT.dateSql str
    TimeFieldType →
      lmap Left $ SqlT.timeSql str
    IntervalFieldType →
      lmap Left $ SqlT.intervalSql str
    ObjectIdFieldType →
      lmap Left $ SqlT.oidSql str
    SqlExprFieldType →
      parseSql' str
    SqlIdentifierFieldType →
      pure $ Sql.ident str
    fieldType → do
      value ← parseSql' str
      unless (EJM.sqlToEJsonMeta value `F.elem` EJM.ejsonTypes fieldType) $
        throwError $ Right $ TypeMismatchError { sql: str, expected: EJM.ejsonTypes fieldType,  actual: EJM.sqlToEJsonMeta value }
      pure value
  where
  parseSql' ∷ String → Either (Either SqlT.ParseError TypeMismatchError) Sql.Sql
  parseSql' = lmap Left ∘ parseSql

urlVarMapValueToVarMapValue
  ∷ FieldType
  → URLVarMapValue
  → Either (Either SqlT.ParseError TypeMismatchError) Port.VarMapValue
urlVarMapValueToVarMapValue ty v =
  map Port.Expr do
    let str = unURLVarMapValue v
    value ← lmap Left $ parseSql str
    traverse_ (throwError ∘ Right) (validateType str value ty)
    pure value

parseSql :: String -> Either SqlT.ParseError Sql.Sql
parseSql s = lmap (SqlT.ParseError ∘ P.parseErrorMessage) $ Sql.parse s

validateType :: String → Sql.Sql → FieldType → Maybe TypeMismatchError
validateType s val ft =
  let
    expected = EJM.ejsonTypes ft
    actual = EJM.sqlToEJsonMeta val
  in
    if F.elem actual expected
      then
        Nothing
      else
        Just $ TypeMismatchError { sql: s, actual, expected }
