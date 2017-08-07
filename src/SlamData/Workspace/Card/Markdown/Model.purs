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

module SlamData.Workspace.Card.Markdown.Model
  ( Model
  , MarkdownExpr(..)
  , encode
  , decode
  , genModel
  , emptyModel
  ) where

import SlamData.Prelude

import Data.Argonaut (Json, JObject, jsonEmptyObject, class EncodeJson, encodeJson, class DecodeJson, decodeJson, (~>), (:=), (.?))
import Data.DateTime as DT
import Data.Enum (fromEnum, toEnum)
import Data.Functor.Compose (Compose(..))
import Data.HugeNum as HN
import Data.Json.Extended as EJSON
import Data.List as L
import Data.Newtype (un)
import Data.StrMap as SM
import Data.String as S
import Data.Traversable as T
import Matryoshka (transAna, embed)
import SqlSquared as Sql
import Test.StrongCheck.Arbitrary as SC
import Test.StrongCheck.Gen as Gen
import Text.Markdown.SlamDown as SD
import Text.Markdown.SlamDown.Halogen.Component.State as SDS
import Text.Markdown.SlamDown.Syntax.Value as SDV

-- | The serialization model used for markdown cards.
type Model = SDS.SlamDownFormState MarkdownExpr

newtype MarkdownExpr = MarkdownExpr Sql.Sql

derive instance newtypeMarkdownExpr ∷ Newtype MarkdownExpr _
derive newtype instance eqMarkdownExpr ∷ Eq MarkdownExpr
derive newtype instance ordMarkdownExpr ∷ Ord MarkdownExpr

instance showMarkdownExpr ∷ Show MarkdownExpr where
  show (MarkdownExpr sql) = "(MarkdownExpr " <> Sql.print sql <> ")"

instance arbitraryMarkdownExpr ∷ SC.Arbitrary MarkdownExpr where
  arbitrary = pure $ MarkdownExpr $ Sql.set []

instance encodeMarkdownExpr ∷ EncodeJson MarkdownExpr where
  encodeJson = Sql.encodeJson ∘ un MarkdownExpr

instance decodeMarkdownExpr ∷ DecodeJson MarkdownExpr where
  decodeJson json = (map MarkdownExpr $ Sql.decodeJson json)
    <|> (decodeLegacy json)
    where
    decodeLegacy = decodeJson >=> \obj →
      map MarkdownExpr
      $ decodeLiteral obj
      <|> decodeSetLiteral obj
      <|> decodeQueryExpr obj

    decodeLiteral obj = do
      (js ∷ Json) ← obj .? "literal"
      (ejs ∷ EJSON.EJson) ←
        EJSON.decodeEJson js
      pure $ transAna Sql.Literal ejs

    decodeSetLiteral obj = do
      (arr ∷ Array Json) ← obj .? "set"
      lst ←
        map (L.fromFoldable ∘ map unwrap)
        $ traverse decodeLegacy arr
      pure $ embed $ Sql.SetLiteral lst

    decodeQueryExpr obj  = do
      queryStr ← obj .? "query"
      lmap show $ Sql.parse queryStr

instance valueMarkdownExpr ∷ SDV.Value MarkdownExpr where
  stringValue = MarkdownExpr ∘ Sql.string
  renderValue = case _ of
    MarkdownExpr sql → stripQuotes (Sql.print sql)
    where
    stripQuotes str =
      fromMaybe str $
        S.stripSuffix (S.Pattern "\"") str >>=
        S.stripPrefix (S.Pattern "\"")


genModel ∷ Gen.Gen Model
genModel =
  SM.fromFoldable <$> Gen.arrayOf (Tuple <$> SC.arbitrary <*> SC.arbitrary)

emptyModel ∷ Model
emptyModel =
  SM.empty

-- | For getting parse error messages that are suitable for diagnostics.
traceError
  ∷ ∀ a
  . String
  → Either String a
  → Either String a
traceError lbl e =
  case e of
    Right x → Right x
    Left err → Left $ lbl ⊕ " » " ⊕ err

-- | Encodes the model as a JSON value.
encode
  ∷ Model
  → Json
encode state
  = "state" := map encodeFormFieldValue state
  ~> jsonEmptyObject

-- | Attempts to decode a JSON value as the model.
decode
  ∷ Json
  → Either String Model
decode =
  decodeJson >=> \obj →
    traverse decodeFormFieldValue =<< obj .? "state"

encodeExpr
  ∷ ∀ a
  . (a → Json)
  → SD.Expr a
  → Json
encodeExpr enc expr =
  case expr of
    SD.Literal a →
      "type" := "lit"
        ~> "value" := enc a
        ~> jsonEmptyObject
    SD.Unevaluated value →
      "type" := "uneval"
        ~> "value" := value
        ~> jsonEmptyObject

decodeExpr
  ∷ ∀ a
  . (Json → Either String a)
  → Json
  → Either String (SD.Expr a)
decodeExpr rec json =
  traceError ("[expr: " ⊕ show json ⊕ "]") do
    obj ← decodeJson json
    ty ← traceError "type" $ obj .? "type"
    traceError ty
      case ty of
        "lit" → SD.Literal <$> (rec =<< obj .? "value")
        "uneval" → SD.Unevaluated <$> obj .? "value"
        _ → Left $ "unknown code expr type '" ⊕ ty ⊕ "'"

encodeFormField
  ∷ SD.FormField MarkdownExpr
  → Json
encodeFormField field =
  case field of
    SD.TextBox tb →
      "type" := "textbox"
        ~> "textBox" := encodeTextBox tb
        ~> jsonEmptyObject
    SD.RadioButtons x xs →
      "type" := "radios"
        ~> "selection" := encodeExpr encodeJson x
        ~> "labels" := encodeExpr encodeJson xs
        ~> jsonEmptyObject
    SD.CheckBoxes bs xs →
      "type" := "checkboxes"
        ~> "selection" := encodeExpr encodeJson bs
        ~> "labels" := encodeExpr encodeJson xs
        ~> jsonEmptyObject
    SD.DropDown mx xs →
      "type" := "dropdown"
        ~> "selection" := map (encodeExpr encodeJson) mx
        ~> "labels" := encodeExpr encodeJson xs
        ~> jsonEmptyObject


decodeFormField
  ∷ Json
  → Either String (SD.FormField MarkdownExpr)
decodeFormField =
  decodeJson >=> \obj → do
    ty ← obj .? "type"
    traceError ty
      case ty of
        "textbox" → do
          tb ← obj .? "textBox" >>= decodeTextBox
          pure $ SD.TextBox tb
        "radios" → do
          selection ← traceError "selection" $ obj .? "selection" >>= decodeExpr decodeJson
          labels ← traceError "labels" $ obj .? "labels" >>= decodeExpr decodeJson
          pure $ SD.RadioButtons selection labels
        "checkboxes" → do
          selection ← traceError "selection" $ obj .? "selection" >>= decodeExpr decodeJson
          labels ← traceError "labels" $ obj .? "labels" >>= decodeExpr decodeJson
          pure $ SD.CheckBoxes selection labels
        "dropdown" → do
          selection ← traceError "selection" $ obj .? "selection" >>= decodeMaybe (decodeExpr decodeJson)
          labels ← traceError "labels" $ obj .? "labels" >>= decodeExpr decodeJson
          pure $ SD.DropDown selection labels
        _ → Left $ "Unknown form field type '" ⊕ ty ⊕ "'"

decodeMaybe
  ∷ ∀ a
  . (Json → Either String a)
  → Json
  → Either String (Maybe a)
decodeMaybe dec j =
  (Just <$> dec j)
    <|> pure Nothing

decodeTextBox
  ∷ Json
  → Either String (SD.TextBox (Compose Maybe SD.Expr))
decodeTextBox =
  decodeJson >=> \obj → do
    ty ← obj .? "type"
    value ← obj .? "value"
    traceError ty
      case ty of
        "plaintext" →
          SD.PlainText ∘ Compose <$>
            T.traverse (decodeExpr decodeJson) value
        "numeric" →
          SD.Numeric ∘ Compose <$>
            T.traverse (decodeExpr decodeHugeNum) value
        "datetime" →
          SD.DateTime (decodePrecision obj) ∘ Compose <$>
            T.traverse (decodeExpr decodeDateTime) value
        "date" →
          SD.Date ∘ Compose <$>
            T.traverse (decodeExpr decodeDate) value
        "time" →
          SD.Time (decodePrecision obj) ∘ Compose <$>
            T.traverse (decodeExpr decodeTime) value
        _ → Left $ "Unknown text box type '" ⊕ ty ⊕ "'"

  where
    decodeHugeNum ∷ Json → Either String HN.HugeNum
    decodeHugeNum =
      decodeJson
        >=> HN.fromString
        >>> maybe (Left "Error decoding number") Right

    decodeDateTime ∷ Json → Either String DT.DateTime
    decodeDateTime =
      decodeJson >=> \obj →
        DT.DateTime
          <$> (obj .? "date" >>= decodeDate)
          <*> (obj .? "time" >>= decodeTime)

    decodeDate ∷ Json → Either String DT.Date
    decodeDate =
      decodeJson >=> \obj → do
        year ← obj .? "year"
        month ← obj .? "month"
        day ← obj .? "day"
        maybe (Left "Invalid date value") Right $
          join $ DT.exactDate <$> toEnum year <*> toEnum month <*> toEnum day

    decodeTime ∷ Json → Either String DT.Time
    decodeTime =
      decodeJson >=> \obj → do
        hours ← obj .? "hours"
        minutes ← obj .? "minutes"
        seconds ← (obj .? "seconds") <|> pure Nothing
        maybe (Left "Invalid time value") Right $
          DT.Time <$> toEnum hours <*> toEnum minutes <*> toEnum (fromMaybe 0 seconds) <*> pure bottom

    decodePrecision ∷ JObject -> SD.TimePrecision
    decodePrecision obj =
      if either (const false) id $ (_ == "seconds") <$> obj .? "prec"
      then SD.Seconds
      else SD.Minutes


-- Note: we use strings to represent numeric values to avoid losing precision
encodeTextBox
  ∷ SD.TextBox (Compose Maybe SD.Expr)
  → Json
encodeTextBox tb =
  case tb of
    SD.PlainText (Compose x) →
      "type" := "plaintext"
        ~> "value" := map (encodeExpr encodeJson) x
        ~> jsonEmptyObject
    SD.Numeric (Compose x) →
      "type" := "numeric"
        ~> "value" := map (encodeExpr (encodeJson ∘ HN.toString)) x
        ~> jsonEmptyObject
    SD.DateTime prec (Compose x) →
      "type" := "datetime"
        ~> "value" := map (encodeExpr encodeDateTime) x
        ~> "prec" := encodePrecision prec
        ~> jsonEmptyObject
    SD.Date (Compose x) →
      "type" := "date"
        ~> "value" := map (encodeExpr encodeDate) x
        ~> jsonEmptyObject
    SD.Time prec (Compose x) →
      "type" := "time"
        ~> "value" := map (encodeExpr encodeTime) x
        ~> "prec" := encodePrecision prec
        ~> jsonEmptyObject

  where
    encodeDate d =
      "year" := fromEnum (DT.year d)
        ~> "month" := fromEnum (DT.month d)
        ~> "day" := fromEnum (DT.day d)
        ~> jsonEmptyObject

    encodeTime t =
      "hours" := fromEnum (DT.hour t)
        ~> "minutes" := fromEnum (DT.minute t)
        ~> "seconds" := fromEnum (DT.second t)
        ~> jsonEmptyObject

    encodeDateTime (DT.DateTime date time) =
      "date" := encodeDate date
        ~> "time" := encodeTime time
        ~> jsonEmptyObject

    encodePrecision =
      encodeJson ∘ case _ of
        SD.Seconds -> "seconds"
        SD.Minutes -> "minutes"


encodeFormFieldValue
  ∷ SDS.FormFieldValue MarkdownExpr
  → Json
encodeFormFieldValue =
  encodeFormField
    ∘ SD.transFormField (SD.Literal ∘ unwrap)

decodeFormFieldValue
  ∷ Json
  → Either String (SDS.FormFieldValue MarkdownExpr)
decodeFormFieldValue json = do
  field ← decodeFormField json
  SD.traverseFormField (map pure ∘ SD.getLiteral) field
    # maybe (Left "Invalid form field value") pure
