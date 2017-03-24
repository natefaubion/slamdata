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

module SlamData.Workspace.Card.Port.VarMap
  ( Var(..)
  , VarMap
  , URLVarMap
  , VarMapValue(..)
  , renderVarMapValue
  , emptyVarMap
  , escapeIdentifier
  , variables
  ) where

import SlamData.Prelude

import Data.Array as A
import Data.Foldable as F
import Data.HugeNum as HN
import Data.Json.Extended as EJSON
import Data.Json.Extended.Signature.Render as EJR
import Data.List as L
import Data.String.Regex (test, replace) as Regex
import Data.String.Regex.Flags (ignoreCase, global) as Regex
import Data.String.Regex.Unsafe (unsafeRegex) as Regex
import Data.StrMap as SM

import Data.Argonaut ((.?))
import Data.Argonaut.Core (jsonSingletonObject)
import Data.Argonaut.Decode (class DecodeJson, decodeJson)
import Data.Argonaut.Encode (class EncodeJson, encodeJson)

import Matryoshka (Algebra, cata)

import Text.Markdown.SlamDown.Syntax.Value as SDV

import Test.StrongCheck.Gen as Gen
import Test.StrongCheck.Arbitrary as SC

newtype Var = Var String

derive newtype instance eqVar ∷ Eq Var
derive newtype instance ordVar ∷ Ord Var
derive newtype instance decodeVar ∷ DecodeJson Var
derive newtype instance encodeVar ∷ EncodeJson Var
derive newtype instance arbitraryVar ∷ SC.Arbitrary Var
derive instance newtypeVar :: Newtype Var _

data VarMapValue
  = Literal EJSON.EJson
  | SetLiteral (Array VarMapValue)
  | QueryExpr String -- TODO: syntax of SQL^2 queries

derive instance eqVarMapValue ∷ Eq VarMapValue
derive instance ordVarMapValue ∷ Ord VarMapValue

instance showVarMapValue ∷ Show VarMapValue where
  show = renderVarMapValue

instance encodeJsonVarMapValue ∷ EncodeJson VarMapValue where
  encodeJson v =
    case v of
      Literal ejson →
        jsonSingletonObject "literal" $
          EJSON.encodeEJson ejson
      SetLiteral as →
        jsonSingletonObject "set" $
          encodeJson as
      QueryExpr str →
        jsonSingletonObject "query" $
          encodeJson str

instance decodeJsonVarMapValue :: DecodeJson VarMapValue where
  decodeJson json = do
    obj <- decodeJson json
    decodeLiteral obj
      <|> decodeSetLiteral obj
      <|> decodeQueryExpr obj

    where
      decodeLiteral =
        (_ .? "literal")
          >=> EJSON.decodeEJson
          >>> map Literal

      decodeSetLiteral =
        (_ .? "set")
          >=> decodeJson
          >>> map SetLiteral

      decodeQueryExpr  =
        (_ .? "query")
          >>> map QueryExpr

renderVarMapValue
  ∷ VarMapValue
  → String
renderVarMapValue val =
  case val of
    Literal lit → EJSON.renderEJson lit
    SetLiteral as → "(" <> F.intercalate "," (renderVarMapValue <$> as) <> ")"
    QueryExpr str → str

displayVarMapValue
  ∷ VarMapValue
  → String
displayVarMapValue val =
  case val of
    Literal lit → displayEJson lit
    SetLiteral as → "(" <> F.intercalate ", " (displayVarMapValue <$> as) <> ")"
    QueryExpr str → str

displayEJsonF ∷ Algebra EJSON.EJsonF String
displayEJsonF =
  case _ of
    EJSON.Null → "null"
    EJSON.Boolean b → if b then "true" else "false"
    EJSON.Integer i → show i
    EJSON.Decimal a → HN.toString a
    EJSON.String str → str
    EJSON.Timestamp dt → EJR.renderTimestamp dt
    EJSON.Time t → EJR.renderTime t
    EJSON.Date d → EJR.renderDate d
    EJSON.Interval str → str
    EJSON.ObjectId str → str
    EJSON.Array ds → "[" <> commaSep ds <> "]"
    EJSON.Map (EJSON.EJsonMap ds) → "{ " <> commaSep (map renderPair ds) <> " }"
  where
  commaSep ∷ Array String → String
  commaSep = F.intercalate ","
  renderPair ∷ Tuple String String → String
  renderPair (Tuple k v) = k <> ": " <> v

-- | A more readable, but forgetful renderer
displayEJson ∷ EJSON.EJson → String
displayEJson = cata displayEJsonF

instance valueVarMapValue ∷ SDV.Value VarMapValue where
  stringValue = Literal <<< EJSON.string
  renderValue = displayVarMapValue

instance arbitraryVarMapValue ∷ SC.Arbitrary VarMapValue where
  arbitrary =
    Literal <$> EJSON.arbitraryJsonEncodableEJsonOfSize 1
      <|> SetLiteral <$> Gen.arrayOf (Literal <$> EJSON.arbitraryJsonEncodableEJsonOfSize 1)
      <|> QueryExpr <$> SC.arbitrary

type VarMap = SM.StrMap VarMapValue

-- | A VarMap passed through the URL - the VarMapValues are left unparsed until
-- | they are unified with the Variables card for the deck so that values can
-- | be parsed according to their defined type.
type URLVarMap = SM.StrMap String

emptyVarMap ∷ VarMap
emptyVarMap = SM.empty

variables ∷ VarMap → L.List Var
variables = L.fromFoldable ∘ map Var ∘ A.sort ∘ SM.keys

escapeIdentifier ∷ String → String
escapeIdentifier str =
  if Regex.test identifier str
    then str
    else "`" <> Regex.replace tick "\\`" str <> "`"
  where
    identifier = Regex.unsafeRegex "^[_a-z][_a-z0-9]*$" Regex.ignoreCase
    tick = Regex.unsafeRegex "`" Regex.global
