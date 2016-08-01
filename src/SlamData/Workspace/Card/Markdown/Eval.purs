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

module SlamData.Workspace.Card.Markdown.Eval
  ( markdownEval
  ) where

import SlamData.Prelude

import Control.Monad.Aff as Aff
import Control.Monad.Aff.Free as AffF
import Control.Monad.Eff.Class as Eff
import Control.Monad.Eff.Exception as Exn
import Control.Monad.Error.Class as Err
import Control.Monad.Writer.Trans as WT

import Data.Array as A
import Data.Identity (Identity)
import Data.Int as Int
import Data.JSDate as JSD
import Data.Json.Extended as EJSON
import Data.Lens ((^?), _Just)
import Data.List as L
import Data.Set as Set
import Data.String as S
import Data.StrMap as SM

import SlamData.Effects (Slam, SlamDataEffects)
import SlamData.Workspace.Card.Eval.CardEvalT as CET
import SlamData.Workspace.Card.Port as Port
import SlamData.Quasar.Query as Quasar
import SlamData.Workspace.Deck.AdditionalSource (AdditionalSource)

import Text.Markdown.SlamDown as SD
import Text.Markdown.SlamDown.Traverse as SDT
import Text.Markdown.SlamDown.Eval as SDE
import Text.Markdown.SlamDown.Parser as SDP

import Text.Parsing.Parser as P
import Text.Parsing.Parser.Combinators as PC
import Text.Parsing.Parser.String as PS

import Utils.Path (DirPath)

markdownEval
  ∷ ∀ m
  . (Monad m, AffF.Affable SlamDataEffects m)
  ⇒ CET.CardEvalInput
  → Maybe String
  → CET.CardEvalT m Port.Port
markdownEval { input, path } str =
  case SDP.parseMd (fromMaybe "" str) of
    Left e → Err.throwError e
    Right sd → do
      let
        vm = fromMaybe Port.emptyVarMap $ input ^? _Just ∘ Port._VarMap
        sm = map Port.renderVarMapValue vm
      result  ← lift ∘ AffF.fromAff ∘ Aff.attempt ∘ evalEmbeddedQueries sm path $ sd
      case result of
        Left e → Err.throwError $ Exn.message e
        Right (doc × as) → do
          CET.additionalSources as
          pure $ Port.SlamDown (vm × doc)

findFields
  ∷ ∀ a
  . SD.SlamDownP a
  → Array String
findFields = SDT.everything (const mempty) extractField
  where
  extractField ∷ SD.Inline a → Array String
  extractField (SD.FormField label _ _) = pure label
  extractField _ = mempty

type EvalM = WT.WriterT (Set.Set AdditionalSource) Slam

runEvalM ∷ ∀ a. EvalM a → Slam (a × (Set.Set AdditionalSource))
runEvalM = WT.runWriterT

evalEmbeddedQueries
  ∷ SM.StrMap String
  → DirPath
  → SD.SlamDownP Port.VarMapValue
  → Slam ((SD.SlamDownP Port.VarMapValue) × (Set.Set AdditionalSource))
evalEmbeddedQueries sm dir =
  runEvalM ∘
    SDE.eval
      { code: evalCode
      , textBox: evalTextBox
      , value: evalValue
      , list: evalList
      }

  where

  evalCode
    ∷ Maybe SDE.LanguageId
    → String
    → EvalM Port.VarMapValue
  evalCode mid code
    | languageIsSql mid =
        Port.Literal ∘ extractCodeValue <$> runQuery code
    | otherwise =
        pure $ Port.QueryExpr code

  extractCodeValue ∷ Array EJSON.EJson → EJSON.EJson
  extractCodeValue [ej] = extractSingletonObject ej
  extractCodeValue arr = EJSON.array arr

  languageIsSql
    ∷ Maybe SDE.LanguageId
    → Boolean
  languageIsSql =
    maybe
      true
      ((_ ≡ "sql") ∘ S.toLower)

  extractSingletonObject
    ∷ EJSON.EJson
    → EJSON.EJson
  extractSingletonObject lit =
    case EJSON.unroll lit of
      EJSON.Object [key × val] → val
      _ → lit

  evalValue
    ∷ String
    → EvalM Port.VarMapValue
  evalValue code = do
    maybe (Port.Literal EJSON.null) (Port.Literal ∘ extractSingletonObject)
      ∘ A.head
      <$> runQuery code

  evalTextBox
    ∷ SD.TextBox (Const String)
    → EvalM (SD.TextBox Identity)
  evalTextBox tb = do
    let sql = getConst $ SD.traverseTextBox (map \_ → Const unit) tb
    mresult ← A.head <$> runQuery sql
    result ←
      maybe (Err.throwError $ Exn.error "No results") (pure ∘ extractSingletonObject) mresult
    case tb × (EJSON.unroll result) of
      (SD.PlainText _) × (EJSON.String str) →
        pure ∘ SD.PlainText $ pure str
      (SD.Numeric _) × (EJSON.Decimal a) →
        pure ∘ SD.Numeric $ pure a
      (SD.Time prec _) × (EJSON.Time str) →
        SD.Time prec ∘ pure <$> parse parseSqlTime str
      (SD.Date _) × (EJSON.Date str) →
        SD.Date ∘ pure <$> parse parseSqlDate str
      (SD.DateTime prec _) × (EJSON.Timestamp str) →
        SD.DateTime prec ∘ pure <$> parseSqlTimeStamp str
      _ → Err.throwError ∘ Exn.error $ "Type error: " ⊕ show result ⊕ " does not match " ⊕ show tb
    where
      parse ∷ ∀ s a. P.Parser s a → s → EvalM a
      parse p str =
        case P.runParser str p of
          Left err → Err.throwError ∘ Exn.error $ "Parse error: " ⊕ show err
          Right a → pure a

  evalList
    ∷ String
    → EvalM (L.List Port.VarMapValue)
  evalList code = do
    items ← map extractSingletonObject <$> runQuery code
    let limit = 500
    pure ∘ L.fromFoldable
      $ if A.length items > limit
          then
          map Port.Literal
            (A.take limit items)
            ⊕ [ SD.stringValue $ "<" ⊕ show limit ⊕ "item limit reached>" ]
          else
          Port.Literal <$> items

  runQuery
    ∷ String
    → EvalM (Array EJSON.EJson)
  runQuery code = do
    compiled ← Quasar.compile (Left dir) code sm
    case compiled of
      Left e → Err.throwError e
      Right {inputs} → CET.addSources inputs

    result ← Quasar.queryEJsonVM dir code sm

    either
      (Err.throwError ∘ Exn.error)
      pure
      result

parseDigit ∷ P.Parser String Int
parseDigit =
  PC.choice
    [ 0 <$ PS.string "0"
    , 1 <$ PS.string "1"
    , 2 <$ PS.string "2"
    , 3 <$ PS.string "3"
    , 4 <$ PS.string "4"
    , 5 <$ PS.string "5"
    , 6 <$ PS.string "6"
    , 7 <$ PS.string "7"
    , 8 <$ PS.string "8"
    , 9 <$ PS.string "9"
    ]

many1
  ∷ ∀ s a
  . P.Parser s a
  → P.Parser s (L.List a)
many1 p =
  L.Cons
    <$> p
    <*> L.many p

parseNat ∷ P.Parser String Int
parseNat =
  many1 parseDigit
    <#> foldl (\a i → a * 10 + i) 0

hyphen ∷ P.Parser String Unit
hyphen = void $ PS.string "-"

colon ∷ P.Parser String Unit
colon = void $ PS.string ":"

parseSqlDate ∷ P.Parser String SD.DateValue
parseSqlDate = do
  year ← parseNat
  hyphen
  month ← parseNat
  hyphen
  day ← parseNat
  pure { year, month, day }

parseSqlTime ∷ P.Parser String SD.TimeValue
parseSqlTime = do
  hours ← parseNat
  colon
  minutes ← parseNat
  colon
  seconds ← Just <$> parseNat
  pure { hours, minutes, seconds }

-- | Parses a date-time string into the user's current locale.
parseSqlTimeStamp
  ∷ String
  → EvalM SD.DateTimeValue
parseSqlTimeStamp str = do
  d <- Eff.liftEff $ JSD.parse str
  case JSD.isValid d of
    false → Err.throwError ∘ Exn.error $ "Invalid date: " ⊕ show str
    true → Eff.liftEff do
      year ← Int.round <$> JSD.getFullYear d
      month ← (_ + 1) ∘ Int.round <$> JSD.getMonth d
      day ← Int.round <$> JSD.getDate d
      hours ← Int.round <$> JSD.getHours d
      minutes ← Int.round <$> JSD.getMinutes d
      seconds ← Just ∘ Int.round <$> JSD.getSeconds d
      pure
        { date: { year , month , day }
        , time: { hours , minutes, seconds }
        }
