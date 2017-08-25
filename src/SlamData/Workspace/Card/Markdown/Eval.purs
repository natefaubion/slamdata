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
  ( evalMarkdown
  , evalMarkdownForm
  ) where

import SlamData.Prelude

import Control.Monad.Eff.Class (class MonadEff)
import Control.Monad.State.Class (class MonadState, put)
import Control.Monad.Writer.Class (class MonadTell)
import Data.Array as A
import Data.HugeInt as HI
import Data.Identity (Identity)
import Data.Json.Extended as EJSON
import Data.List as L
import Data.List.NonEmpty as NEL
import Data.Newtype (wrap)
import Data.NonEmpty ((:|))
import Data.Path.Pathy ((</>), unsandbox, currentDir)
import Data.StrMap as SM
import Data.String as S
import Matryoshka (project, transAna)
import SlamData.Effects (SlamDataEffects)
import SlamData.Quasar.Class (class QuasarDSL)
import SlamData.Quasar.Query as Quasar
import SlamData.SqlSquared.Tagged as SqlT
import SlamData.Workspace.Card.Error as CE
import SlamData.Workspace.Card.Eval.Monad as CEM
import SlamData.Workspace.Card.Eval.Process as Process
import SlamData.Workspace.Card.Eval.State as CES
import SlamData.Workspace.Card.Markdown.Component.State as MDS
import SlamData.Workspace.Card.Markdown.Error (MarkdownError(..), throwMarkdownError)
import SlamData.Workspace.Card.Markdown.Interpret (formFieldDefaultValue)
import SlamData.Workspace.Card.Markdown.Model as MD
import SlamData.Workspace.Card.Port as Port
import SlamData.Workspace.Card.Port.VarMap as VM
import SqlSquared as Sql
import SqlSquared.Parser (prettyParse)
import Text.Markdown.SlamDown as SD
import Text.Markdown.SlamDown.Eval as SDE
import Text.Markdown.SlamDown.Halogen.Component.State as SDH
import Text.Markdown.SlamDown.Parser as SDP
import Text.Markdown.SlamDown.Traverse as SDT
import Utils.Path (DirPath, tmpDir)

evalMarkdownForm
  ∷ ∀ m
  . MonadEff SlamDataEffects m
  ⇒ MonadAsk CEM.CardEnv m
  ⇒ MonadState CEM.CardState m
  ⇒ Monad m
  ⇒ MD.Model
  → SD.SlamDownP MD.MarkdownExpr
  → Port.VarMap
  → m Port.Out
evalMarkdownForm model doc varMap = do
  cardId ← CEM.localCardId
  let
    inputState = SDH.formStateFromDocument doc
    outputState = MDS.updateFormState inputState model
    formVarMap = formFieldDefaultValue <$> outputState
    varMap' = SM.fold (\vm k v → VM.insert cardId (VM.Var k) v vm) varMap formVarMap
  put $ Just $ CES.SlamDown outputState
  pure (Port.Variables × varMap')

evalMarkdown
  ∷ ∀ m v
  . MonadEff SlamDataEffects m
  ⇒ MonadAsk CEM.CardEnv m
  ⇒ MonadThrow (Variant (markdown ∷ MarkdownError, qerror ∷ CE.QError | v)) m
  ⇒ MonadTell CEM.CardLog m
  ⇒ QuasarDSL m
  ⇒ String
  → Port.VarMap
  → m Port.Out
evalMarkdown str varMap = do
  CEM.CardEnv { path } ← ask
  case SDP.parseMd str of
    Left e → throwMarkdownError (MarkdownParseError {markdown: str, error: e})
    Right sd → do
      doc ← evalEmbeddedQueries varMap path sd
      pure (Port.SlamDown doc × varMap)

findFields
  ∷ ∀ a
  . SD.SlamDownP a
  → Array String
findFields = SDT.everything (const mempty) extractField
  where
  extractField ∷ SD.Inline a → Array String
  extractField (SD.FormField label _ _) = pure label
  extractField _ = mempty

evalEmbeddedQueries
  ∷ ∀ m v
  . MonadEff SlamDataEffects m
  ⇒ MonadThrow (Variant (markdown ∷ MarkdownError, qerror ∷ CE.QError | v)) m
  ⇒ MonadTell CEM.CardLog m
  ⇒ QuasarDSL m
  ⇒ VM.VarMap
  → DirPath
  → SD.SlamDownP MD.MarkdownExpr
  → m (SD.SlamDownP MD.MarkdownExpr)
evalEmbeddedQueries varMap dir =
  SDE.eval
    { code: evalCode
    , textBox: evalTextBox
    , value: evalValue
    , list: evalList
    }

  where
  urlVarMap ∷ VM.URLVarMap
  urlVarMap = VM.toURLVarMap varMap

  evalCode
    ∷ Maybe SDE.LanguageId
    → String
    → m MD.MarkdownExpr
  evalCode mid code
    | languageIsSql mid =
        wrap ∘ transAna Sql.Literal ∘ extractCodeValue <$> runQuery Nothing code
    | otherwise =
        pure $ wrap $ Sql.null

  extractCodeValue ∷ Array EJSON.EJson → EJSON.EJson
  extractCodeValue [ej] = extractSingletonObject ej
  extractCodeValue arr = EJSON.array arr

  languageIsSql
    ∷ Maybe SDE.LanguageId
    → Boolean
  languageIsSql =
    maybe true $ eq "sql" ∘ S.toLower

  extractSingletonObject
    ∷ EJSON.EJson
    → EJSON.EJson
  extractSingletonObject lit =
    case project lit of
      EJSON.Map (EJSON.EJsonMap [key × val]) → val
      _ → lit

  evalValue
    ∷ String
    → String
    → m MD.MarkdownExpr
  evalValue field code = do
    maybe
      (wrap Sql.null)
      (wrap ∘ transAna Sql.Literal ∘ extractSingletonObject)
      ∘ A.head
      <$> runQuery (Just field) code

  evalTextBox
    ∷ String
    → SD.TextBox (Const String)
    → m (SD.TextBox Identity)
  evalTextBox field tb = do
    let sql = unwrap $ SD.traverseTextBox (map \_ → Const unit) tb
    mresult ← A.head <$> runQuery (Just field) sql
    result ←
      maybe (throwMarkdownError (MarkdownNoTextBoxResults { field, sql })) (pure ∘ extractSingletonObject) mresult
    case tb, project result of
      (SD.PlainText _), (EJSON.String str) →
        pure ∘ SD.PlainText $ pure str
      (SD.Numeric _), (EJSON.Decimal a) →
        pure ∘ SD.Numeric $ pure a
      (SD.Numeric _), (EJSON.Integer a) →
        pure ∘ SD.Numeric $ pure (HI.toHugeNum a)
      (SD.Time prec _), (EJSON.String time) →
        case SqlT.parseTime time of
          Left error → throwMarkdownError (MarkdownInvalidTimeValue { field, time, error: unwrap error })
          Right r → pure ∘ SD.Time prec $ pure r
      (SD.Date _), (EJSON.String date) →
        case SqlT.parseDate date of
          Left error → throwMarkdownError (MarkdownInvalidDateValue { field, date, error: unwrap error })
          Right r → pure ∘ SD.Date $ pure r
      (SD.DateTime prec _), (EJSON.String datetime) →
        case SqlT.parseDateTime datetime of
          Left error → throwMarkdownError (MarkdownInvalidDateTimeValue { field, datetime, error: unwrap error })
          Right r → pure ∘ SD.DateTime prec $ pure r
      _, _ →
        throwMarkdownError (MarkdownTypeError { field, actual: typeOf result, expected: typesOfField tb })

  -- TODO: use `Meta` when we have it to better determine types. -gb
  typeOf :: EJSON.EJson -> String
  typeOf = show ∘ EJSON.getType

  typesOfField :: ∀ a. SD.TextBox a -> NEL.NonEmptyList String
  typesOfField = case _ of
    SD.PlainText _ → pure "String"
    SD.Numeric _ → NEL.NonEmptyList $ "Number" :| pure "Integer"
    SD.Date _ → pure "Date"
    SD.Time _ _ → pure "Time"
    SD.DateTime _ _ → pure "Timestamp"

  evalList
    ∷ String
    → String
    → m (L.List MD.MarkdownExpr)
  evalList field code = do
    jitems ← map extractSingletonObject <$> runQuery (Just field) code
    let limit = 500
    pure ∘ L.fromFoldable
      $ if A.length jitems > limit
          then
            ( map (wrap ∘ transAna Sql.Literal) $ A.take limit jitems )
            ⊕ [ wrap $ Sql.string $ "<" ⊕ show limit ⊕ "item limit reached>" ]
          else
          map (wrap ∘ transAna Sql.Literal) jitems

  runQuery
    ∷ Maybe String
    → String
    → m (Array EJSON.EJson)
  runQuery field code =
    let
      esql =
        Process.elaborateQuery (unsandbox (currentDir </> tmpDir)) varMap
          <$> prettyParse Sql.parseQuery code
    in
      case esql of
        Left error →
          throwMarkdownError (MarkdownSqlParseError { field, sql: code, error: error })
        Right sql → do
          let varMap' = VM.unURLVarMapF $ urlVarMap
          { inputs } ← CE.liftQ $ Quasar.compile dir sql varMap'
          CEM.addSources inputs
          CE.liftQ $ Quasar.queryEJsonVM dir sql varMap'
