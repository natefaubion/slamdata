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

module SlamData.Workspace.Card.Query.Eval
  ( evalQuery
  ) where

import SlamData.Prelude

import Control.Monad.Writer.Class (class MonadTell)
import SlamData.Quasar.Class (class QuasarDSL, class ParQuasarDSL)
import SlamData.Workspace.Card.Error as CE
import SlamData.Workspace.Card.Eval.Common as CEC
import SlamData.Workspace.Card.Eval.Monad as CEM
import SlamData.Workspace.Card.Port as Port
import SlamData.Workspace.Card.Query.Error (QueryError(..), throwQueryError)
import SqlSquared as Sql
import SqlSquared.Parser (prettyParse)

evalQuery
  ∷ ∀ m v
  . MonadAsk CEM.CardEnv m
  ⇒ MonadThrow (Variant (query ∷ QueryError, qerror ∷ CE.QError | v)) m
  ⇒ MonadTell CEM.CardLog m
  ⇒ QuasarDSL m
  ⇒ ParQuasarDSL m
  ⇒ String
  → Port.VarMap
  → m Port.Out
evalQuery sqlInput varMap = do
  sql ← queryError QueryParseError $ prettyParse Sql.parseQuery sqlInput
  resource ← CEC.localEvalResource sql varMap >>= queryError QueryCompileError
  CEM.resourceOut resource

queryError ∷ ∀ e a m v. MonadThrow (Variant (query ∷ QueryError | v)) m ⇒ (e → QueryError) → Either e a → m a
queryError e = either throwQueryError pure ∘ lmap e
