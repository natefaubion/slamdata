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

module SlamData.Workspace.Card.Search.Eval
  ( evalSearch
  ) where

import SlamData.Prelude

import Control.Monad.Aff.Class (class MonadAff)
import Control.Monad.State (class MonadState, get, put)
import Control.Monad.Writer.Class (class MonadTell)
import SlamData.Effects (SlamDataEffects)
import SlamData.Quasar.Class (class ParQuasarDSL)
import SlamData.Workspace.Card.Error as CE
import SlamData.Workspace.Card.Eval.Common as CEC
import SlamData.Workspace.Card.Eval.Monad as CEM
import SlamData.Workspace.Card.Port as Port
import SlamData.Workspace.Card.Port.VarMap as VM
import SlamData.Workspace.Card.Search.Error (SearchError(..), throwSearchError)
import SlamData.Workspace.Card.Search.Interpret as Search
import SlamData.Workspace.Card.Setups.Axis as Ax
import SlamData.Workspace.Card.Setups.Common.Eval (analyze)
import SqlSquared as Sql
import Text.SlamSearch as SS

evalSearch
  ∷ ∀ m v
  . MonadAff SlamDataEffects m
  ⇒ MonadAsk CEM.CardEnv m
  ⇒ MonadState CEM.CardState m
  ⇒ MonadThrow (Variant (search ∷ SearchError, qerror ∷ CE.QError, resource ∷ CE.ResourceError | v)) m
  ⇒ MonadTell CEM.CardLog m
  ⇒ ParQuasarDSL m
  ⇒ String
  → Port.Port
  → m Port.Out
evalSearch queryText port = do
  let
    queryText'
      | queryText ≡ "" = "*"
      | otherwise = queryText
  CEM.CardEnv { cardId, varMap } ← ask
  searchQuery ← case SS.mkQuery queryText' of
    Left pe → throwSearchError $ SearchQueryParseError { query: queryText, error: pe }
    Right q → pure q
  resourceVar × resource ← CEM.extractResourcePair port
  records × axes ← analyze resource =<< get
  let
    state' = CEM.Analysis { resource, records, axes }
    fields = Ax.axesToFields axes
    sql = Search.searchSql resourceVar Search.defaultFilterVar Search.defaultDistinctVar
    filter = VM.Expr $ Search.filterSql fields searchQuery
    isDistinct = VM.Expr $ Sql.bool $ Search.isDistinct searchQuery
    varMap' =
      varMap
        # VM.insert cardId Search.defaultFilterVar filter
        # VM.insert cardId Search.defaultDistinctVar isDistinct
  put $ Just state'
  resource' ← CEC.localEvalResource (Sql.Query mempty sql) varMap' >>= searchError SearchQueryCompilationError
  pure $ Port.resourceOut cardId resource' varMap'

searchError ∷ ∀ e a m v. MonadThrow (Variant (search ∷ SearchError | v)) m ⇒ (e → SearchError) → Either e a → m a
searchError f = either (throwSearchError ∘ f) pure
