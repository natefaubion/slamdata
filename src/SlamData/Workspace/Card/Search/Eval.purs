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
import Control.Monad.Writer.Class (class MonadTell)
import Data.Path.Pathy as Path
import Data.StrMap as SM
import SlamData.Effects (SlamDataEffects)
import SlamData.Quasar.Class (class QuasarDSL, class ParQuasarDSL)
import SlamData.Quasar.FS as QFS
import SlamData.Quasar.Query as QQ
import SlamData.Workspace.Card.Error as CE
import SlamData.Workspace.Card.Eval.Common (validateResources)
import SlamData.Workspace.Card.Eval.Monad as CEM
import SlamData.Workspace.Card.Port as Port
import SlamData.Workspace.Card.Search.Interpret as Search
import SqlSquared as Sql
import Text.SlamSearch as SS

evalSearch
  ∷ ∀ m
  . MonadAff SlamDataEffects m
  ⇒ MonadAsk CEM.CardEnv m
  ⇒ MonadThrow CE.CardError m
  ⇒ MonadTell CEM.CardLog m
  ⇒ QuasarDSL m
  ⇒ ParQuasarDSL m
  ⇒ String
  → Port.Resource
  → m Port.Out
evalSearch queryText resource = do
  CEM.CardEnv env ← ask
  filePath ← CEM.anyTemporaryPath $ Port.filePath resource
  let queryText' = if queryText ≡ "" then "*" else queryText
  query ← case SS.mkQuery queryText' of
    Left pe → CE.throwSearchError (CE.SearchQueryParseError { query: queryText, error: pe })
    Right q → pure q

  fields ← CE.liftQ do
    _ ← QFS.messageIfFileNotFound
      filePath
      ("Input resource " ⊕ Path.printPath filePath ⊕ " doesn't exist")
    QQ.fields filePath

  tmpPath × outputResource ← CEM.temporaryOutputResource

  let
    sql = Sql.Query mempty $ Search.queryToSql fields query filePath
    backendPath = fromMaybe Path.rootDir $ Path.parentDir filePath

  compileResult ← QQ.compile backendPath sql SM.empty

  case compileResult of
    Left err →
      CE.throwSearchError (CE.SearchQueryCompilationError err)
    Right { inputs } → do
      validateResources inputs
      CEM.addSources inputs

  _ ← CE.liftQ do
    _ ← QQ.viewQuery tmpPath sql SM.empty
    QFS.messageIfFileNotFound
      tmpPath
      "Error making search temporary resource"

  CEM.resourceOut $ Port.View outputResource sql SM.empty
