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

module SlamData.Workspace.Card.Query.Error where

import SlamData.Prelude

import Quasar.Advanced.QuasarAF (QError)
import SlamData.GlobalError as GE
import Utils (throwVariantError)

data QueryError
  = QueryCompileError QError
  -- ???: It's not entirely clear how this arises, the QueryCompileError case seems to catch the test cases we tried -gb
  | QueryRetrieveResultError QError
  | QueryParseError String

instance showQueryError ∷ Show QueryError where
  show = case _ of
    QueryCompileError qe → "(QueryCompileError " <> show qe <> ")"
    QueryRetrieveResultError qe → "(QueryRetrieveResultError " <> show qe <> ")"
    QueryParseError pe → "(QueryParseError " <> show pe <> ")"

queryToGlobalError ∷ QueryError → Maybe GE.GlobalError
queryToGlobalError = case _ of
  QueryCompileError qErr → hush (GE.fromQError qErr)
  QueryRetrieveResultError qErr → hush (GE.fromQError qErr)
  QueryParseError pe → Nothing

throwQueryError ∷ forall v m a. MonadThrow (Variant (query ∷ QueryError | v)) m ⇒ QueryError → m a
throwQueryError = throwVariantError (SProxy :: SProxy "query")
