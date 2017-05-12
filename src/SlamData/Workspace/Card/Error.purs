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

module SlamData.Workspace.Card.Error
  ( module SlamData.Workspace.Card.Error
  , module CCE
  , module CDLOE
  , module CHE
  , module CMDE
  , module COE
  , module CQE
  , module CSE
  , module CTE
  , module FILE
  , module FISE
  ) where

import SlamData.Prelude

import Quasar.Error (QError)
import SlamData.GlobalError as GE
import SlamData.Workspace.Card.Cache.Error as CCE
import SlamData.Workspace.Card.Chart.Error as CHE
import SlamData.Workspace.Card.DownloadOptions.Error as CDLOE
import SlamData.Workspace.Card.Markdown.Error as CMDE
import SlamData.Workspace.Card.Open.Error as COE
import SlamData.Workspace.Card.Query.Error as CQE
import SlamData.Workspace.Card.Search.Error as CSE
import SlamData.Workspace.Card.Setups.FormInput.Labeled.Error as FILE
import SlamData.Workspace.Card.Setups.FormInput.Static.Error as FISE
import SlamData.Workspace.Card.Table.Error as CTE
import Utils (hush)

data CardError
  = QuasarError QError
  | StringlyTypedError String
  | CacheCardError CCE.CacheError
  | ChartCardError CHE.ChartError
  | DownloadOptionsCardError CDLOE.DownloadOptionsError
  | FormInputLabeledCardError FILE.FormInputLabeledError
  | FormInputStaticCardError FISE.FormInputStaticError
  | MarkdownCardError CMDE.MarkdownError
  | OpenCardError COE.OpenError
  | QueryCardError CQE.QueryError
  | SearchCardError CSE.SearchError
  | TableCardError CTE.TableError

instance showCardError ∷ Show CardError where
  show = case _ of
    QuasarError err → "(QuasarError " <> show err <> ")"
    StringlyTypedError err → "(StringlyTypedError " <> err <> ")"
    CacheCardError err → "(CacheCardError " <> show err <> ")"
    ChartCardError err → "(ChartCardError " <> show err <> ")"
    DownloadOptionsCardError err → "(DownloadOptionsCardError " <> show err <> ")"
    FormInputLabeledCardError err → "(FormInputLabeledCardError " <> show err <> ")"
    FormInputStaticCardError err → "(FormInputStaticCardError " <> show err <> ")"
    MarkdownCardError err → "(MarkdownCardError " <> show err <> ")"
    OpenCardError err → "(OpenCardError " <> show err <> ")"
    QueryCardError err → "(QueryCardError " <> show err <> ")"
    SearchCardError err → "(SearchCardError " <> show err <> ")"
    TableCardError err → "(TableCardError " <> show err <> ")"

quasarToCardError ∷ QError → CardError
quasarToCardError = QuasarError

cardToGlobalError ∷ CardError → Maybe GE.GlobalError
cardToGlobalError = case _ of
  QuasarError qError → hush (GE.fromQError qError)
  StringlyTypedError err → Nothing
  CacheCardError err → CCE.cacheToGlobalError err
  ChartCardError err → CHE.chartToGlobalError err
  DownloadOptionsCardError _ → Nothing
  FormInputLabeledCardError _ → Nothing
  FormInputStaticCardError _ → Nothing
  MarkdownCardError _ → Nothing
  OpenCardError _ → Nothing
  QueryCardError err → CQE.queryToGlobalError err
  SearchCardError err → CSE.searchToGlobalError err
  TableCardError err → CTE.tableToGlobalError err

-- TODO(Christoph): use this warn constraint to track down unstructured error messages
-- throw ∷ ∀ m a. MonadThrow CardError m ⇒ Warn "You really don't want to" ⇒ String → m a
throw ∷ ∀ m a. MonadThrow CardError m ⇒ String → m a
throw = throwError ∘ StringlyTypedError

throwCacheError ∷ ∀ m a. MonadThrow CardError m ⇒ CCE.CacheError → m a
throwCacheError = throwError ∘ CacheCardError

throwQueryError ∷ ∀ m a. MonadThrow CardError m ⇒ CQE.QueryError → m a
throwQueryError = throwError ∘ QueryCardError

liftQueryError ∷ ∀ m a. MonadThrow CardError m ⇒ (Either CQE.QueryError a) → m a
liftQueryError x = case lmap QueryCardError x of
  Left err → throwError err
  Right v → pure v

throwDownloadOptionsError ∷ ∀ m a. MonadThrow CardError m ⇒ CDLOE.DownloadOptionsError → m a
throwDownloadOptionsError = throwError ∘ DownloadOptionsCardError

throwMarkdownError ∷ ∀ m a. MonadThrow CardError m ⇒ CMDE.MarkdownError → m a
throwMarkdownError = throwError ∘ MarkdownCardError

throwOpenError ∷ ∀ m a. MonadThrow CardError m ⇒ COE.OpenError → m a
throwOpenError = throwError ∘ OpenCardError

throwTableError ∷ ∀ m a. MonadThrow CardError m ⇒ CTE.TableError → m a
throwTableError = throwError ∘ TableCardError

throwChartError ∷ ∀ m a. MonadThrow CardError m ⇒ CHE.ChartError → m a
throwChartError = throwError ∘ ChartCardError

throwFormInputStaticError ∷ ∀ m a. MonadThrow CardError m ⇒ FISE.FormInputStaticError → m a
throwFormInputStaticError = throwError ∘ FormInputStaticCardError

throwFormInputLabeledError ∷ ∀ m a. MonadThrow CardError m ⇒ FILE.FormInputLabeledError → m a
throwFormInputLabeledError = throwError ∘ FormInputLabeledCardError

throwSearchError ∷ ∀ m a. MonadThrow CardError m ⇒ CSE.SearchError → m a
throwSearchError = throwError ∘ SearchCardError

liftQ ∷ ∀ m a. MonadThrow CardError m ⇒ m (Either QError a) → m a
liftQ = flip bind (either (throwError ∘ quasarToCardError) pure)
