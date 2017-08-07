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
  , module SlamData.Quasar.Error
  ) where

import SlamData.Prelude
import Data.List as L
import Data.Variant (case_, on)
import SlamData.GlobalError as GE
import SlamData.Quasar.Error (QError)
import SlamData.Workspace.Card.Cache.Error as CCE
import SlamData.Workspace.Card.Chart.Error as CHE
import SlamData.Workspace.Card.DownloadOptions.Error as CDLOE
import SlamData.Workspace.Card.Markdown.Error as CMDE
import SlamData.Workspace.Card.Open.Error as COE
import SlamData.Workspace.Card.Port.VarMap as VM
import SlamData.Workspace.Card.Query.Error as CQE
import SlamData.Workspace.Card.Search.Error as CSE
import SlamData.Workspace.Card.Setups.Chart.PivotTable.Error as CPT
import SlamData.Workspace.Card.Setups.FormInput.Labeled.Error as FILE
import SlamData.Workspace.Card.Setups.FormInput.Static.Error as FISE
import SlamData.Workspace.Card.Table.Error as CTE
import SlamData.Workspace.Card.Variables.Error as CVE
import Utils (throwVariantError)

type CardError = Variant
  ( qerror ∷ QError
  , stringly ∷ String
  , resource ∷ ResourceError
  , cache ∷ CCE.CacheError
  , chart ∷ CHE.ChartError
  , downloadOptions ∷ CDLOE.DownloadOptionsError
  , formInputLabeled ∷ FILE.FormInputLabeledError
  , formInputStatic ∷ FISE.FormInputStaticError
  , markdown ∷ CMDE.MarkdownError
  , open ∷ COE.OpenError
  , pivotTable ∷ CPT.PivotTableError
  , query ∷ CQE.QueryError
  , search ∷ CSE.SearchError
  , table ∷ CTE.TableError
  , variables ∷ CVE.VariablesError
  )

newtype ResourceError = ResourceError (L.List VM.Resource)

derive newtype instance eqResourceError ∷ Eq ResourceError
derive newtype instance showResourceError ∷ Show ResourceError
derive instance newtypeResourceError ∷ Newtype ResourceError _

_qerror = SProxy ∷ SProxy "qerror"
_stringly = SProxy ∷ SProxy "stringly"
_resource = SProxy ∷ SProxy "resource"
_cache = SProxy ∷ SProxy "cache"
_chart = SProxy ∷ SProxy "chart"
_downloadOptions = SProxy ∷ SProxy "downloadOptions"
_formInputLabeled = SProxy ∷ SProxy "formInputLabeled"
_formInputStatic = SProxy ∷ SProxy "formInputStatic"
_markdown = SProxy ∷ SProxy "markdown"
_open = SProxy ∷ SProxy "open"
_pivotTable = SProxy ∷ SProxy "pivotTable"
_query = SProxy ∷ SProxy "query"
_search = SProxy ∷ SProxy "search"
_table = SProxy ∷ SProxy "table"
_variables = SProxy ∷ SProxy "variables"

showCardError ∷ CardError → String
showCardError =
  case_
    # on _qerror (\err → "(QuasarError " <> show err <> ")")
    # on _stringly (\err → "(StringlyTypedError " <> err <> ")")
    # on _resource (\err → "(ResourceError " <> show err <> ")")
    # on _cache (\err → "(CacheCardError " <> show err <> ")")
    # on _chart (\err → "(ChartCardError " <> show err <> ")")
    # on _downloadOptions (\err → "(DownloadOptionsCardError " <> show err <> ")")
    # on _formInputLabeled (\err → "(FormInputLabeledCardError " <> show err <> ")")
    # on _formInputStatic (\err → "(FormInputStaticCardError " <> show err <> ")")
    # on _markdown (\err → "(MarkdownCardError " <> show err <> ")")
    # on _open (\err → "(OpenCardError " <> show err <> ")")
    # on _pivotTable (\err → "(PivotTableCardError " <> show err <> ")")
    # on _query (\err → "(QueryCardError " <> show err <> ")")
    # on _search (\err → "(SearchCardError " <> show err <> ")")
    # on _table (\err → "(TableCardError " <> show err <> ")")
    # on _variables (\err → "(VariablesCardError " <> show err <> ")")

cardToGlobalError ∷ CardError → Maybe GE.GlobalError
cardToGlobalError =
  case_
    # on _qerror (hush ∘ GE.fromQError)
    # on _stringly (const Nothing)
    # on _resource (const Nothing)
    # on _cache CCE.cacheToGlobalError
    # on _chart CHE.chartToGlobalError
    # on _downloadOptions (const Nothing)
    # on _formInputLabeled (const Nothing)
    # on _formInputStatic (const Nothing)
    # on _markdown (const Nothing)
    # on _open (const Nothing)
    # on _pivotTable CPT.pivotTableToGlobalError
    # on _query CQE.queryToGlobalError
    # on _search CSE.searchToGlobalError
    # on _table CTE.tableToGlobalError
    # on _variables (const Nothing)

-- TODO(Christoph): use this warn constraint to track down unstructured error messages
-- throw ∷ ∀ m a. MonadThrow CardError m ⇒ Warn "You really don't want to" ⇒ String → m a
throw
  ∷ ∀ v m a
  . MonadThrow (Variant (stringly ∷ String | v)) m
  ⇒ String
  → m a
throw = throwVariantError _stringly

throwQError
  ∷ ∀ v m a
  . MonadThrow (Variant (qerror ∷ QError | v)) m
  ⇒ QError
  → m a
throwQError = throwVariantError _qerror

throwResourceError
  ∷ ∀ v m a
  . MonadThrow (Variant (resource ∷ ResourceError | v)) m
  ⇒ ResourceError
  → m a
throwResourceError = throwVariantError _resource

liftQ
  ∷ ∀ m v a
  . MonadThrow (Variant (qerror ∷ QError | v)) m
  ⇒ m (Either QError a)
  → m a
liftQ = flip bind (either throwQError pure)
