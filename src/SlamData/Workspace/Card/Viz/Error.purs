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

module SlamData.Workspace.Card.Viz.Error where

import SlamData.Prelude

import Quasar.QuasarF (QError)
import SlamData.GlobalError as GE
import Utils (throwVariantError)

data ChartError
  = ChartMissingResourceInputError
  | ChartCountQuasarError QError
  | ChartSampleQuasarError QError

instance showChartError ∷ Show ChartError where
  show = case _ of
    ChartMissingResourceInputError → "ChartMissingResourceInputError"
    ChartCountQuasarError err → "(ChartCountQuasarError " <> show err <> ")"
    ChartSampleQuasarError err → "(ChartSampleQuasarError " <> show err <> ")"

chartToGlobalError ∷ ChartError → Maybe GE.GlobalError
chartToGlobalError = case _ of
  ChartCountQuasarError qErr → hush (GE.fromQError qErr)
  ChartSampleQuasarError qErr → hush (GE.fromQError qErr)
  _ → Nothing

throwChartError ∷ forall v m a. MonadThrow (Variant (chart ∷ ChartError | v)) m ⇒ ChartError → m a
throwChartError = throwVariantError (SProxy :: SProxy "chart")
