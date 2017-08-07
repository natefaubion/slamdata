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

module SlamData.Workspace.Card.Setups.Chart.Metric.Eval
  ( eval
  , module SlamData.Workspace.Card.Setups.Chart.Metric.Model
  ) where

import SlamData.Prelude

import Control.Monad.State (class MonadState)
import Data.Argonaut (JArray, Json)
import Data.Array as A
import Data.Formatter.Number as FN
import Data.Lens ((^?), _Just)
import Data.String as Str
import Data.String.Regex as Rgx
import Data.String.Regex.Flags as RXF
import SlamData.Quasar.Class (class QuasarDSL)
import SlamData.Workspace.Card.Error as CE
import SlamData.Workspace.Card.Eval.Monad as CEM
import SlamData.Workspace.Card.Port as Port
import SlamData.Workspace.Card.Setups.Chart.Metric.Model (Model, ModelR)
import SlamData.Workspace.Card.Setups.Common.Eval as BCE
import SlamData.Workspace.Card.Setups.Dimension as D
import SlamData.Workspace.Card.Setups.Semantics (getValues)
import SlamData.Workspace.Card.Setups.Transform as T
import SlamData.Workspace.Card.Setups.Transform.Aggregation as Ag

eval
  ∷ ∀ m
  . MonadState CEM.CardState m
  ⇒ MonadThrow CE.CardError m
  ⇒ MonadAsk CEM.CardEnv m
  ⇒ QuasarDSL m
  ⇒ Model
  → Port.Resource
  → m Port.Port
eval m =
  BCE.analysisEval
    (\_ b c → Port.ValueMetric (buildMetric b c))
    m
    (const m)

buildMetric ∷ ModelR → JArray → Port.MetricPort
buildMetric r records =
  { value, label: r.label }
  where
  formatterRegex ∷ Rgx.Regex
  formatterRegex =
    unsafePartial fromRight $ Rgx.regex "{{[^}]+}}" RXF.noFlags

  value ∷ String
  value = fromMaybe (show metricValue) do
    input ← r.formatter
    matches ← Rgx.match formatterRegex input
    firstMatch ← join $ A.head matches
    woPrefix ← Str.stripPrefix (Str.Pattern "{{") firstMatch
    formatString ← Str.stripSuffix (Str.Pattern "}}") woPrefix
    formatter ← either (const Nothing) Just $ FN.parseFormatString formatString
    let
      formattedNumber = FN.format formatter metricValue

    pure $ Rgx.replace formatterRegex formattedNumber input

  metricValue ∷ Number
  metricValue =
    flip Ag.runAggregation (foldMap foldFn records)
    $ fromMaybe Ag.Sum
    $ r.value ^? D._value ∘ D._transform ∘ _Just ∘ T._Aggregation

  foldFn ∷ Json → Array Number
  foldFn js =
    getValues js $ r.value ^? D._value ∘ D._projection
