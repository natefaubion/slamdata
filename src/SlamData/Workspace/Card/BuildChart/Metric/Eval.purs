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

module SlamData.Workspace.Card.BuildChart.Metric.Eval
  ( eval
  , module SlamData.Workspace.Card.BuildChart.Metric.Model
  ) where

import SlamData.Prelude

import Data.Argonaut (JArray, Json)
import Data.Array as A
import Data.Formatter.Number as FN
import Data.String as Str
import Data.String.Regex as Rgx
import Data.Lens ((^?))

import Quasar.Types (FilePath)

import SlamData.Quasar.Class (class QuasarDSL)
import SlamData.Quasar.Error as QE
import SlamData.Workspace.Card.BuildChart.Common.Eval as BCE
import SlamData.Workspace.Card.Eval.CardEvalT as CET
import SlamData.Workspace.Card.Port as Port
import SlamData.Workspace.Card.BuildChart.Metric.Model (Model, MetricR)
import SlamData.Workspace.Card.BuildChart.Semantics (getValues)
import SlamData.Workspace.Card.BuildChart.Aggregation as Ag
import SlamData.Workspace.Card.BuildChart.Axis as Ax

eval
  ∷ ∀ m
  . (Monad m, QuasarDSL m)
  ⇒ Model
  → FilePath
  → CET.CardEvalT m Port.Port
eval Nothing _  =
  QE.throw "Please select axis to aggregate"
eval (Just conf) resource = do
  records ← BCE.records resource
  let axes = Ax.buildAxes (A.take 100 records)
  pure $ Port.Metric $ buildMetric axes conf records


buildMetric ∷ Ax.Axes → MetricR → JArray → Port.MetricPort
buildMetric axes r records =
  { value, label: r.label, axes }
  where
  formatterRegex ∷ Rgx.Regex
  formatterRegex =
    unsafePartial fromRight $ Rgx.regex "{{[^}]+}}" Rgx.noFlags

  value ∷ String
  value = fromMaybe (show metricValue) do
    input ← r.formatter
    matches ← Rgx.match formatterRegex input
    firstMatch ← join $ A.head matches
    woPrefix ← Str.stripPrefix "{{" firstMatch
    formatString ← Str.stripSuffix "}}" woPrefix
    formatter ← either (const Nothing) Just $ FN.parseFormatString formatString
    let
      formattedNumber = FN.format formatter metricValue

    pure $ Rgx.replace formatterRegex formattedNumber input

  metricValue ∷ Number
  metricValue =
    Ag.runAggregation r.valueAggregation $ foldMap foldFn records

  foldFn ∷ Json → Array Number
  foldFn js =
    getValues js $ pure r.value
