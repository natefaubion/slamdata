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

module SlamData.Workspace.Card.Eval
  ( runCard
  , modelToEval
  , module SlamData.Workspace.Card.Eval.Transition
  ) where

import SlamData.Prelude

import Control.Monad.Aff.Class (class MonadAff)
import Control.Monad.State.Class (class MonadState)
import Control.Monad.Writer.Class (class MonadTell)
import Data.StrMap as SM
import Data.List ((:))
import SlamData.Effects (SlamDataEffects)
import SlamData.Quasar.Class (class QuasarDSL, class ParQuasarDSL)
import SlamData.Workspace.Card.Cache.Eval as Cache
import SlamData.Workspace.Card.CardType as CT
import SlamData.Workspace.Card.Chart.Eval as Chart
import SlamData.Workspace.Card.Chart.Model as ChartModel
import SlamData.Workspace.Card.Chart.PivotTableRenderer.Eval as PivotTable
import SlamData.Workspace.Card.Chart.PivotTableRenderer.Model as PTM
import SlamData.Workspace.Card.DownloadOptions.Eval as DOptions
import SlamData.Workspace.Card.Error as CE
import SlamData.Workspace.Card.Eval.Common as Common
import SlamData.Workspace.Card.Eval.Monad as CEM
import SlamData.Workspace.Card.Eval.Transition (Eval(..), tagEval)
import SlamData.Workspace.Card.FormInput.Eval as FormInput
import SlamData.Workspace.Card.Markdown.Eval as MDE
import SlamData.Workspace.Card.Model as Model
import SlamData.Workspace.Card.Open.Eval as Open
import SlamData.Workspace.Card.Port as Port
import SlamData.Workspace.Card.Query.Eval as Query
import SlamData.Workspace.Card.Search.Eval as Search
import SlamData.Workspace.Card.Setups.Chart.Area.Eval as BuildArea
import SlamData.Workspace.Card.Setups.Chart.Bar.Eval as BuildBar
import SlamData.Workspace.Card.Setups.Chart.Boxplot.Eval as BuildBoxplot
import SlamData.Workspace.Card.Setups.Chart.Candlestick.Eval as BuildCandlestick
import SlamData.Workspace.Card.Setups.Chart.Funnel.Eval as BuildFunnel
import SlamData.Workspace.Card.Setups.Chart.Gauge.Eval as BuildGauge
import SlamData.Workspace.Card.Setups.Chart.Graph.Eval as BuildGraph
import SlamData.Workspace.Card.Setups.Chart.Heatmap.Eval as BuildHeatmap
import SlamData.Workspace.Card.Setups.Chart.Line.Eval as BuildLine
import SlamData.Workspace.Card.Setups.Chart.Metric.Eval as BuildMetric
import SlamData.Workspace.Card.Setups.Chart.Parallel.Eval as BuildParallel
import SlamData.Workspace.Card.Setups.Chart.Pie.Eval as BuildPie
import SlamData.Workspace.Card.Setups.Chart.PivotTable.Eval as BuildPivotTable
import SlamData.Workspace.Card.Setups.Chart.PunchCard.Eval as BuildPunchCard
import SlamData.Workspace.Card.Setups.Chart.Radar.Eval as BuildRadar
import SlamData.Workspace.Card.Setups.Chart.Sankey.Eval as BuildSankey
import SlamData.Workspace.Card.Setups.Chart.Scatter.Eval as BuildScatter
import SlamData.Workspace.Card.Setups.FormInput.Date.Eval as SetupDate
import SlamData.Workspace.Card.Setups.FormInput.Datetime.Eval as SetupDatetime
import SlamData.Workspace.Card.Setups.FormInput.Labeled.Eval as SetupLabeled
import SlamData.Workspace.Card.Setups.FormInput.Numeric.Eval as SetupNumeric
import SlamData.Workspace.Card.Setups.FormInput.Static.Eval as SetupStatic
import SlamData.Workspace.Card.Setups.FormInput.Text.Eval as SetupText
import SlamData.Workspace.Card.Setups.FormInput.Time.Eval as SetupTime
import SlamData.Workspace.Card.Table.Eval as Table
import SlamData.Workspace.Card.Variables.Eval as VariablesE

runCard
  ∷ ∀ f m
  . MonadAff SlamDataEffects m
  ⇒ QuasarDSL m
  ⇒ Parallel f m
  ⇒ Monad m
  ⇒ CEM.CardEnv
  → CEM.CardState
  → Eval
  → Port.Port
  → Port.VarMap
  → m (CEM.CardResult CE.CardError Port.Out)
runCard env state trans input varMap =
  CEM.runCardEvalM env state (evalCard trans input varMap ∷ CEM.CardEval CE.CardError Port.Out)

evalCard
  ∷ ∀ m
  . MonadAff SlamDataEffects m
  ⇒ MonadAsk CEM.CardEnv m
  ⇒ MonadState CEM.CardState m
  ⇒ MonadThrow CE.CardError m
  ⇒ MonadTell CEM.CardLog m
  ⇒ QuasarDSL m
  ⇒ ParQuasarDSL m
  ⇒ Eval
  → Port.Port
  → Port.VarMap
  → m Port.Out
evalCard trans port varMap = map (_ `SM.union` varMap) <$> case trans, port of
  Error msg, _ → CE.throw msg
  _, Port.CardError err → throwError err
  Pass, _ → pure (port × varMap)
  Table m, _ → Table.eval m port varMap
  PivotTable m, Port.PivotTable p → PivotTable.eval m p varMap
  Chart, Port.PivotTable p → PivotTable.eval PTM.initialModel p varMap
  Chart, Port.ChartInstructions { options } → tapResource (Chart.eval options) varMap
  Chart, _ → pure (Port.ResourceKey Port.defaultResourceVar × varMap)
  Composite, _ → Port.varMapOut <$> Common.evalComposite
  Terminal, _ → pure Port.terminalOut
  Query sql, _ → Query.evalQuery sql varMap
  Markdown txt, _ → MDE.evalMarkdown txt varMap
  MarkdownForm model, Port.SlamDown doc → MDE.evalMarkdownForm model doc varMap
  Search query, _ → Search.evalSearch query =<< extractResource varMap
  Cache path, _ → Cache.eval path =<< extractResource varMap
  Open res, _ → Open.evalOpen res varMap
  Variables model, _ → VariablesE.eval model
  BuildMetric model, _ → tapResource (BuildMetric.eval model) varMap
  BuildSankey model, _ → BuildSankey.eval model =<< extractResource varMap
  BuildGauge model, _ → BuildGauge.eval model =<< extractResource varMap
  BuildGraph model, _ → BuildGraph.eval model =<< extractResource varMap
  BuildPie model, _ → BuildPie.eval model =<< extractResource varMap
  BuildRadar model, _ → BuildRadar.eval model =<< extractResource varMap
  BuildArea model, _ → BuildArea.eval model =<< extractResource varMap
  BuildLine model, _ → BuildLine.eval model =<< extractResource varMap
  BuildBar model, _ → BuildBar.eval model =<< extractResource varMap
  BuildScatter model, _ → BuildScatter.eval model =<< extractResource varMap
  BuildFunnel model, _ → BuildFunnel.eval model =<< extractResource varMap
  BuildHeatmap model, _ → BuildHeatmap.eval model =<< extractResource varMap
  BuildBoxplot model, _ → BuildBoxplot.eval model =<< extractResource varMap
  BuildPivotTable model, _ → BuildPivotTable.eval model varMap =<< extractResource varMap
  BuildPunchCard model, _ → BuildPunchCard.eval model =<< extractResource varMap
  BuildCandlestick model, _ → BuildCandlestick.eval model =<< extractResource varMap
  BuildParallel model, _ → BuildParallel.eval model =<< extractResource varMap
  SetupCheckbox model, _ → tapResource (SetupLabeled.eval model CT.Checkbox) varMap
  SetupRadio model, _ → tapResource (SetupLabeled.eval model CT.Radio) varMap
  SetupDropdown model, _ → tapResource (SetupLabeled.eval model CT.Dropdown) varMap
  SetupText model, _ → tapResource (SetupText.eval model) varMap
  SetupNumeric model, _ → tapResource (SetupNumeric.eval model) varMap
  SetupDate model, _ → tapResource (SetupDate.eval model) varMap
  SetupTime model, _ → tapResource (SetupTime.eval model) varMap
  SetupDatetime model, _ → tapResource (SetupDatetime.eval model) varMap
  SetupStatic model, _ → tapResource (SetupStatic.eval model) varMap
  FormInput (FormInput.Labeled model), Port.SetupLabeledFormInput lp →
    FormInput.evalLabeled model lp =<< extractResource varMap
  FormInput (FormInput.TextLike model), Port.SetupTextLikeFormInput tlp →
    FormInput.evalTextLike model tlp =<< extractResource varMap
  FormInput _, _ → pure (Port.ResourceKey Port.defaultResourceVar × varMap)
  DownloadOptions model, _ → tapResource (DOptions.eval model) varMap
  e, i → CE.throw $ "Card received unexpected input type; " <> tagEval e <> " | " <> Port.tagPort i

modelToEval ∷ Model.AnyCardModel → Eval
modelToEval = case _ of
  Model.Ace CT.SQLMode model → Query model.text
  Model.Ace CT.MarkdownMode model → Markdown model.text
  Model.Markdown model → MarkdownForm model
  Model.Search txt → Search txt
  Model.Cache fp → Cache fp
  Model.Open res → Open res
  Model.Variables model → Variables model
  Model.DownloadOptions model → DownloadOptions model
  Model.Draftboard _ → Composite
  Model.BuildMetric model  → BuildMetric model
  Model.BuildSankey model → BuildSankey model
  Model.BuildGauge model → BuildGauge model
  Model.BuildGraph model → BuildGraph model
  Model.BuildPie model → BuildPie model
  Model.BuildRadar model → BuildRadar model
  Model.BuildArea model → BuildArea model
  Model.BuildLine model → BuildLine model
  Model.BuildBar model → BuildBar model
  Model.BuildScatter model → BuildScatter model
  Model.BuildFunnel model → BuildFunnel model
  Model.BuildHeatmap model → BuildHeatmap model
  Model.BuildBoxplot model → BuildBoxplot model
  Model.BuildPivotTable model → BuildPivotTable model
  Model.BuildPunchCard model → BuildPunchCard model
  Model.BuildCandlestick model → BuildCandlestick model
  Model.BuildParallel model → BuildParallel model
  Model.Chart (Just (ChartModel.PivotTableRenderer model)) → PivotTable model
  Model.Chart Nothing → Chart
  Model.SetupDropdown model → SetupDropdown model
  Model.SetupStatic model → SetupStatic model
  Model.SetupText model → SetupText model
  Model.SetupNumeric model → SetupNumeric model
  Model.SetupCheckbox model → SetupCheckbox model
  Model.SetupRadio model → SetupRadio model
  Model.SetupDate model → SetupDate model
  Model.SetupTime model → SetupTime model
  Model.SetupDatetime model → SetupDatetime model
  Model.FormInput model → FormInput model
  Model.Tabs _ → Terminal
  Model.Table model → Table model
  _ → Pass

-- TODO(Christoph): Get rid of this monstrosity of an error message
extractResourceVar ∷ ∀ m. MonadThrow CE.CardError m ⇒ Port.VarMap → m (String × Port.Resource)
extractResourceVar dm = case SM.toUnfoldable (Port.filterResources dm) of
  _ : _ : _ → CE.throw "Multiple resources selected"
  r : _ → pure r
  _ → CE.throw "No resource selected"

extractResource ∷ ∀ m. MonadThrow CE.CardError m ⇒ Port.VarMap → m (Port.Resource)
extractResource = map snd ∘ extractResourceVar

tapResource ∷ ∀ m. MonadThrow CE.CardError m ⇒ (Port.Resource → m Port.Port) → Port.VarMap → m Port.Out
tapResource f dm = map (_ × dm) (f =<< extractResource dm)
