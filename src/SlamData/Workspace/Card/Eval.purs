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
  ( EvalStep
  , evalCard
  , stepEval
  , module SlamData.Workspace.Card.Eval.Transition
  ) where

import SlamData.Prelude

import Control.Monad.Eff as Eff
import Control.Monad.Aff.Free (class Affable, fromEff)

import Data.Lens ((^?))
import Data.Map as Map
import Data.Path.Pathy as Path
import Data.StrMap as SM
import Data.Set as Set

import Quasar.Types (SQL, FilePath)

import SlamData.Effects (SlamDataEffects)
import SlamData.FileSystem.Resource as R
import SlamData.GlobalError as GE
import SlamData.Quasar.Error as QE
import SlamData.Quasar.FS as QFS
import SlamData.Quasar.Class (class QuasarDSL)
import SlamData.Quasar.Query as QQ
import SlamData.Workspace.Card.Cache.Eval as Cache
import SlamData.Workspace.Card.Eval.Monad as CEM
import SlamData.Workspace.Card.Eval.Machine (Step(..))
import SlamData.Workspace.Card.Eval.Transition (Eval(..), CardEvalInput, tagEval)
import SlamData.Workspace.Card.Markdown.Component.State.Core as MDS
import SlamData.Workspace.Card.Markdown.Eval as MDE
import SlamData.Workspace.Card.Markdown.Model as MD
import SlamData.Workspace.Card.Port as Port
import SlamData.Workspace.Card.Search.Interpret as Search
import SlamData.Workspace.Card.Variables.Eval as VariablesE
import SlamData.Workspace.Deck.AdditionalSource (AdditionalSource)
import SlamData.Workspace.Card.BuildChart.Metric.Eval as BuildMetric
import SlamData.Workspace.Card.BuildChart.Sankey.Eval as BuildSankey
import SlamData.Workspace.Card.BuildChart.Gauge.Eval as BuildGauge
import SlamData.Workspace.Card.BuildChart.Graph.Eval as BuildGraph
import SlamData.Workspace.Card.BuildChart.Pie.Eval as BuildPie
import SlamData.Workspace.Card.BuildChart.Radar.Eval as BuildRadar
import SlamData.Workspace.Card.BuildChart.Area.Eval as BuildArea
import SlamData.Workspace.Card.BuildChart.Line.Eval as BuildLine
import SlamData.Workspace.Card.BuildChart.Bar.Eval as BuildBar
import SlamData.Workspace.Card.BuildChart.Scatter.Eval as BuildScatter
import SlamData.Workspace.Card.BuildChart.Funnel.Eval as BuildFunnel
import SlamData.Workspace.Card.BuildChart.Heatmap.Eval as BuildHeatmap
import SlamData.Workspace.Card.BuildChart.Boxplot.Eval as BuildBoxplot
import SlamData.Workspace.Card.BuildChart.PivotTable.Eval as BuildPivotTable

import Text.SlamSearch as SS
import Text.Markdown.SlamDown as SD
import Text.Markdown.SlamDown.Halogen.Component.State as SDH

type EvalStep =
  { output ∷ Port.Port
  , sources ∷ Set.Set AdditionalSource
  , next ∷ CEM.EvalMachine
  }

evalCard ∷ CEM.EvalMachine
evalCard arg@(input × eval) =
  case eval, input.input of
    Error msg, _ →
      loop (Port.CardError msg)
    _, Just Port.Blocked →
      loop Port.Blocked
    Pass, Nothing →
      CEM.throw "Card expected an input value"
    Pass, Just port →
      loop port
    Draftboard, _ →
      loop Port.Draftboard
    Query sql, Just (Port.VarMap varMap) →
      loop ∘ Port.TaggedResource
        =<< evalQuery input sql (fromMaybe SM.empty $ Map.lookup (fst input.cardCoord) input.urlVarMaps) varMap
    Query sql, _ →
      loop ∘ Port.TaggedResource
        =<< evalQuery input sql (fromMaybe SM.empty $ Map.lookup (fst input.cardCoord) input.urlVarMaps) Port.emptyVarMap
    Markdown txt, _ →
      loop =<< MDE.markdownEval input txt
    MarkdownForm model, (Just (Port.SlamDown doc)) →
      loop ∘ Port.VarMap =<< evalMarkdownForm doc model
    Search query, Just (Port.TaggedResource { resource }) →
      loop ∘ Port.TaggedResource =<< evalSearch input query resource
    Cache pathString, Just (Port.TaggedResource { resource, varMap }) →
      loop ∘ Port.TaggedResource =<< Cache.eval input pathString resource varMap
    Open res, _ →
      loop ∘ Port.TaggedResource =<< evalOpen input res
    Variables model, _ →
      loop $ Port.VarMap $ VariablesE.eval (fst input.cardCoord) input.urlVarMaps model
    DownloadOptions { compress, options }, Just (Port.TaggedResource { resource }) →
      loop $ Port.DownloadOptions { resource, compress, options }
    BuildMetric model, Just (Port.TaggedResource { resource }) →
      loop =<< BuildMetric.eval model resource
    BuildSankey model, Just (Port.TaggedResource { resource }) →
      loop =<< BuildSankey.eval model resource
    BuildGauge model, Just (Port.TaggedResource { resource }) →
      loop =<< BuildGauge.eval model resource
    BuildGraph model, Just (Port.TaggedResource { resource }) →
      loop =<< BuildGraph.eval model resource
    BuildPie model, Just (Port.TaggedResource { resource }) →
      loop =<< BuildPie.eval model resource
    BuildRadar model, Just (Port.TaggedResource { resource }) →
      loop =<< BuildRadar.eval model resource
    BuildArea model, Just (Port.TaggedResource { resource }) →
      loop =<< BuildArea.eval model resource
    BuildLine model, Just (Port.TaggedResource { resource }) →
      loop =<< BuildLine.eval model resource
    BuildBar model, Just (Port.TaggedResource { resource }) →
      loop =<< BuildBar.eval model resource
    BuildScatter model, Just (Port.TaggedResource { resource }) →
      loop =<< BuildScatter.eval model resource
    BuildFunnel model, Just (Port.TaggedResource { resource }) →
      loop =<< BuildFunnel.eval model resource
    BuildHeatmap model, Just (Port.TaggedResource { resource }) →
      loop =<< BuildHeatmap.eval model resource
    BuildBoxplot model, Just (Port.TaggedResource { resource }) →
      loop =<< BuildBoxplot.eval model resource
    BuildPivotTable _, _ →
      BuildPivotTable.eval arg
    _, _ →
      CEM.unexpectedInput arg
  where
  loop ∷ Port.Port → CEM.CardEval CEM.EvalMachineStep
  loop = pure ∘ flip Step evalCard

evalMarkdownForm
  ∷ Port.VarMap × SD.SlamDownP Port.VarMapValue
  → MD.Model
  → CEM.CardEval Port.VarMap
evalMarkdownForm (vm × doc) model = do
  let inputState = SDH.formStateFromDocument doc
  -- TODO: find a way to smash these annotations if possible -js
  thisVarMap ←
    fromEff (MDS.formStateToVarMap inputState model.state ∷ Eff.Eff SlamDataEffects Port.VarMap)
  pure $ thisVarMap `SM.union` vm

evalOpen
  ∷ CardEvalInput
  → R.Resource
  → CEM.CardEval Port.TaggedResourcePort
evalOpen info res = do
   filePath ←
     maybe (CEM.throw "No resource is selected") pure
       $ res ^? R._filePath
   msg ←
     CEM.liftQ
       $ QFS.messageIfFileNotFound
         filePath
         ("File " ⊕ Path.printPath filePath ⊕ " doesn't exist")
   case msg of
     Nothing → do
       CEM.addSource filePath
       pure { resource: filePath, tag: Nothing, varMap: Nothing }
     Just err →
       CEM.throw err

evalQuery
  ∷ CardEvalInput
  → SQL
  → Port.URLVarMap
  → Port.VarMap
  → CEM.CardEval Port.TaggedResourcePort
evalQuery info sql urlVarMap varMap = do
  let
    varMap' =
      SM.union urlVarMap $ map Port.renderVarMapValue varMap
    resource =
      CEM.temporaryOutputResource info
    backendPath =
      Left $ fromMaybe Path.rootDir (Path.parentDir resource)
  { inputs } ←
    CEM.liftQ $
      lmap (QE.prefixMessage "Error compiling query")
        <$> QQ.compile backendPath sql varMap'
  validateResources inputs
  CEM.addSources inputs
  pure
    { resource
    , tag: pure sql
    , varMap: Just varMap
    }

evalSearch
  ∷ CardEvalInput
  → String
  → FilePath
  → CEM.CardEval Port.TaggedResourcePort
evalSearch info queryText resource = do
  query ← case SS.mkQuery queryText of
    Left _ → CEM.throw "Incorrect query string"
    Right q → pure q

  fields ← CEM.liftQ do
    QFS.messageIfFileNotFound
      resource
      ("Input resource " ⊕ Path.printPath resource ⊕ " doesn't exist")
    QQ.fields resource

  let
    template = Search.queryToSQL fields query
    sql = QQ.templated resource template
    outputResource = CEM.temporaryOutputResource info

  compileResult ← QQ.compile (Right resource) sql SM.empty
  case compileResult of
    Left err →
      case GE.fromQError err of
        Left msg → CEM.throw $ "Error compiling query: " ⊕ msg
        Right _ → CEM.throw $ "Error compiling query: " ⊕ QE.printQError err
    Right { inputs } → do
      validateResources inputs
      CEM.addSources inputs

  pure
    { resource: outputResource
    , tag: pure sql
    , varMap: Nothing
    }

stepEval
  ∷ ∀ m
  . (MonadPar m, QuasarDSL m, Affable SlamDataEffects m)
  ⇒ CardEvalInput
  → Eval
  → CEM.EvalMachine
  → m (Either GE.GlobalError EvalStep)
stepEval input eval machine = do
  res × sources ← CEM.runCardEvalM (machine (input × eval))
  pure case res of
    Left err →
      case GE.fromQError err of
        Left msg → Right { output: Port.CardError msg, sources, next: machine }
        Right ge → Left ge
    Right (Step output next) →
      Right { output, sources, next }

validateResources
  ∷ ∀ f
  . Foldable f
  ⇒ f FilePath
  → CEM.CardEval Unit
validateResources =
  parTraverse_ \path → do
    noAccess ← QFS.fileNotAccessible path
    for_ noAccess \reason →
      CEM.throwError $ QE.prefixMessage ("Resource `" ⊕ Path.printPath path ⊕ "` is unavailable") reason
