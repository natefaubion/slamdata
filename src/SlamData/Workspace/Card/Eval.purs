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

module SlamData.Workspace.Card.Eval where

import SlamData.Prelude

import Control.Monad.Eff as Eff
import Control.Monad.Aff.Free (class Affable, fromEff)
import Control.Monad.Eff.Exception as Exn
import Control.Monad.Error.Class as EC

import Data.Lens ((^?))
import Data.Path.Pathy as Path
import Data.StrMap as SM

import Quasar.Types (SQL, FilePath)

import SlamData.Effects (SlamDataEffects)
import SlamData.FileSystem.Resource as R
import SlamData.Quasar.FS as QFS
import SlamData.Quasar.Query as QQ
import SlamData.Workspace.Card.Cache.Eval as Cache
import SlamData.Workspace.Card.CardType as CT
import SlamData.Workspace.Card.ChartOptions.Eval as ChartE
import SlamData.Workspace.Card.ChartOptions.Model as ChartOptions
import SlamData.Workspace.Card.DownloadOptions.Component.State as DO
import SlamData.Workspace.Card.Eval.CardEvalT as CET
import SlamData.Workspace.Card.Markdown.Component.State.Core as MDS
import SlamData.Workspace.Card.Markdown.Eval as MDE
import SlamData.Workspace.Card.Markdown.Model as MD
import SlamData.Workspace.Card.Model as CM
import SlamData.Workspace.Card.Port as Port
import SlamData.Workspace.Card.Search.Interpret as Search
import SlamData.Workspace.Card.Variables.Eval as VariablesE
import SlamData.Workspace.Card.Variables.Model as Variables

import Text.SlamSearch as SS
import Text.Markdown.SlamDown as SD
import Text.Markdown.SlamDown.Halogen.Component.State as SDH

data Eval
  = Pass
  | Query SQL
  | Search String
  | Cache (Maybe String)
  | Error String
  | Markdown String
  | MarkdownForm MD.Model
  | Open (Maybe R.Resource)
  | Variables Variables.Model
  | ChartOptions ChartOptions.Model
  | DownloadOptions DO.State
  | Draftboard

instance showEval ∷ Show Eval where
  show =
    case _ of
      Pass → "Pass"
      Query str → "Query " <> show str
      Search str → "Search " <> show str
      Cache str → "Cache " <> show str
      Error str → "Error " <> show str
      Markdown str → "Markdown " <> show str
      Open res → "Open " <> show res
      MarkdownForm m → "MarkdownForm"
      ChartOptions m → "ChartOptions"
      Variables m → "Variables" -- TODO: I don't have time to write these show instances -js
      DownloadOptions m → "DownloadOptions"
      Draftboard → "Draftboard"

evalCard
  ∷ ∀ m
  . (Monad m, Affable SlamDataEffects m)
  ⇒ Eval
  → CET.CardEvalT m Port.Port
evalCard eval = do
  input ← CET.evalInput
  case eval, input.input of
    Error msg, _ →
      pure $ Port.CardError msg
    _, Just Port.Blocked →
      pure Port.Blocked
    Pass, Nothing →
      EC.throwError "Card expected an input value"
    Pass, Just port →
      pure port
    Draftboard, _ →
      pure Port.Draftboard
    Query sql, Just (Port.VarMap varMap) →
      Port.TaggedResource <$> evalQuery input sql varMap
    Query sql, _ →
      Port.TaggedResource <$> evalQuery input sql Port.emptyVarMap
    Markdown txt, _ →
      MDE.markdownEval input txt
    MarkdownForm model, (Just (Port.SlamDown doc)) →
      lift $ Port.VarMap <$> evalMarkdownForm doc model
    Search query, Just (Port.TaggedResource { resource }) →
      Port.TaggedResource <$> evalSearch input query resource
    Cache pathString, Just (Port.TaggedResource { resource }) →
      Port.TaggedResource <$> Cache.eval input pathString resource
    Open Nothing, _ →
      EC.throwError "No resource is selected"
    Open (Just res), _ →
      Port.TaggedResource <$> evalOpen input res
    ChartOptions model, _ →
      Port.Chart <$> ChartE.eval input model
    Variables model, _ →
      pure $ Port.VarMap $ VariablesE.eval (fst input.cardCoord) input.urlVarMaps model
    DownloadOptions { compress, options }, Just (Port.TaggedResource { resource }) →
      pure $ Port.DownloadOptions { resource, compress, options }
    e, i →
      EC.throwError $ "Card received unexpected input type; " <> show e <> " | " <> show i

evalMarkdownForm
  ∷ ∀ m
  . (Monad m, Affable SlamDataEffects m)
  ⇒ (Port.VarMap × (SD.SlamDownP Port.VarMapValue))
  → MD.Model
  → m Port.VarMap
evalMarkdownForm (vm × doc) model = do
  let inputState = SDH.formStateFromDocument doc
  -- TODO: find a way to smash these annotations if possible -js
  thisVarMap ←
    fromEff (MDS.formStateToVarMap inputState model.state ∷ Eff.Eff SlamDataEffects Port.VarMap)
  pure $ thisVarMap `SM.union` vm

evalOpen
  ∷ ∀ m
  . (Monad m, Affable SlamDataEffects m)
  ⇒ CET.CardEvalInput
  → R.Resource
  → CET.CardEvalT m Port.TaggedResourcePort
evalOpen info res = do
   filePath ← maybe (EC.throwError "No resource is selected") pure $ res ^? R._filePath
   msg ←
     QFS.messageIfFileNotFound
       filePath
       ("File " ⊕ Path.printPath filePath ⊕ " doesn't exist")
     # lift
   case msg of
     Right Nothing → do
       CET.addSource filePath
       pure { resource: filePath, tag: Nothing }
     Right (Just err) →
       EC.throwError err
     Left exn →
       EC.throwError $ Exn.message exn

evalQuery
  ∷ ∀ m
  . (Monad m, Affable SlamDataEffects m)
  ⇒ CET.CardEvalInput
  → SQL
  → Port.VarMap
  → CET.CardEvalT m Port.TaggedResourcePort
evalQuery info sql varMap = do
  let
    varMap' = Port.renderVarMapValue <$> varMap
    resource = CET.temporaryOutputResource info
    backendPath = Left $ fromMaybe Path.rootDir (Path.parentDir resource)
  compileResult ← lift $ QQ.compile backendPath sql varMap'
  case compileResult of
    Left err → EC.throwError $ "Error compiling query: " ⊕ Exn.message err
    Right { inputs } → do
      validateResources inputs
      CET.addSources inputs
  liftQ do
    QQ.viewQuery backendPath resource sql varMap'
    QFS.messageIfFileNotFound resource "Requested collection doesn't exist"
  pure { resource, tag: pure sql }

evalSearch
  ∷ ∀ m
  . (Monad m, Affable SlamDataEffects m)
  ⇒ CET.CardEvalInput
  → String
  → FilePath
  → CET.CardEvalT m Port.TaggedResourcePort
evalSearch info queryText resource = do
  query ← case SS.mkQuery queryText of
    Left _ → EC.throwError "Incorrect query string"
    Right q → pure q

  fields ← liftQ do
    QFS.messageIfFileNotFound
      resource
      ("Input resource " ⊕ Path.printPath resource ⊕ " doesn't exist")
    QQ.fields resource

  let
    template = Search.queryToSQL fields query
    sql = QQ.templated resource template
    outputResource = CET.temporaryOutputResource info

  compileResult ← lift $ QQ.compile (Right resource) sql SM.empty
  case compileResult of
    Left err → EC.throwError $ "Error compiling query: " ⊕ Exn.message err
    Right { inputs } → do
      validateResources inputs
      CET.addSources inputs

  liftQ do
    QQ.viewQuery (Right resource) outputResource template SM.empty
    QFS.messageIfFileNotFound
      outputResource
      "Error making search temporary resource"

  pure { resource: outputResource, tag: pure sql }

liftQ ∷ ∀ m a. Monad m ⇒ m (Either Exn.Error a) → CET.CardEvalT m a
liftQ = either (EC.throwError ∘ Exn.message) pure <=< lift

runEvalCard
  ∷ ∀ m
  . (Monad m, Affable SlamDataEffects m)
  ⇒ CET.CardEvalInput
  → CM.AnyCardModel
  → Eval
  → m CET.CardEvalResult
runEvalCard input model =
  CET.runCardEvalT input model ∘ evalCard

-- TODO: This really needs to be parallel, but we need `MonadPar`.
validateResources
  ∷ ∀ m f
  . (Monad m, Affable SlamDataEffects m, Foldable f)
  ⇒ f FilePath
  → CET.CardEvalT m Unit
validateResources =
  traverse_ \path → do
    noAccess ← lift $ QFS.fileNotAccessible path
    for_ noAccess \reason →
      EC.throwError $ "Resource unavailable: `" ⊕ Path.printPath path ⊕ "`. " ⊕ reason

modelToEval
  ∷ CM.AnyCardModel
  → Eval
modelToEval = case _ of
  CM.Ace CT.SQLMode model → Query $ fromMaybe "" $ _.text <$> model
  CM.Ace CT.MarkdownMode model → Markdown $ fromMaybe "" $ _.text <$> model
  CM.Markdown model → MarkdownForm model
  CM.Search txt → Search txt
  CM.Cache fp → Cache fp
  CM.Open res → Open res
  CM.Variables model → Variables model
  CM.ChartOptions model → ChartOptions model
  CM.DownloadOptions model → DownloadOptions model
  CM.Draftboard _ → Draftboard
  _ → Pass
