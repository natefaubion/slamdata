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
import Control.Monad.State.Class as SC

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
import SlamData.Workspace.Card.Eval.CardEvalT as CET
import SlamData.Workspace.Card.Markdown.Component.State.Core as MDS
import SlamData.Workspace.Card.Markdown.Eval as MDE
import SlamData.Workspace.Card.Markdown.Model as MD
import SlamData.Workspace.Card.Model as CM
import SlamData.Workspace.Card.Port as Port
import SlamData.Workspace.Card.Search.Interpret as Search
import SlamData.Workspace.Card.Variables.Eval as VariablesE

import Text.SlamSearch as SS
import Text.Markdown.SlamDown as SD
import Text.Markdown.SlamDown.Halogen.Component.State as SDH

newtype Eval = Eval CM.AnyCardModel

evalCard
  ∷ ∀ m
  . (Monad m, Affable SlamDataEffects m)
  ⇒ Eval
  → CET.CardEvalT m Port.Port
evalCard (Eval eval) = do
  -- TODO: Move to individial evals
  SC.put eval
  input ← CET.evalInput
  case eval, input.input of
    _, Just (Port.CardError err) →
      pure $ Port.CardError err

    _, Just Port.Blocked →
      pure Port.Blocked

    CM.ErrorCard, _ →
      pure Port.Blocked

    CM.NextAction, _ →
      pure Port.Blocked

    CM.PendingCard, _ →
      pure Port.Blocked

    CM.Chart, Just port →
      pure port

    CM.Download, Just port →
      pure port

    CM.Troubleshoot, Just port →
      pure port

    CM.Table _, Just port →
      pure port

    CM.Draftboard _, _ →
      pure Port.Draftboard

    CM.Ace CT.SQLMode ace, Just (Port.VarMap varMap) →
      Port.TaggedResource <$> evalQuery input (_.text <$> ace) varMap

    CM.Ace CT.SQLMode ace, _ →
      Port.TaggedResource <$> evalQuery input (_.text <$> ace) Port.emptyVarMap

    CM.Ace CT.MarkdownMode ace, _ →
      MDE.markdownEval input (_.text <$> ace)

    CM.Markdown model, Just (Port.SlamDown doc) →
      lift $ Port.VarMap <$> evalMarkdownForm doc model

    CM.Search query, Just (Port.TaggedResource { resource }) →
      Port.TaggedResource <$> evalSearch input query resource

    CM.Cache pathString, Just (Port.TaggedResource { resource }) →
      Port.TaggedResource <$> Cache.eval input pathString resource

    CM.Open Nothing, _ →
      EC.throwError "No resource selected"

    CM.Open (Just res), _ →
      Port.TaggedResource <$> evalOpen input res

    CM.ChartOptions model, _ →
      Port.Chart <$> ChartE.eval input model

    CM.Variables model, _ →
      pure $ Port.VarMap $ VariablesE.eval (fst input.cardCoord) input.urlVarMaps model

    CM.DownloadOptions { compress, options }, Just (Port.TaggedResource { resource }) →
      pure $ Port.DownloadOptions { resource, compress, options }

    _, _ →
      EC.throwError $ "Card received unexpected input type"

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
   filePath ← maybe (EC.throwError "No resource selected") pure $ res ^? R._filePath
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
  → Maybe SQL
  → Port.VarMap
  → CET.CardEvalT m Port.TaggedResourcePort
evalQuery info mbSql varMap = do
  let
    sql = fromMaybe "" mbSql
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
