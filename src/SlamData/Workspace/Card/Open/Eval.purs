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

module SlamData.Workspace.Card.Open.Eval (evalOpen) where

import SlamData.Prelude

import Control.Monad.Writer.Class (class MonadTell)
import Data.Lens ((?~))
import Data.List as L
import Data.Path.Pathy as Path
import SlamData.FileSystem.Resource as R
import SlamData.Quasar.Class (class ParQuasarDSL)
import SlamData.Quasar.FS as QFS
import SlamData.Workspace.Card.Error as CE
import SlamData.Workspace.Card.Eval.Common as CEC
import SlamData.Workspace.Card.Eval.Monad as CEM
import SlamData.Workspace.Card.Open.Error (OpenError(..), throwOpenError)
import SlamData.Workspace.Card.Open.Model as Open
import SlamData.Workspace.Card.Port as Port
import SlamData.Workspace.Card.Port.VarMap as VM
import SqlSquared as Sql
import Utils.SqlSquared (all, selectStar)
import Utils.Path (dirPathAsFilePath)

evalOpen
  ∷ ∀ m v
  . MonadThrow (Variant (open ∷ OpenError, qerror ∷ CE.QError, stringly ∷ String | v)) m
  ⇒ MonadTell CEM.CardLog m
  ⇒ MonadAsk CEM.CardEnv m
  ⇒ ParQuasarDSL m
  ⇒ Open.Model
  → Port.VarMap
  → m Port.Out
evalOpen model varMap = case model of
  Nothing → throwOpenError OpenNoResourceSelected
  Just (Open.Resource res) →
    either selectStarDirPathOut filePathOut $ R.getPath res
  Just (Open.Variable (VM.Var var)) → do
    CEM.CardEnv { cardId, path } ← ask
    let
      body =
        Sql.buildSelect
          $ all
          ∘ (Sql._relations ?~ Sql.VariRelation { vari: var, alias: Nothing })
    CEM.resourceOut =<< CE.liftQ (CEC.localEvalResource (Sql.Query mempty body) varMap)

  where
  checkPath filePath =
    CE.liftQ $ QFS.messageIfFileNotFound filePath $ OpenFileNotFound (Path.printPath filePath)

  selectStarDirPathOut dirPath = do
    filePath ← maybe (throwOpenError OpenNoFileSelected) pure $ dirPathAsFilePath dirPath
    let query = Sql.Query L.Nil $ selectStar filePath
    let err = openError (const $ OpenFileNotFound $ Path.printPath filePath)
    resource ← CEC.localEvalResource query varMap >>= err
    CEM.resourceOut resource

  filePathOut filePath =
    checkPath filePath >>= case _ of
      Nothing → do
        CEM.addSource filePath
        CEM.resourceOut (Port.Path filePath)
      Just err → throwOpenError err

openError ∷ ∀ e a m v. MonadThrow (Variant (open ∷ OpenError | v)) m ⇒ (e → OpenError) → Either e a → m a
openError e = either throwOpenError pure ∘ lmap e
