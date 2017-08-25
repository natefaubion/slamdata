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

module SlamData.Workspace.Card.Eval.Common where

import SlamData.Prelude

import Control.Monad.Writer.Class (class MonadTell)
import Data.Argonaut as J
import Data.Array as A
import Data.Int as Int
import Data.Json.Extended as E
import Data.Lens ((.~), (?~))
import Data.List as L
import Data.Path.Pathy ((</>))
import Data.Path.Pathy as Path
import Data.StrMap as SM
import Quasar.Advanced.QuasarAF as QF
import Quasar.Data.Json as QJ
import Quasar.Data.Json.Extended (resultsAsEJson)
import Quasar.QuasarF as Q
import Quasar.Types (FilePath, Pagination)
import SlamData.Quasar.Class (class QuasarDSL, class ParQuasarDSL, sequenceQuasar, liftQuasar)
import SlamData.Quasar.Error as QE
import SlamData.Quasar.Query as QQ
import SlamData.Workspace.Card.Error as CE
import SlamData.Workspace.Card.Eval.Monad as CEM
import SlamData.Workspace.Card.Eval.Process as Process
import SlamData.Workspace.Card.Port.VarMap as VM
import SqlSquared as Sql
import Utils.Path (tmpDir, parentDir, DirPath)

validateResources
  ∷ ∀ m t
  . Monad m
  ⇒ QuasarDSL m
  ⇒ ParQuasarDSL m
  ⇒ Traversable t
  ⇒ t FilePath
  → m (Either CE.QError Unit)
validateResources fs = runExceptT do
  res ← sequenceQuasar (map (\path → Tuple path <$> QF.fileMetadata path) fs)
  for_ res case _ of
    path × Left reason →
      throwError $ QE.prefixMessage ("Resource `" ⊕ Path.printPath path ⊕ "` is unavailable") reason
    _ →
      pure unit

localEvalResource
  ∷ ∀ m
  . MonadAsk CEM.CardEnv m
  ⇒ MonadTell CEM.CardLog m
  ⇒ ParQuasarDSL m
  ⇒ Sql.SqlQuery
  → VM.VarMap
  → m (Either QE.QError VM.Resource)
localEvalResource sql varMap = runExceptT do
  CEM.CardEnv { cardId, readOnly } ← ask
  let
    Path.FileName fileName × result = Process.elaborate Path.currentDir parentDir cardId varMap sql
  case result of
    Left sqlQuery → do
      filePath × relFilePath ← lift $ CEM.temporaryOutputResource
      unless readOnly do
        let
          varMap' = VM.unURLVarMapF $ VM.toURLVarMap varMap
          compilePath = fromMaybe Path.rootDir (Path.parentDir filePath)
        { inputs } ← ExceptT $ QQ.compile compilePath sqlQuery varMap'
        ExceptT $ validateResources inputs
        lift $ CEM.addSources inputs
        ExceptT $ QQ.viewQuery filePath sqlQuery varMap'
        ExceptT $ QQ.liftQuasar $ QF.fileMetadata filePath
      pure $ VM.View relFilePath sqlQuery varMap
    Right sqlModule → do
      dirPath × relDirPath ← lift $ CEM.temporaryOutputModule
      -- TODO: Attempt a compile (currently unsupported by backend)
      unless readOnly do
        ExceptT $ QQ.mountModule dirPath sqlModule
      pure $ VM.Process (relDirPath Path.</> Path.file fileName) sqlModule varMap

sampleResource'
  ∷ ∀ m
  . QuasarDSL m
  ⇒ QJ.PrecisionMode
  → DirPath
  → VM.Resource
  → Maybe Pagination
  → m (Either QE.QError J.JArray)
sampleResource' mode path res pagination =
  liftQuasar $ case res of
    VM.Path filePath → QF.readFile mode filePath pagination
    VM.View filePath _ _ → QF.readFile mode (path </> tmpDir </> filePath) pagination
    VM.Process filePath _ varMap → left $ Q.invokeFile mode (path </> tmpDir </> filePath) (VM.unURLVarMapF $ VM.toURLVarMap varMap) pagination

sampleResource
  ∷ ∀ m
  . QuasarDSL m
  ⇒ DirPath
  → VM.Resource
  → Maybe Pagination
  → m (Either QE.QError J.JArray)
sampleResource = sampleResource' QJ.Readable

sampleResourceEJson
  ∷ ∀ m
  . Functor m
  ⇒ QuasarDSL m
  ⇒ DirPath
  → VM.Resource
  → Maybe Pagination
  → m (Either QE.QError (Array E.EJson))
sampleResourceEJson path res pagination =
  resultsAsEJson <$> sampleResource' QJ.Precise path res pagination

runElaboratedQuery'
  ∷ ∀ m
  . QuasarDSL m
  ⇒ QJ.PrecisionMode
  → DirPath
  → Sql.SqlQuery
  → VM.VarMap
  → m (Either QE.QError J.JArray)
runElaboratedQuery' mode path query varMap =
  let query' = Process.elaborateQuery (Path.unsandbox (Path.currentDir </> tmpDir)) varMap query
  in liftQuasar $ QF.readQuery QJ.Readable path (Sql.printQuery query') (VM.unURLVarMapF $ VM.toURLVarMap varMap) Nothing

runElaboratedQuery
  ∷ ∀ m
  . QuasarDSL m
  ⇒ DirPath
  → Sql.SqlQuery
  → VM.VarMap
  → m (Either QE.QError J.JArray)
runElaboratedQuery = runElaboratedQuery' QJ.Readable

runElaboratedQueryEJson
  ∷ ∀ m
  . Functor m
  ⇒ QuasarDSL m
  ⇒ DirPath
  → Sql.SqlQuery
  → VM.VarMap
  → m (Either QE.QError (Array E.EJson))
runElaboratedQueryEJson path query varMap =
  resultsAsEJson <$> runElaboratedQuery' QJ.Precise path query varMap

countResource
  ∷ ∀ m
  . MonadAsk CEM.CardEnv m
  ⇒ QuasarDSL m
  ⇒ VM.Resource
  → m (Either QE.QError Int)
countResource res = do
  CEM.CardEnv { varMap, cardId, path } ← ask
  let varMap' = VM.insert cardId (VM.Var "resource") (VM.Resource res) varMap
  count ← runElaboratedQuery path selectCount varMap'
  pure $ map (fromMaybe 0 ∘ readCount) count

selectCount ∷ Sql.SqlQuery
selectCount =
  Sql.Query mempty
    $ Sql.buildSelect
    $ (Sql._relations ?~ Sql.VariRelation { vari: "resource", alias: Nothing })
    ∘ (Sql._projections .~ L.singleton
        (Sql.as "count"
          $ Sql.projection
          $ Sql.invokeFunction "COUNT"
          $ L.singleton
          $ Sql.splice Nothing))

readCount ∷ J.JArray → Maybe Int
readCount =
  Int.fromNumber
    <=< J.toNumber
    <=< SM.lookup "count"
    <=< J.toObject
    <=< A.head

evalComposite
  ∷ ∀ m
  . MonadAsk CEM.CardEnv m
  ⇒ m VM.VarMap
evalComposite = do
  CEM.CardEnv { varMap, children, cardId } ← ask
  pure $ foldr (\ch → VM.union cardId ch.namespace ch.varMap) varMap children
