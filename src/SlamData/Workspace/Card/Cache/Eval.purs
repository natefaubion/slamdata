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

module SlamData.Workspace.Card.Cache.Eval where

import SlamData.Prelude

import Control.Monad.Writer.Class (class MonadTell)
import Data.Lens ((.~))
import Data.Path.Pathy as Path
import Data.StrMap as SM
import Quasar.Types (FilePath)
import SlamData.Quasar.Class (class QuasarDSL)
import SlamData.Quasar.FS as QFS
import SlamData.Quasar.Query as QQ
import SlamData.Workspace.Card.Cache.Error (CacheError(..), throwCacheError)
import SlamData.Workspace.Card.Error as CE
import SlamData.Workspace.Card.Eval.Monad as CEM
import SlamData.Workspace.Card.Port as Port
import SqlSquared as Sql
import Utils.Path as PU
import Utils.SqlSquared (tableRelation, all)

eval
  ∷ ∀ m v
  . MonadAsk CEM.CardEnv m
  ⇒ MonadThrow (Variant (cache ∷ CacheError, qerror ∷ CE.QError | v)) m
  ⇒ MonadTell CEM.CardLog m
  ⇒ QuasarDSL m
  ⇒ Maybe String
  → Port.Resource
  → m Port.Out
eval mfp resource =
  CEM.resourceOut =<< case mfp, resource of
    _, Port.Process _ _ _ → pure resource
    Nothing, _ → do
      tmp ← fst <$> CEM.temporaryOutputResource
      eval' tmp resource
    Just pt, _ →
      case PU.parseAnyPath pt of
        Just (Right fp) → eval' fp resource
        _ → throwCacheError (CacheInvalidFilepath pt)

eval'
  ∷ ∀ m v
  . MonadThrow (Variant (cache ∷ CacheError, qerror ∷ CE.QError | v)) m
  ⇒ MonadTell CEM.CardLog m
  ⇒ MonadAsk CEM.CardEnv m
  ⇒ QuasarDSL m
  ⇒ FilePath
  → Port.Resource
  → m Port.Resource
eval' tmp resource = do
  let
    anyPath = Port.filePath resource
    sql =
      Sql.buildSelect
        $ all
        ∘ (Sql._relations .~ tableRelation anyPath)
  backendPath ← fromMaybe Path.rootDir ∘ Path.parentDir <$> CEM.anyTemporaryPath anyPath
  outputResource ← CE.liftQ $
    QQ.fileQuery backendPath tmp (Sql.Query mempty sql) SM.empty
  checkResult ← QFS.messageIfFileNotFound outputResource (CacheErrorSavingFile outputResource)
  for_ (either (Just ∘ CacheQuasarError) id checkResult)
    throwCacheError
  when (tmp /= outputResource) $
    throwCacheError CacheResourceNotModified
  CEM.addCache outputResource
  pure (Port.Path outputResource)
