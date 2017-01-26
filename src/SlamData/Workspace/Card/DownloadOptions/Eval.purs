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

module SlamData.Workspace.Card.DownloadOptions.Eval where

import SlamData.Prelude
import Control.Monad.Throw (class MonadThrow)
import Data.Lens ((^.))
import SlamData.Download.Model as D
import SlamData.Workspace.Card.DownloadOptions.Component.State as Download
import SlamData.Workspace.Card.Eval.Monad as CEM
import SlamData.Workspace.Card.Port as Port

eval ∷ ∀ m. MonadThrow CEM.CardError m => Download.State → Port.Resource → m Port.Port
eval { compress, options, targetName } resource = case targetName of
  Nothing → required
  Just "" → required
  Just fn → do
    when (isLeft (D.validFilename fn)) do
      CEM.throw $ "Invalid target filename: " <> fn
    pure $ Port.DownloadOptions
      { compress
      , options
      , targetName: fn
      , resource: resource ^. Port._filePath
      }

  where
    required = CEM.throw "Target filename required"
