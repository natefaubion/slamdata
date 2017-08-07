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

import Data.Path.Pathy (runFileName)
import SlamData.Download.Model as D
import SlamData.Workspace.Card.DownloadOptions.Component.State as Download
import SlamData.Workspace.Card.DownloadOptions.Error (DownloadOptionsError(..), throwDownloadOptionsError)
import SlamData.Workspace.Card.Port as Port
import Utils.Path as PU

eval
  ∷ ∀ m v
  . MonadThrow (Variant (downloadOptions ∷ DownloadOptionsError | v)) m
  ⇒ Download.State
  → Port.Resource
  → m Port.Port
eval { compress, options, targetName } resource = case targetName of
  Nothing →
    -- For legacy download options. Otherwise this shouldn't be Nothing.
    pure $ Port.DownloadOptions
      { compress
      , options
      , targetName: runFileName (PU.anyFileName (Port.filePath resource))
      , resource
      }
  Just "" → throwDownloadOptionsError DownloadOptionsFilenameRequired
  Just fn → do
    when (isLeft (D.validFilename fn)) do
      throwDownloadOptionsError (DownloadOptionsFilenameInvalid fn)
    pure $ Port.DownloadOptions
      { compress
      , options
      , targetName: fn
      , resource
      }
