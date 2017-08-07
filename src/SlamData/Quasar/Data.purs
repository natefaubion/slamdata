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

module SlamData.Quasar.Data
  ( makeFile
  , makeDir
  , save
  , load
  , delete
  ) where

import SlamData.Prelude

import DOM.File.Types (Blob)
import Data.Argonaut.Core as J
import Quasar.Advanced.QuasarAF as QF
import Quasar.Data (QData(..))
import Quasar.Data.Json as QJ
import Quasar.Error (QError)
import Quasar.Types (AnyPath, DirPath, FilePath)
import SlamData.Quasar.Class (class QuasarDSL, liftQuasar)
import SlamData.Quasar.Error (msgToQError)

makeFile
  ∷ ∀ m
  . QuasarDSL m
  ⇒ FilePath
  → QData
  → m (Either QError Unit)
makeFile path = liftQuasar ∘ QF.writeFile path

makeDir
  ∷ ∀ m
  . QuasarDSL m
  ⇒ DirPath
  → Blob
  → m (Either QError Unit)
makeDir path = liftQuasar ∘ QF.writeDir path

-- | Saves a single JSON value to a file.
-- |
-- | Even though the path is expected to be absolute it should not include the
-- | `/data/fs` part of the path for the API.
save
  ∷ ∀ m
  . QuasarDSL m
  ⇒ FilePath
  → J.Json
  → m (Either QError Unit)
save path json =
  let
    options = Left (QJ.Options { precision: QJ.Readable, encoding: QJ.Array })
    content = J.stringify (J.fromArray [json])
  in
    liftQuasar $ QF.writeFile path (QData options content)

-- | Loads a single JSON value from a file.
-- |
-- | Even though the path is expected to be absolute it should not include the
-- | `/data/fs` part of the path for the API.
load
  ∷ ∀ m
  . Functor m
  ⇒ QuasarDSL m
  ⇒ FilePath
  → m (Either QError J.Json)
load path =
  liftQuasar (QF.readFile QJ.Readable path Nothing) <#> case _ of
    Right [file] → Right file
    Right _ → throwError $ msgToQError "Unexpected result when loading value from file"
    Left err → Left err

delete
  ∷ ∀ m
  . Functor m
  ⇒ QuasarDSL m
  ⇒ AnyPath
  → m (Either QError Unit)
delete = liftQuasar ∘ QF.deleteData
