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
  , save
  , load
  , delete
  ) where

import SlamData.Prelude

import Control.Monad.Eff.Exception as Exn
import Control.Monad.Aff.Free (class Affable)

import Data.Argonaut as JS

import Quasar.Advanced.QuasarAF as QF
import Quasar.Data (QData(..), JSONMode(..))
import Quasar.Error (lowerQError)
import Quasar.Types (FilePath, AnyPath)

import SlamData.Quasar.Aff (QEff, runQuasarF)

makeFile
  ∷ ∀ eff m
  . Affable (QEff eff) m
  ⇒ FilePath
  → QData
  → m (Either Exn.Error Unit)
makeFile path content =
  runQuasarF $ lmap lowerQError <$>
    QF.writeFile path content

-- | Saves a single JSON value to a file.
-- |
-- | Even though the path is expected to be absolute it should not include the
-- | `/data/fs` part of the path for the API.
save
  ∷ ∀ eff m
  . Affable (QEff eff) m
  ⇒ FilePath
  → JS.Json
  → m (Either Exn.Error Unit)
save path json =
  runQuasarF $ lmap lowerQError <$>
    QF.writeFile path (JSON Readable [json])

-- | Loads a single JSON value from a file.
-- |
-- | Even though the path is expected to be absolute it should not include the
-- | `/data/fs` part of the path for the API.
load
  ∷ ∀ eff m
  . (Functor m, Affable (QEff eff) m)
  ⇒ FilePath
  → m (Either String JS.Json)
load file =
  runQuasarF (QF.readFile Readable file Nothing) <#> case _ of
    Right [file] → Right file
    Right _ → Left "Unexpected result when loading value from file"
    Left err → Left (QF.printQError err)

delete
  ∷ ∀ eff m
  . (Functor m, Affable (QEff eff) m)
  ⇒ AnyPath
  → m (Either Exn.Error Unit)
delete path =
  runQuasarF $ lmap lowerQError <$>
    QF.deleteData path
