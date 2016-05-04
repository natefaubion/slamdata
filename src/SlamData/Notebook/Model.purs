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

module SlamData.Notebook.Model
  ( Notebook
  , emptyNotebook
  , encode
  , decode
  , fresh
  , getRoot
  ) where

import SlamData.Prelude

import Control.Monad.Eff.Exception as Exn
import Control.Monad.Aff.Free (class Affable)
import Control.Monad.Except.Trans (ExceptT(..), runExceptT, throwError)

import Data.Argonaut (Json, (:=), (~>), (.?), decodeJson, jsonEmptyObject)

import Quasar.Types (FilePath)

import SlamData.Quasar.Aff (QEff)
import SlamData.Quasar.Data as QD

type Notebook =
  { fresh ∷ Int
  , root ∷ Int
  }

emptyNotebook ∷ Notebook
emptyNotebook = { fresh: 0, root: 0 }

encode ∷ Notebook → Json
encode nb
   = "fresh" := nb.fresh
  ~> "root" := nb.root
  ~> jsonEmptyObject

decode ∷ Json → Either String Notebook
decode = decodeJson >=> \obj ->
  { fresh: _
  , root: _
  } <$> obj .? "fresh"
    <*> obj .? "root"

fresh
  ∷ ∀ eff m
  . (Monad m, Affable (QEff eff) m)
  ⇒ FilePath
  → m (Either Exn.Error Int)
fresh file = do
  json ← QD.load file
  runExceptT $ case json of
    Left _ → bump emptyNotebook
    Right json' → either (throwError ∘ Exn.error) pure (decode json') >>= bump
  where
  bump nb = do
    ExceptT $ QD.save file $ encode $ nb { fresh = nb.fresh + 1 }
    pure nb.fresh

getRoot
  ∷ ∀ eff m
  . (Monad m, Affable (QEff eff) m)
  ⇒ FilePath
  → m (Either String Int)
getRoot file = runExceptT do
  json ← ExceptT $ QD.load file
  nb ← ExceptT $ pure $ decode json
  pure nb.root
