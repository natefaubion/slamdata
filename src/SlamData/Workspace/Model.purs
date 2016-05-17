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

module SlamData.Workspace.Model
  ( Workspace
  , emptyWorkspace
  , encode
  , decode
  , freshId
  , freshRoot
  , getRoot
  , setRoot
  ) where

import SlamData.Prelude

import Control.Monad.Eff.Exception as Exn
import Control.Monad.Aff.Free (class Affable)
import Control.Monad.Except.Trans (ExceptT(..), runExceptT, withExceptT)

import Data.Argonaut (Json, (:=), (~>), (.?), decodeJson, jsonEmptyObject)

import Quasar.Types (FilePath)

import SlamData.Quasar.Aff (QEff)
import SlamData.Quasar.Data as QD

type Workspace =
  { fresh ∷ Int
  , root ∷ Int
  }

emptyWorkspace ∷ Workspace
emptyWorkspace = { fresh: 0, root: 0 }

encode ∷ Workspace → Json
encode ws
   = "fresh" := ws.fresh
  ~> "root" := ws.root
  ~> jsonEmptyObject

decode ∷ Json → Either String Workspace
decode = decodeJson >=> \obj ->
  { fresh: _
  , root: _
  } <$> obj .? "fresh"
    <*> obj .? "root"

fresh
  ∷ ∀ eff m
  . (Monad m, Affable (QEff eff) m)
  ⇒ Boolean -- Should this be the new root
  → FilePath
  → m (Either Exn.Error Int)
fresh isRoot file  = do
  json ← QD.load file
  runExceptT $ case json of
    Left _ → bump emptyWorkspace
    Right json' → liftExn (pure $ decode json') >>= bump
  where
  bump ws = do
    let root =
          if isRoot
            then ws.fresh
            else ws.root
    ExceptT
      $ QD.save file
      $ encode
      $ ws { fresh = ws.fresh + 1, root = root }
    pure ws.fresh

freshId
  ∷ ∀ eff m
  . (Monad m, Affable (QEff eff) m)
  ⇒ FilePath
  → m (Either Exn.Error Int)
freshId = fresh false

freshRoot
  ∷ ∀ eff m
  . (Monad m, Affable (QEff eff) m)
  ⇒ FilePath
  → m (Either Exn.Error Int)
freshRoot = fresh true

getRoot
  ∷ ∀ eff m
  . (Monad m, Affable (QEff eff) m)
  ⇒ FilePath
  → m (Either String Int)
getRoot file = runExceptT do
  json ← ExceptT $ QD.load file
  ws ← ExceptT $ pure $ decode json
  pure ws.root

setRoot
  ∷ ∀ eff m
  . (Monad m, Affable (QEff eff) m)
  ⇒ Int
  → FilePath
  → m (Either Exn.Error Unit)
setRoot root file = runExceptT do
  json ← liftExn $ QD.load file
  ws ← liftExn $ pure $ decode json
  ExceptT
    $ QD.save file
    $ encode
    $ ws { root = root }

liftExn ∷ ∀ m a. (Monad m) ⇒ m (Either String a) → ExceptT Exn.Error m a
liftExn = withExceptT Exn.error ∘ ExceptT
