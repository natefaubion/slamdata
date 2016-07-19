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
import SlamData.Workspace.Deck.DeckId (DeckId)

type Workspace =
  { root ∷ Maybe DeckId
  }

emptyWorkspace ∷ Workspace
emptyWorkspace = { root: Nothing }

encode ∷ Workspace → Json
encode ws
   = "root" := ws.root
  ~> jsonEmptyObject

decode ∷ Json → Either String Workspace
decode = decodeJson >=> \obj ->
  { root: _
  } <$> obj .? "root"

getRoot
  ∷ ∀ eff m
  . (Monad m, Affable (QEff eff) m)
  ⇒ FilePath
  → m (Either String DeckId)
getRoot file = runExceptT do
  json ← ExceptT $ QD.load file
  ws ← ExceptT $ pure $ decode json
  maybe (ExceptT $ pure $ Left "No root") pure ws.root

setRoot
  ∷ ∀ eff m
  . (Monad m, Affable (QEff eff) m)
  ⇒ FilePath
  → DeckId
  → m (Either String Unit)
setRoot file root =
  map (lmap Exn.message)
    $ QD.save file
        $ encode
        $ { root: Just root }

liftExn ∷ ∀ m a. (Monad m) ⇒ m (Either String a) → ExceptT Exn.Error m a
liftExn = withExceptT Exn.error ∘ ExceptT
