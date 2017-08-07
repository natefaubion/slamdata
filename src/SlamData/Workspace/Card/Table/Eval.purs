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

module SlamData.Workspace.Card.Table.Eval (eval) where

import SlamData.Prelude

import Control.Monad.State (class MonadState, put, gets)
import Data.Lens ((^?), _Just)
import SlamData.Quasar.Class (class QuasarDSL)
import SlamData.Workspace.Card.Eval.Common as CEC
import SlamData.Workspace.Card.Error as CE
import SlamData.Workspace.Card.Eval.Monad as CEM
import SlamData.Workspace.Card.Eval.State as ES
import SlamData.Workspace.Card.Port as Port
import SlamData.Workspace.Card.Table.Error (TableError(..), throwTableError)
import SlamData.Workspace.Card.Table.Model as M

eval
  ∷ ∀ m v
  . MonadState CEM.CardState m
  ⇒ MonadAsk CEM.CardEnv m
  ⇒ MonadThrow (Variant (table ∷ TableError, resource ∷ CE.ResourceError | v)) m
  ⇒ QuasarDSL m
  ⇒ M.Model
  → Port.Port
  → m Port.Out
eval model port = do
  CEM.CardEnv { varMap, path } ← ask
  resource ← CEM.extractResource port
  rawState ← gets (_ ^? _Just ∘ ES._Table)
  let
    defaultState =
      { resource
      , result: []
      , size: 0
      , page: 1
      , pageSize: 10
      }
    state = fromMaybe defaultState rawState
    pageSize = fromMaybe state.pageSize model.pageSize
    page = fromMaybe state.page model.page
    offset = (page - 1) * pageSize
  unless (samePageAndResource rawState resource model) do
    size ←
      case rawState of
        Just t | t.resource ≡ resource → pure t.size
        _ → CEC.countResource resource >>= searchError TableCountQuasarError
    result ←
      CEC.sampleResource path resource (Just { offset, limit: pageSize })
        >>= searchError TableSampleQuasarError
    put $ Just $ ES.Table
      { resource
      , result
      , size
      , page
      , pageSize
      }
  pure $ port × varMap


  where
  samePageAndResource ∷ Maybe ES.TableR → Port.Resource → M.Model → Boolean
  samePageAndResource Nothing _ _ = false
  samePageAndResource (Just t) resource {page, pageSize} =
    t.resource ≡ resource
    ∧ Just t.page ≡ page
    ∧ Just t.pageSize ≡ pageSize

searchError ∷ ∀ e a m v. MonadThrow (Variant (table ∷ TableError | v)) m ⇒ (e → TableError) → Either e a → m a
searchError f = either (throwTableError ∘ f) pure
