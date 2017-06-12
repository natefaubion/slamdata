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

module SlamData.Workspace.Card.Table.Eval
  ( eval
  ) where

import SlamData.Prelude

import Control.Monad.State (class MonadState, put, get)
import Data.Lens ((^?), _Just)
import SlamData.Quasar.Class (class QuasarDSL)
import SlamData.Quasar.Query as Quasar
import SlamData.Workspace.Card.Error as CE
import SlamData.Workspace.Card.Eval.Monad as CEM
import SlamData.Workspace.Card.Eval.State as ES
import SlamData.Workspace.Card.Port as Port
import SlamData.Workspace.Card.Table.Model as M

eval
  ∷ ∀ m
  . MonadState CEM.CardState m
  ⇒ MonadAsk CEM.CardEnv m
  ⇒ MonadThrow CE.CardError m
  ⇒ QuasarDSL m
  ⇒ M.Model
  → Port.Port
  → Port.VarMap
  → m Port.Out
eval model port varMap =
  Port.extractResource varMap
    # maybe (CE.throwTableError CE.TableMissingResourceInputError) \resource → do
      rawTableState ← map (_ ^? _Just ∘ ES._Table ) get
      let
        defaultState =
          { resource
          , result: [ ]
          , size: 0
          , page: 1
          , pageSize: 10
          }
        state =
          fromMaybe defaultState rawTableState
        pageSize =
          fromMaybe state.pageSize model.pageSize
        page =
          fromMaybe state.page model.page

      unless (samePageAndResource rawTableState resource model) do
        resultAndSize ← do
          filePath ← CEM.anyTemporaryPath $ Port.filePath resource
          size ←
            case rawTableState of
              Just t | t.resource ≡ resource → pure t.size
              _ → Quasar.count filePath >>= case _ of
                Left err → CE.throwTableError (CE.TableCountQuasarError err)
                Right result → pure result
          sampleResult ← Quasar.sample filePath ((page - 1) * pageSize) pageSize
          case sampleResult of
            Left err → CE.throwTableError (CE.TableSampleQuasarError err)
            Right result →
              pure $ result × size
        put $ Just $ ES.Table
          { resource
          , result: fst resultAndSize
          , size: snd resultAndSize
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
