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

module SlamData.Workspace.Card.Setups.FormInput.Static.Eval
  ( eval
  , module SlamData.Workspace.Card.Setups.FormInput.Static.Model
  ) where

import SlamData.Prelude

import Control.Monad.State (class MonadState, get, put)
import Data.Array as Arr
import SlamData.Quasar.Class (class QuasarDSL)
import SlamData.Workspace.Card.Error as CE
import SlamData.Workspace.Card.Eval.Monad as CEM
import SlamData.Workspace.Card.Port as Port
import SlamData.Workspace.Card.Setups.Behaviour as B
import SlamData.Workspace.Card.Setups.Common.Eval as BCE
import SlamData.Workspace.Card.Setups.FormInput.Static.Error (FormInputStaticError(..), throwFormInputStaticError)
import SlamData.Workspace.Card.Setups.FormInput.Static.Model (Model, behaviour, initialState)
import SlamData.Workspace.Card.Setups.Semantics as Sem
import Utils (showPrettyJCursor)

eval
  ∷ ∀ m v
  . MonadState CEM.CardState m
  ⇒ MonadThrow (Variant (qerror ∷ CE.QError, formInputStatic ∷ FormInputStaticError | v)) m
  ⇒ MonadAsk CEM.CardEnv m
  ⇒ QuasarDSL m
  ⇒ Model
  → Port.Resource
  → m Port.Port
eval m resource = do
  records × axes ← BCE.analyze resource =<< get
  put (Just (CEM.Analysis { resource, records, axes }))
  case m <|> B.defaultModel behaviour m initialState{axes = axes} of
    Nothing →
      throwFormInputStaticError FIStaticNoAxis
    Just conf → case Arr.head records >>= flip Sem.getMaybeString conf.value of
      Nothing →
        throwFormInputStaticError (FIStaticMissingAxis (showPrettyJCursor conf.value))
      Just value →
        pure $ Port.CategoricalMetric { value, label: Nothing }
