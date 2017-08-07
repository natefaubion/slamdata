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

module SlamData.Workspace.Card.Setups.FormInput.TextLike.Eval
  ( eval
  , module SlamData.Workspace.Card.Setups.FormInput.TextLike.Model
  ) where

import SlamData.Prelude

import Control.Monad.State (class MonadState)
import Data.Argonaut (JCursor)
import Data.Set as Set
import SlamData.Quasar.Class (class QuasarDSL)
import SlamData.Workspace.Card.CardType.FormInputType (FormInputType)
import SlamData.Workspace.Card.Error as CE
import SlamData.Workspace.Card.Eval.Monad as CEM
import SlamData.Workspace.Card.Port as Port
import SlamData.Workspace.Card.Setups.Axis as Ax
import SlamData.Workspace.Card.Setups.Behaviour as B
import SlamData.Workspace.Card.Setups.Common.Eval as BCE
import SlamData.Workspace.Card.Setups.FormInput.TextLike.Model (Model, behaviour, initialState)

eval
  ∷ ∀ m
  . MonadState CEM.CardState m
  ⇒ MonadThrow CE.CardError m
  ⇒ MonadAsk CEM.CardEnv m
  ⇒ QuasarDSL m
  ⇒ ( Ax.Axes → Set.Set JCursor )
  → FormInputType
  → Model
  → Port.Resource
  → m Port.Port
eval valueProjection formInputType m =
  BCE.analysisEval buildFn m modelFromAxes
  where
  -- We need to store axes to display selects
  buildFn axes conf records =
    Port.SetupTextLikeFormInput
      { name: conf.name
      , formInputType
      , cursor: conf.value
      }
  modelFromAxes axes =
    B.defaultModel (behaviour valueProjection) m initialState{axes = axes}
