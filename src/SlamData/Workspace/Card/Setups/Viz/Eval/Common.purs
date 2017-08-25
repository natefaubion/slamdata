{-
Copyright 2017 SlamData, Inc.

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

module SlamData.Workspace.Card.Setups.Viz.Eval.Common where

import SlamData.Prelude

import SlamData.Workspace.Card.Error as CE
import Control.Monad.State (class MonadState)
import Control.Monad.Writer.Class (class MonadTell)
import Control.Monad.Aff.Class (class MonadAff)
import SlamData.Workspace.Card.Eval.Monad as CEM
import SlamData.Quasar.Class (class ParQuasarDSL)
import SlamData.Effects (SlamDataEffects)

type VizEval m a =
  MonadState CEM.CardState m
  ⇒ MonadThrow CE.CardError m
  ⇒ MonadAsk CEM.CardEnv m
  ⇒ MonadTell CEM.CardLog m
  ⇒ ParQuasarDSL m
  ⇒ MonadAff SlamDataEffects m
  ⇒ a
