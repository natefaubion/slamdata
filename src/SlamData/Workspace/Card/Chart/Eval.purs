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

module SlamData.Workspace.Card.Chart.Eval
  ( eval
  ) where

import SlamData.Prelude

import Control.Monad.State (class MonadState, put)
import Data.Argonaut (Json)
import ECharts.Monad (DSL)
import ECharts.Types.Phantom (OptionI)
import SlamData.Quasar.Class (class QuasarDSL)
import SlamData.Workspace.Card.Error as CE
import SlamData.Workspace.Card.Eval.Common as CEC
import SlamData.Workspace.Card.Eval.Monad as CEM
import SlamData.Workspace.Card.Eval.State as ES
import SlamData.Workspace.Card.Port as Port

eval
  ∷ ∀ m
  . MonadState CEM.CardState m
  ⇒ MonadThrow CE.CardError m
  ⇒ MonadAsk CEM.CardEnv m
  ⇒ QuasarDSL m
  ⇒ (Array Json → DSL OptionI)
  → Port.Resource
  → m Port.Port
eval buildOptions resource = do
  CEM.CardEnv { path } ← ask
  results ← CE.liftQ $ CEC.sampleResource path resource Nothing
  put $ Just $ ES.ChartOptions (buildOptions results)
  pure $ Port.ResourceKey Port.defaultResourceVar
