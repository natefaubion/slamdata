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

module SlamData.FileSystem.Dialog.Mount.Mimir.Component
  ( component
  , Query
  , module Q
  , module S
  ) where

import SlamData.Prelude

import Halogen as H
import Halogen.HTML as HH
import Quasar.Mount.Mimir as QMM
import SlamData.FileSystem.Dialog.Mount.Common.Render as MCR
import SlamData.FileSystem.Dialog.Mount.Common.SettingsQuery as Q
import SlamData.FileSystem.Dialog.Mount.Mimir.Component.State as S
import SlamData.Monad (Slam)

type Query = Q.SettingsQuery S.State
type Message = Q.SettingsMessage QMM.Config

component ∷ H.Component HH.HTML Query (Maybe QMM.Config) Message Slam
component =
  H.component
    { initialState: maybe S.initialState S.fromConfig
    , render
    , eval: Q.eval S.toConfig
    , receiver: const Nothing
    }

render ∷ S.State → H.ComponentHTML Query
render state =
  HH.div_
    [ MCR.label "Path" [ MCR.input state id [] ] ]
