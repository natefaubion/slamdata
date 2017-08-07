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

module SlamData.FileSystem.Dialog.Mount.MongoDB.Component
  ( component
  , Query
  , module Q
  , module S
  ) where

import SlamData.Prelude

import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Quasar.Mount.MongoDB as QMM
import SlamData.FileSystem.Dialog.Mount.Common.Render as MCR
import SlamData.FileSystem.Dialog.Mount.Common.SettingsQuery as Q
import SlamData.FileSystem.Dialog.Mount.Common.State as MCS
import SlamData.FileSystem.Dialog.Mount.MongoDB.Component.State as S
import SlamData.Monad (Slam)
import SlamData.Render.ClassName as CN

type Query = Q.SettingsQuery S.State
type Message = Q.SettingsMessage QMM.Config

component ∷ H.Component HH.HTML Query (Maybe QMM.Config) Message Slam
component =
  H.component
    { initialState: maybe S.initialState S.fromConfig
    , render
    , eval: Q.eval (MCS.vToE ∘ S.toConfig) ∘ Q.onModify S.processState
    , receiver: const Nothing
    }

render ∷ S.State → H.ComponentHTML Query
render state =
  HH.div_
    [ MCR.section "Server(s)" [ MCR.hosts state S._hosts ]
    , MCR.section "Authentication"
        [ HH.div
            [ HP.classes [CN.formGroup, CN.mountUserInfo] ]
            [ MCR.label "Username" [ MCR.input state S._user [] ]
            , MCR.label "Password" [ MCR.input state S._password [ HP.type_ HP.InputPassword ] ]
            ]
        , HH.div_
            [ MCR.label "Database" [ MCR.input state S._path [] ] ]
        ]
    , MCR.section "Settings" [ MCR.propList S._props state ]
    ]
