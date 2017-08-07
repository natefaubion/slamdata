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

module SlamData.FileSystem.Dialog.Mount.View.Component
  ( component
  , Query
  , module Q
  , module S
  ) where

import SlamData.Prelude

import Ace.Editor as Editor
import Ace.EditSession as Session
import Ace.Halogen.Component as Ace
import Ace.Types (Editor)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Elements.Keyed as HHEK
import Halogen.HTML.Events as HE
import Quasar.Mount.View as QMV
import SlamData.FileSystem.Dialog.Mount.Common.Render (propList, section)
import SlamData.FileSystem.Dialog.Mount.Common.SettingsQuery as Q
import SlamData.FileSystem.Dialog.Mount.View.Component.State as S
import SlamData.Monad (Slam)

type Query = Q.SettingsQuery S.State
type Message = Q.SettingsMessage QMV.Config

component ∷ H.Component HH.HTML Query (Maybe QMV.Config) Message Slam
component =
  H.parentComponent
    { initialState: maybe S.initialState S.fromConfig
    , render
    , eval: Q.eval S.toConfig
    , receiver: const Nothing
    }

render ∷ S.State → H.ParentHTML Query Ace.AceQuery Unit Slam
render state@{ query } =
  HHEK.div_
    [ "mount-sql2-query" × section "SQL² query"
        [ HH.slot
            unit
            (Ace.aceComponent (aceSetup query) (Just Ace.Live))
            unit
            (HE.input (\(Ace.TextChanged q) → Q.ModifyState (_ { query = q })))
        ]
    , "mount-sql2-vars" × section "Query variables" [ propList S._vars state ]
    ]

aceSetup ∷ String → Editor → Slam Unit
aceSetup query editor = H.liftEff do
  Editor.setMinLines 6 editor
  Editor.setMaxLines 10000 editor
  Editor.setAutoScrollEditorIntoView true editor
  Editor.setTheme "ace/theme/chrome" editor
  Editor.setEnableLiveAutocompletion true editor
  Editor.setEnableBasicAutocompletion true editor
  session ← Editor.getSession editor
  Session.setMode "ace/mode/sql" session
  void $ Editor.setValue query Nothing editor
