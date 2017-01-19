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

module SlamData.FileSystem.Dialog.Mount.MarkLogic.Component
  ( comp
  , Query
  , module SlamData.FileSystem.Dialog.Mount.Common.SettingsQuery
  , module MCS
  ) where

import SlamData.Prelude

import Data.Path.Pathy (dir, (</>))

import Halogen as H
import Halogen.HTML.Indexed as HH
import Halogen.HTML.Events.Indexed as HE
import Halogen.HTML.Properties.Indexed as HP
import Halogen.Themes.Bootstrap3 as B

import Quasar.Mount as QM
import Quasar.Mount.MarkLogic (Format(..))

import SlamData.Monad (Slam)
import SlamData.FileSystem.Dialog.Mount.Common.Render as MCR
import SlamData.FileSystem.Dialog.Mount.Common.SettingsQuery (SettingsQuery(..))
import SlamData.FileSystem.Dialog.Mount.MarkLogic.Component.State as MCS
import SlamData.FileSystem.Resource (Mount(..))
import SlamData.Quasar.Mount as API
import SlamData.Quasar.Error as QE
import SlamData.Render.CSS as Rc

type Query = SettingsQuery MCS.State

type HTML = H.ComponentHTML Query

comp ∷ H.Component MCS.State Query Slam
comp = H.component { render, eval }

render ∷ MCS.State → HTML
render state =
  HH.div
    [ HP.key "mount-marklogic"
    , HP.class_ Rc.mountMarkLogic
    ]
    [ MCR.section "Server" [ MCR.host state MCS._host' ]
    , MCR.section "Authentication"
        [ HH.div
            [ HP.classes [B.formGroup, Rc.mountUserInfo] ]
            [ MCR.label "Username" [ MCR.input state MCS._user [] ]
            , MCR.label "Password" [ MCR.input state MCS._password [ HP.inputType HP.InputPassword ] ]
            ]
        , HH.div
            [ HP.classes [B.formGroup, Rc.mountPath] ]
            [ MCR.label "Database" [ MCR.input state MCS._path [] ] ]
        , HH.div
            [ HP.class_ Rc.mountFormat ]
            [ HH.label_ [ HH.span_ [ HH.text "Format" ] ]
            , formatRadio "XML" XML
            , formatRadio "JSON" JSON
            ]
        ]
    ]

  where
  formatRadio lbl val =
    HH.label_
      [ HH.input
          [ HP.inputType HP.InputRadio
          , HP.name "mlformat"
          , HP.checked (state.format ≡ val)
          , HE.onValueChange (HE.input_ (ModifyState _ { format = val }))
          ]
      , HH.text lbl
      ]


eval ∷ Query ~> H.ComponentDSL MCS.State Query Slam
eval = case _ of
  ModifyState f next →
    H.modify f $> next
  Validate k →
    k <<< either Just (const Nothing) <<< MCS.toConfig <$> H.get
  Submit parent name k →
    k <$> runExceptT do
      st ← lift H.get
      config ← except $ lmap QE.msgToQError $ MCS.toConfig st
      let path = parent </> dir name
      ExceptT $ API.saveMount (Left path) (QM.MarkLogicConfig config)
      pure $ Database path
