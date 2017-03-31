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

module SlamData.FileSystem.Dialog.Mount.SparkHDFS.Component
  ( comp
  , Query
  , module SlamData.FileSystem.Dialog.Mount.Common.SettingsQuery
  , module MCS
  ) where

import SlamData.Prelude

import Data.Array as Array
import Data.Path.Pathy (dir, (</>))

import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.Themes.Bootstrap3 as B

import Quasar.Mount as QM

import SlamData.Monad (Slam)
import SlamData.FileSystem.Dialog.Mount.Common.Render as MCR
import SlamData.FileSystem.Dialog.Mount.Common.SettingsQuery (SettingsQuery(..), SettingsMessage(..))
import SlamData.FileSystem.Dialog.Mount.SparkHDFS.Component.State as MCS
import SlamData.FileSystem.Resource (Mount(..))
import SlamData.Quasar.Mount as API
import SlamData.Quasar.Error as QE
import SlamData.Render.CSS as Rc

type Query = SettingsQuery MCS.State

comp ∷ MCS.State → H.Component HH.HTML Query Unit SettingsMessage Slam
comp initialState =
  H.component
    { initialState: const initialState
    , render
    , eval
    , receiver: const Nothing
    }

render ∷ MCS.State → H.ComponentHTML Query
render state =
  HH.div
    [ HP.class_ Rc.mountSpark ]
    [ MCR.section "Spark Server" [ MCR.host state MCS._sparkHost ]
    , MCR.section "HDFS Server" [ MCR.host state MCS._hdfsHost ]
    , MCR.section "Root"
        [ HH.div
            [ HP.class_ Rc.mountPath ]
            [ MCR.label "Path" [ MCR.input state MCS._path [] ] ]
        ]
    , MCR.section "Advanced Settings"
        [ MCR.propListTable (Array.mapWithIndex renderPropRow (state.props <> [ "" × "" ])) ]
    ]
  where
  renderPropRow ∷ Int → String × String → H.ComponentHTML Query
  renderPropRow ix (key × value) =
    let
      values =
        (guard (key ≠ "") $> "")
          <> [ key ]
          <> state.availableProps
      updateFnKey = MCS.updatePropAt ix ∘ flip Tuple value
      updateFnVal = MCS.updatePropAt ix ∘ Tuple key
      classes = [ B.formControl, B.inputSm ]
    in
    HH.tr_
      [ HH.td_
          [ HH.select
              [ HP.value key
              , HP.classes classes
              , HE.onSelectedIndexChange (map (H.action ∘ ModifyState ∘ updateFnKey) ∘ Array.index values)
              ]
              (values <#> HH.option_ ∘ pure ∘ HH.text)
          ]
      , HH.td_
          [ HH.input
              [ HP.value value
              , HP.type_ HP.InputText
              , HP.classes classes
              , HE.onValueInput (HE.input (ModifyState ∘ updateFnVal))
              ]
          ]
      ]

eval ∷ Query ~> H.ComponentDSL MCS.State Query SettingsMessage Slam
eval = case _ of
  ModifyState f next → do
    H.modify f
    H.raise Modified
    pure next
  Validate k →
    k <<< either Just (const Nothing) <<< MCS.toConfig <$> H.get
  Submit parent name k →
    k <$> runExceptT do
      st ← lift H.get
      config ← except $ lmap QE.msgToQError $ MCS.toConfig st
      let path = parent </> dir name
      ExceptT $ API.saveMount (Left path) (QM.SparkHDFSConfig config)
      pure $ Database path
