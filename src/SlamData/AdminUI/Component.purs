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

module SlamData.AdminUI.Component
  ( component
  ) where

import SlamData.Prelude

import Control.Monad.Eff.Exception as Exception
import Data.Array as Array
import Data.Newtype (over)
import Data.Path.Pathy ((</>))
import Data.Path.Pathy as Pathy
import Data.Variant as V
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Quasar.Advanced.Types as QA
import SlamData.AdminUI.Database.Component as DB
import SlamData.AdminUI.Dialog as Dialog
import SlamData.AdminUI.Group as Group
import SlamData.AdminUI.MySettings.Component as MySettings
import SlamData.AdminUI.Types as AT
import SlamData.AdminUI.Users as Users
import SlamData.AdminUI.Users.Component as UC
import SlamData.Monad (Slam)
import SlamData.Notification as Notification
import SlamData.Quasar.Security (createGroup, deleteGroup)
import SlamData.Workspace.MillerColumns.Component as Miller
import Utils.DOM as DOM

component ∷ H.Component HH.HTML AT.Query Unit AT.Message Slam
component =
  H.lifecycleParentComponent
    { initialState: \_ →
       { open: false
       , active: AT.Users
       , formState:
          { server: AT.defaultServerState }
      , dialog: Nothing
       }
    , render
    , eval
    , initializer: Just (H.action AT.Init)
    , finalizer: Nothing
    , receiver: const Nothing
    }

render ∷ AT.State → AT.HTML
render state =
  HH.div
    [ HP.classes $ fold
        [ pure (H.ClassName "sd-admin-ui")
        , guard (not state.open) $> H.ClassName "hidden"
        ]
    ]
    [ HH.slot' AT.cpDialog unit Dialog.component state.dialog (HE.input AT.HandleDialog)
    , tabHeader state.active
    , tabBody state
    ]

tabHeader ∷ AT.TabIndex → AT.HTML
tabHeader active =
  HH.ul
    [ HP.class_ $ H.ClassName "sd-admin-ui-tabs"]
    $ Array.fromFoldable
    $ AT.allTabs <#> \t →
      HH.li
        (fold
          [ pure $ HE.onClick $ HE.input_ $ AT.SetActive t
          , guard (t == active) $> HP.class_ (H.ClassName "active-tab")
          ])
        [ HH.text (AT.tabTitle t) ]

tabBody ∷ AT.State → AT.HTML
tabBody state =
  HH.div
    [HP.class_ $ HH.ClassName "sd-admin-ui-tab-body"]
    (if state.open then activeTab <> [closeButton] else [])
  where
    closeButton =
      HH.div
        [ HP.class_ (HH.ClassName "sd-admin-ui-close") ]
        [ HH.button
            [ HE.onClick (HE.input_ AT.Close)
            , HP.classes (H.ClassName <$> ["btn", "btn-primary"])
            ]
            [ HH.text "Dismiss Settings" ]
        ]
    activeTab = case state.active of
      AT.MySettings →
        [ HH.slot' AT.cpMySettings unit MySettings.component unit absurd ]
      AT.Database →
        [ HH.slot' AT.cpDatabase unit DB.component unit absurd ]
      AT.Server →
        pure $ HH.div
          [ HP.class_ (HH.ClassName "sd-admin-ui-server") ]
          (renderServerForm state.formState.server)
      AT.Users →
        [ HH.slot' AT.cpUsers unit UC.component unit (HE.input AT.HandleUsers) ]
      AT.Groups →
        pure $ HH.div
          [ HP.class_ (HH.ClassName "sd-admin-ui-groups") ]
          Group.renderGroupsForm
      _ →
        [HH.text "Not implemented"]
      -- AT.Authentication → ?x

renderServerForm ∷ AT.ServerState → Array AT.HTML
renderServerForm (AT.ServerState state) =
  [ HH.fieldset_
      [ HH.legend_ [ HH.text "Port" ]
      , HH.input
          [ HP.class_ (HH.ClassName "form-control") ]
      , HH.p_ [ HH.text "Changing the port will restart the server and reload the browser to the new port. If there are any errors in changing to the new port, however, you may have to use the browser back button."
              ]
      ]
  , HH.fieldset_
      [ HH.legend_ [HH.text "Location of log file in the SlamData file system"]
      , HH.input
          [ HP.class_ (HH.ClassName "form-control") ]
      ]
  , HH.fieldset_
      [ HH.legend
          [ HP.class_ (HH.ClassName "checkbox") ]
          [ HH.label_
              [ HH.input
                  [ HP.checked state.enableCustomSSL
                  , HE.onChecked (HE.input_ (AT.SetServer (AT.ServerState (state {enableCustomSSL = not state.enableCustomSSL}))))
                  , HP.type_ HP.InputCheckbox
                  ]
                , HH.text "Enable Custom SSL"
                ]
            ]
      , HH.textarea [HP.class_ (HH.ClassName "form-control"), HP.disabled (not state.enableCustomSSL)]
      ]
  ]

eval ∷ AT.Query ~> AT.DSL
eval = case _ of
  AT.Init next → do
    pure next
  AT.Open next → do
    H.modify (_ { open = true })
    pure next
  AT.Close next → do
    H.modify (_ { open = false })
    H.raise AT.Closed
    pure next
  AT.SetActive ix next → do
    H.modify (_ { active = ix })
    pure next
  AT.SetServer new next → do
    H.modify (_ { formState { server = new } })
    pure next
  AT.HandleColumns columnMsg next → do
    case columnMsg of
      Miller.SelectionChanged _ _ _ →
        pure next
      Miller.LoadRequest req@(path × _) → do
        res ← Group.load req
        _ ← H.query' AT.cpGroups unit (H.action (Miller.FulfilLoadRequest (path × res)))
        pure next
  AT.HandleColumnOrItem columnMsg next → case columnMsg of
    AT.AddNewGroup { path, event, name } → do
      H.liftEff (DOM.preventDefault event)
      createGroup (over QA.GroupPath (_ </> Pathy.dir name) path) >>= case _ of
        Right _ →
          H.query' AT.cpGroups unit (H.action Miller.Reload) $> unit
        Left err → do
          Notification.error
            ("Failed to add the group " <> name <> " at " <> QA.printGroupPath path)
            (Just (Notification.Details (Exception.message err)))
            Nothing
            Nothing
      pure next
    AT.DeleteGroup { path } → do
      H.modify (_ { dialog = Just (Dialog.DeleteGroup path) })
      pure next
    AT.DisplayUsers { path } → do
      H.modify (_ { active = AT.Users })
      _ ← H.query' AT.cpUsers unit (H.action (UC.SetGroupFilter path))
      pure next
  AT.HandleDialog msg next → do
    let dismissDialog = H.modify (_ { dialog = Nothing })
    case msg of
      Dialog.Bubble v → do
        v # (V.case_
          # V.on Dialog._deleteUser (\userId → do
            Users.deleteUser userId
            _ ← H.query' AT.cpUsers unit (H.action UC.FetchUsers)
            dismissDialog
            pure unit)
          # V.on Dialog._refreshUsers (\_ → do
            _ ← H.query' AT.cpUsers unit (H.action UC.Refresh)
            pure unit)
          # V.on Dialog._deleteGroup (\group → do
            _ ← deleteGroup group
            _ ← H.query' AT.cpGroups unit (H.action Miller.Reload)
            dismissDialog
            pure unit))
      Dialog.Dismiss →
        dismissDialog
    pure next
  AT.HandleUsers (UC.RaiseDialog dlg) next → do
    H.modify (_ { dialog = Just dlg })
    pure next
