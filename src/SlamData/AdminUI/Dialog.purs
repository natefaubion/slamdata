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

module SlamData.AdminUI.Dialog
  ( module SlamData.AdminUI.Dialog
  , module Exports
  ) where

import SlamData.Prelude

import Data.Variant as V
import Halogen as H
import Halogen.HTML as HH
import Quasar.Advanced.Types as QA
import SlamData.AdminUI.Dialog.NewUser.Component as NewUser
import SlamData.AdminUI.Dialog.UserPermissions.Component as UserPermissions
import SlamData.Dialog.Component (Message(..)) as Exports
import SlamData.Dialog.Component as D
import SlamData.Dialog.Message.Component as Message
import SlamData.Monad (Slam)

type Query = D.Query Definition Action
type Message' = D.Message Action

data Definition
  = DeleteGroup QA.GroupPath
  | DeleteUser QA.UserId
  | NewUser
  | UserPermissions QA.UserId

type Action = Variant
  ( deleteGroup ∷ QA.GroupPath
  , deleteUser ∷ QA.UserId
  , refreshUsers ∷ Unit
  )

component ∷ H.Component HH.HTML Query (Maybe Definition) Message' Slam
component = D.component dialog

dialog ∷ Definition → D.DialogSpec Action Slam
dialog = case _ of
  DeleteGroup group →
    Message.mkSpec
      { title: "Delete Group"
      , message:
          HH.div_
            [ HH.text "Are you sure you want to delete the group "
            , HH.code_ [ HH.text (QA.printGroupPath group) ]
            , HH.text "?"
            ]
      , class_: HH.ClassName "sd-admin-ui-delete-group-dialog"
      , action: Right ("Delete" × V.inj _deleteGroup group)
      }
  DeleteUser userId →
    Message.mkSpec
      { title: "Delete User"
      , message:
          HH.div_
            [ HH.text "Are you sure you want to delete the user "
            , HH.code_ [ HH.text (QA.runUserId userId) ]
            , HH.text "?"
            ]
      , class_: HH.ClassName "sd-admin-ui-delete-user-dialog"
      , action: Right ("Delete" × V.inj _deleteUser userId)
      }
  NewUser →
    NewUser.dialog
  UserPermissions userId →
    UserPermissions.dialog userId

_refreshUsers = SProxy ∷ SProxy "refreshUsers"
_deleteUser = SProxy ∷ SProxy "deleteUser"
_deleteGroup = SProxy ∷ SProxy "deleteGroup"
