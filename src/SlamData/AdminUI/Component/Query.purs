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

module SlamData.AdminUI.Component.Query where

import SlamData.Prelude

import Quasar.Advanced.Types as QA
import SlamData.AdminUI.Component.State (GroupsState, MySettingsState, ServerState, TabIndex)
import SlamData.AdminUI.Dialog as Dialog
import SlamData.AdminUI.Users.Component as Users
import SlamData.Workspace.MillerColumns.Component as Miller
import Utils.DOM as DOM

data Message = Closed

data Query a
  = Init a
  | Open a
  | Close a
  | SetActive TabIndex a
  | SetMySettings MySettingsState a
  | SetServer ServerState a
  | SetGroups GroupsState a
  | DefaultThemeChanged String a
  | HandleColumns (Miller.Message GroupItem QA.GroupPath) a
  | HandleColumnOrItem GroupMessage a
  | HandleDialog Dialog.Message' a
  | HandleUsers Users.Message a

data GroupItem = GroupItem { path ∷ QA.GroupPath, name ∷ String }

derive instance eqGroupItem ∷ Eq GroupItem
derive instance ordGroupItem ∷ Ord GroupItem

groupItemName ∷ GroupItem → String
groupItemName = case _ of
  GroupItem { name } → name

data GroupMessage
  = AddNewGroup { path ∷ QA.GroupPath, event ∷ DOM.Event, name ∷ String }
  | DeleteGroup { path ∷ QA.GroupPath }
  | DisplayUsers { path ∷ QA.GroupPath }
