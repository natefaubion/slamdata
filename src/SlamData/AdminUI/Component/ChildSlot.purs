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

module SlamData.AdminUI.Component.ChildSlot where

import SlamData.Prelude

import Halogen.Component.ChildPath as CP
import Quasar.Advanced.Types as QA
import SlamData.AdminUI.Component.Query (GroupItem, GroupMessage)
import SlamData.AdminUI.Dialog.Component as Dialog
import SlamData.Workspace.MillerColumns.Component as Miller

type MillerQuery = Miller.Query GroupItem QA.GroupPath GroupMessage

type ChildQuery = MillerQuery ⨁ Dialog.Query ⨁ Const Void
type ChildSlot = Unit ⊹ Dialog.Dialog ⊹ Void

cpGroups ∷ CP.ChildPath MillerQuery ChildQuery Unit ChildSlot
cpGroups = CP.cp1

cpDialog ∷ CP.ChildPath Dialog.Query ChildQuery Dialog.Dialog ChildSlot
cpDialog = CP.cp2