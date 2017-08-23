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

module SlamData.AdminUI.Types (module X, HTML, DSL) where

import Halogen as H
import SlamData.AdminUI.Component.ChildSlot (ChildQuery, ChildSlot, MillerQuery, cpMySettings, cpDialog, cpDatabase, cpGroups, cpUsers) as X
import SlamData.AdminUI.Component.Query (GroupItem(..), GroupMessage(..), Query(..), groupItemName, Message(..)) as X
import SlamData.AdminUI.Component.State (ServerState(..), State, TabIndex(..), allTabs, defaultServerState, tabTitle) as X
import SlamData.Monad (Slam)

type HTML = H.ParentHTML X.Query X.ChildQuery X.ChildSlot Slam
type DSL = H.ParentDSL X.State X.Query X.ChildQuery X.ChildSlot X.Message Slam
