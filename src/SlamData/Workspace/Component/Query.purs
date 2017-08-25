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

module SlamData.Workspace.Component.Query where

import SlamData.Prelude

import DOM.Event.Types (MouseEvent)
import Data.List (List)
import Halogen as H
import Quasar.Advanced.Types (ProviderR)
import SlamData.AdminUI.Types as AdminUI
import SlamData.GlobalMenu.Bus (SignInMessage)
import SlamData.Guide.StepByStep.Component as Guide
import SlamData.Header.Component as Header
import SlamData.License (LicenseProblem)
import SlamData.Notification as N
import SlamData.Wiring as Wiring
import SlamData.Workspace.Deck.DeckId (DeckId)
import SlamData.Workspace.Dialog.Component as Dialog
import SlamData.Workspace.Guide (GuideType)
import Utils.Path as UP

data Query a
  = Init a
  | DismissAll MouseEvent a
  | New a
  | Load (List DeckId) a
  | ExploreFile UP.FilePath a
  | PresentStepByStepGuide GuideType (H.SubscribeStatus → a)
  | Resize (H.SubscribeStatus → a)
  | SignIn ProviderR a
  | HandleDialog Dialog.Message a
  | HandleGuideMessage GuideType Guide.Message a
  | HandleHeader Header.Message a
  | HandleAdminUI AdminUI.Message a
  | HandleNotification N.Action a
  | HandleSignInMessage SignInMessage a
  | HandleWorkspace Wiring.WorkspaceMessage a
  | HandleLicenseProblem (Maybe LicenseProblem) a
  | HandleDeck Wiring.DeckMessage a
  | ResetTheme a
