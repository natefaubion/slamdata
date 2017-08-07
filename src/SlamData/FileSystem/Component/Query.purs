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

module SlamData.FileSystem.Component.Query where

import SlamData.Prelude

import DOM.Event.Types (Event, MouseEvent)
import DOM.HTML.Types (HTMLElement, HTMLInputElement)
import SlamData.AdminUI.Types as AdminUI
import SlamData.Common.Sort (Sort)
import SlamData.FileSystem.Dialog as Dialog
import SlamData.FileSystem.Listing.Component as Listing
import SlamData.FileSystem.Listing.Item (Item)
import SlamData.FileSystem.Routing.Salt (Salt)
import SlamData.FileSystem.Search.Component as Search
import SlamData.GlobalError (GlobalError)
import SlamData.GlobalMenu.Bus (SignInMessage)
import SlamData.Header.Component as Header
import SlamData.License (LicenseProblem)
import SlamData.Notification.Component as Notification
import Utils.Path (DirPath)

type PageTransition =
  { path ∷ DirPath
  , query ∷ Maybe String
  , sort ∷ Sort
  , salt ∷ Salt
  , isMount ∷ Boolean
  }

data Query a
  = Resort a
  | Transition PageTransition a
  | SetPath DirPath a
  | SetSort Sort a
  | SetSalt Salt a
  | CheckIsMount DirPath a
  | CheckIsUnconfigured a
  | Configure a
  | ShowHiddenFiles a
  | HideHiddenFiles a
  | Download a
  | MakeMount a
  | MakeFolder a
  | MakeWorkspace a
  | UploadFile HTMLElement a
  | FileListChanged HTMLInputElement a
  | SetVersion String a
  | DismissSignInSubmenu a
  | DismissMountHint a
  | DismissIntroVideo a
  | DismissIntroVideoBackdrop MouseEvent a
  | Init a
  | PreventDefault Event (Query a)
  | HandleError GlobalError a
  | HandleListing Listing.Message a
  | HandleDialog Dialog.Message' a
  | HandleNotifications Notification.Message a
  | HandleSignInMessage SignInMessage a
  | HandleSearch Search.Message a
  | HandleLicenseProblem (Maybe LicenseProblem) a
  | HandleHeader Header.Message a
  | HandleAdminUI AdminUI.Message a
  | SetLoading Boolean a
  | SetIsSearching Boolean a
  | AddListings (Array Item) a
  | ShowError String a
