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

module SlamData.Workspace.Notification
  ( DetailedError
  , loadDeckFail
  , loadParentFail
  , saveDeckFail
  , saveMirrorFail
  , deleteDeckFail
  , setRootFail
  , module N
  ) where

import SlamData.Prelude
import SlamData.Notification as N

import Control.Monad.Aff.AVar (AVAR)
import Control.Monad.Aff.Bus (Bus, Cap)
import Control.Monad.Aff.Free (class Affable)

type DetailedError =
  ∀ r m eff
  . (Affable (avar ∷ AVAR | eff) m)
  ⇒ String
  → Bus (write ∷ Cap | r) N.NotificationOptions
  → m Unit

loadDeckFail ∷ DetailedError
loadDeckFail detail =
  N.error_ "Failed to load your deck." (Just detail) Nothing

loadParentFail ∷ DetailedError
loadParentFail detail =
  N.error_ "Failed to load a parent deck." (Just detail) Nothing

saveDeckFail ∷ DetailedError
saveDeckFail detail =
  N.error_ "Failed to save your deck." (Just detail) Nothing

saveMirrorFail ∷ DetailedError
saveMirrorFail detail =
  N.error_ "Failed to save a mirrored card." (Just detail) Nothing

deleteDeckFail ∷ DetailedError
deleteDeckFail detail =
  N.error_ "Failed to delete your deck." (Just detail) Nothing

setRootFail ∷ DetailedError
setRootFail detail =
  N.error_ "Failed to update your workspace root." (Just detail) Nothing
