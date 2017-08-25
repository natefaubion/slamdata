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

module SlamData.AdminUI.Users where


import SlamData.Prelude

import Control.Monad.Eff.Exception as Exception
import Data.Array as Array
import Data.Path.Pathy as Pathy
import Quasar.Advanced.Types as QA
import SlamData.Notification as Notification
import SlamData.Quasar.Class (class QuasarDSL)
import SlamData.Quasar.Security (groupInfo, removeUsersFromGroup)

groupInfoNotify
  ∷ ∀ m a
  . Monad m
  ⇒ QuasarDSL m
  ⇒ Notification.NotifyDSL m
  ⇒ String
  → QA.GroupPath
  → a
  → (QA.GroupInfoR → m a)
  → m a
groupInfoNotify msg path def f =
  groupInfo path >>= case _ of
    Left err → do
      Notification.error
        msg
        (Just (Notification.Details (Exception.message err)))
        Nothing
        Nothing
      pure def
    Right result → f result

allUsers ∷ ∀ m. Monad m ⇒ QuasarDSL m ⇒ Notification.NotifyDSL m ⇒ m (Array QA.UserId)
allUsers = fetchTransitiveUsers (QA.GroupPath Pathy.rootDir)

fetchAllGroups
  ∷ ∀ m. Monad m
  ⇒ QuasarDSL m
  ⇒ Notification.NotifyDSL m
  ⇒ m (Array QA.GroupPath)
fetchAllGroups = do
  groupInfoNotify
    "Failed to load groups from Quasar"
    (QA.GroupPath Pathy.rootDir)
    []
    (pure ∘ _.subGroups)

fetchTransitiveUsers
  ∷ ∀ m
  . Monad m
  ⇒ QuasarDSL m
  ⇒ Notification.NotifyDSL m
  ⇒ QA.GroupPath
  → m (Array QA.UserId)
fetchTransitiveUsers path = do
  groupInfoNotify
    "Failed to load users from Quasar"
    path
    []
    (pure ∘ _.allMembers)

crawlGroups
  ∷ ∀ f m
  . Monad m
  ⇒ Parallel f m
  ⇒ QuasarDSL m
  ⇒ Notification.NotifyDSL m
  ⇒ QA.UserId
  → m (Array QA.GroupPath)
crawlGroups userId =
  map
    -- Drop the root directory, because every User is member of it implicitly
    (fromMaybe [] ∘ Array.tail)
    (crawlGroup (QA.GroupPath Pathy.rootDir))
  where
    crawlGroup path =
      groupInfoNotify ("Failed to load users for " <> QA.printGroupPath path) path [] (go path)

    go path { allMembers, subGroups, members }
      | userId `Array.elem` allMembers = do
          -- We only consider the group hierarchy one level at a time, because we
          -- need to filter on group membership
          paths ← parTraverse crawlGroup (Array.filter (isDirectSubgroup path) subGroups)
          let addPath = if userId `Array.elem` members then Array.cons path else id
          pure (addPath (fold paths))
      | otherwise = pure []


isDirectSubgroup ∷ QA.GroupPath → QA.GroupPath → Boolean
isDirectSubgroup (QA.GroupPath parent) (QA.GroupPath child) =
  case Pathy.peel child of
    Nothing → -- Should never happen
      false
    Just (childPrefix × _) →
      Pathy.identicalPath parent childPrefix

deleteUser
  ∷ ∀ f m
  . Monad m
  ⇒ QuasarDSL m
  ⇒ Parallel f m
  ⇒ Notification.NotifyDSL m
  ⇒ QA.UserId
  → m Unit
deleteUser userId = do
  groups ← crawlGroups userId
  flip parTraverse_ groups \group →
    removeUsersFromGroup group [userId]
