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

module SlamData.AdminUI.Dialog.UserPermissions.Component (dialog) where

import SlamData.Prelude

import Control.Monad.Eff.Exception as Exception
import Data.Array as Array
import Data.String as String
import Data.Variant as V
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Quasar.Advanced.Types as QA
import SlamData.AdminUI.Users as Users
import SlamData.Autocomplete.Component as AC
import SlamData.Dialog.Component as D
import SlamData.Monad (Slam)
import SlamData.Notification as Notification
import SlamData.Quasar.Security (addUsersToGroup, removeUsersFromGroup)
import SlamData.Render.ClassName as CN

data Query a
  = Init a
  | Delete QA.GroupPath a
  | Add QA.GroupPath a
  | HandleGroupSelection (AC.Message String) a
  | Dismiss a

type State =
  { groups ∷ Array QA.GroupPath
  , allGroups ∷ Array QA.GroupPath
  , refreshing ∷ Boolean
  , groupSelection ∷ Maybe QA.GroupPath
  }

initialState ∷ State
initialState = { groups: [], allGroups: [], refreshing: true, groupSelection: Nothing }

type ChildSlot = Unit
type Message o = Variant (refreshUsers ∷ Unit | o)
type HTML = H.ParentHTML Query (AC.Query String) ChildSlot Slam
type DSL o = H.ParentDSL State Query (AC.Query String) ChildSlot (D.Message (Message o)) Slam

dialog ∷ ∀ o. QA.UserId → D.DialogSpec (Message o) Slam
dialog userId =
  D.dialog
    $ D.withTitle ("Edit Permissions — " <> QA.runUserId userId)
    >>> D.withClass (H.ClassName "sd-admin-ui-user-permissions")
    >>> D.withInitialState initialState
    >>> D.withInitializer Init
    >>> D.withParentRender render
    >>> D.withEval eval
    >>> D.withPending _.refreshing
    >>> D.withButton
        (D.button
          $ D.withLabel "Done"
          >>> D.withClass CN.btnPrimary
          >>> D.withAction (const (Just Dismiss)))
  where
    render ∷ State → HTML
    render { groups, allGroups, groupSelection, refreshing } =
      HH.div_
        [ HH.div
            [ HP.class_ (HH.ClassName "sd-admin-ui-user-permissions-add-group") ]
            [ HH.slot
                unit
                (AC.component
                   AC.defaultConfig
                     { containerClass = H.ClassName "sd-admin-ui-autocomplete"
                     , placeholder = "Add user to group"
                     , autofirst = true
                     , itemFilter = String.contains ∘ String.Pattern
                     })
                (map QA.printGroupPath (allGroups `Array.difference` groups))
                (HE.input HandleGroupSelection)
            , addButton
            ]
        , HH.ul
            [ HP.class_ (HH.ClassName "sd-admin-ui-user-permissions-groups") ]
            (map renderGroup groups)
        ]
      where
        addButton =
          let
            props = case groupSelection of
              Nothing →
                [ HP.disabled true ]
              Just group →
                [ HE.onClick (HE.input_ (Add group)) ]
          in
           HH.button ([HP.classes ([CN.btn, H.ClassName "btn-success"])] <> props) [ HH.text "Add"]

        renderGroup group =
           HH.li
             [ HP.class_ (HH.ClassName "sd-admin-ui-user-permissions-group") ]
             [ HH.div
                 [ HP.class_ (HH.ClassName "sd-admin-ui-user-permissions-group-label") ]
                 [ HH.text (QA.printGroupPath group) ]
             , HH.div
                 [ HP.class_ (HH.ClassName "sd-admin-ui-user-permissions-group-actions") ]
                 [ HH.a [ HE.onClick (HE.input_ (Delete group)) ] [ HH.text "Delete"] ]
             ]
    eval ∷ Query ~> DSL o
    eval = case _ of
      Init next → do
        refresh userId
        pure next
      Delete group next → do
        removeUsersFromGroup group [userId] >>= case _ of
          Left err →
            Notification.error
              ("Failed to delete user: " <> QA.runUserId userId <> " from: " <> QA.printGroupPath group)
              (Just (Notification.Details (Exception.message err)))
              Nothing
              Nothing
          Right _ → do
            H.raise (D.Bubble (V.inj (SProxy ∷ SProxy "refreshUsers") unit))
            refresh userId
        pure next
      Add group next → do
        addUsersToGroup group [userId] >>= case _ of
          Left err →
            Notification.error
              ("Failed to add user: " <> QA.runUserId userId <> " to group: " <> QA.printGroupPath group)
              (Just (Notification.Details (Exception.message err)))
              Nothing
              Nothing
          Right _ → do
            _ ← H.query unit (H.action (AC.Input ""))
            _ ← H.query unit (H.action (AC.Close AC.CuzEscape))
            H.raise (D.Bubble (V.inj (SProxy ∷ SProxy "refreshUsers") unit))
            refresh userId
        pure next
      HandleGroupSelection msg next → case msg of
        AC.Changed g → do
          { allGroups } ← H.get
          let selection = do
                path ← hush (QA.parseGroupPath g)
                guard (Array.elem path allGroups)
                pure path
          H.modify (_ { groupSelection = selection })
          pure next
        AC.Selected _ → pure next
      Dismiss next → do
        H.raise D.Dismiss
        pure next

refresh ∷ ∀ o. QA.UserId → DSL o Unit
refresh userId = do
  H.modify (_ { refreshing = true })
  groups ← Users.crawlGroups userId
  allGroups ← Users.fetchAllGroups
  H.modify (_ { groups = groups, allGroups = allGroups, refreshing = false })
