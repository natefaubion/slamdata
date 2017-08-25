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

module SlamData.AdminUI.Users.Component where


import SlamData.Prelude

import Data.Array as Array
import Data.String as String
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Quasar.Advanced.Types as QA
import SlamData.AdminUI.Dialog as Dialog
import SlamData.AdminUI.Users (allUsers, fetchAllGroups, fetchTransitiveUsers)
import SlamData.Autocomplete.Component as AC
import SlamData.Monad (Slam)
import SlamData.Render.Common as R
import SlamData.Render.Icon as I

data Query a
  = Init a
  | Refresh a
  | FetchUsers a
  | SetFilter String a
  | SetGroupFilter QA.GroupPath a
  | NewUser a
  | DeleteUser QA.UserId a
  | EditUser QA.UserId a
  | Select Int a
  | HoverIn Int a
  | HoverOut a
  | HandleGroupFilter (AC.Message String) a

data GroupFilter
  = NoFilter
  | GroupFilter (Array QA.UserId)
  | InvalidGroupFilter String

data Message = RaiseDialog Dialog.Definition

type State =
  { filter ∷ String
  , users ∷ Maybe (Array QA.UserId)
  , allGroups ∷ Maybe (Array QA.GroupPath)
  , groupFilter ∷ GroupFilter
  , selected ∷ Maybe Int
  , hovered ∷ Maybe Int
  }

defaultState ∷ State
defaultState = { filter: "", users: Nothing, allGroups: Nothing, groupFilter: NoFilter, selected: Nothing, hovered: Nothing }

type ChildSlot = Unit

type HTML = H.ParentHTML Query (AC.Query String) ChildSlot Slam
type DSL = H.ParentDSL State Query (AC.Query String) ChildSlot Message Slam

component ∷ H.Component HH.HTML Query Unit Message Slam
component =
  H.lifecycleParentComponent
    { render
    , eval
    , receiver: const Nothing
    , initialState: const defaultState
    , initializer: Just (H.action Init)
    , finalizer: Nothing
    }
    where
      render state =
        HH.div
          [ HP.class_ (HH.ClassName "sd-admin-ui-users") ]
          [ HH.div
            [ HP.class_ (HH.ClassName "sd-admin-ui-users-header")]
            [ HH.div [ HP.class_ (H.ClassName "sd-admin-ui-users-search")]
                [ HH.div
                    [ HP.class_ (HH.ClassName "sd-admin-ui-users-search-icon") ]
                    [ I.searchSm ]
                , HH.input
                    [ HP.classes (HH.ClassName <$> ["sd-admin-ui-users-search-field", "form-control"])
                    , HP.type_ HP.InputText
                    , HP.placeholder "Search by user id"
                    , HE.onValueInput (HE.input \str → SetFilter str)
                    , HP.value state.filter
                    ]
                , HH.button
                    [ HP.class_ (HH.ClassName "sd-admin-ui-users-search-clear")
                    , HP.type_ HP.ButtonButton
                    , HE.onClick (HE.input_ (SetFilter ""))
                    , HP.enabled (state.filter /= "")
                    ]
                    [ R.clearFieldIcon "Clear search string" ]
                ]
            , HH.slot
                unit
                (AC.component
                   AC.defaultConfig
                     { containerClass = H.ClassName "sd-admin-ui-autocomplete"
                     , placeholder = "Search by group"
                     , autofirst = true
                     , itemFilter = String.contains ∘ String.Pattern
                     })
                (maybe [] (map QA.printGroupPath) state.allGroups)
                (HE.input HandleGroupFilter)
            , HH.button
                [ HE.onClick (HE.input_ NewUser)
                , HP.classes (map H.ClassName ["btn", "btn-success"])
                ]
                [ HH.text "New User" ]
            ]
          , HH.ul
              [ HP.class_ (HH.ClassName "sd-admin-ui-users-list") ]
              case state.users of
                Nothing →
                  []
                Just users →
                  (Array.mapWithIndex renderUser (Array.filter (userFilter && groupFilter) users))
          ]
        where
          userFilter uid = String.contains (String.Pattern state.filter) (QA.runUserId uid)
          groupFilter userId = case state.groupFilter of
            NoFilter →
              true
            InvalidGroupFilter _ →
              true
            GroupFilter userIds →
              userId `Array.elem` userIds

          renderUser ix userId =
            let
              isActive = state.selected == Just ix || state.hovered == Just ix
            in
              HH.li
                [ HP.class_ (HH.ClassName "sd-admin-ui-user")
                , HE.onMouseEnter (HE.input_ (HoverIn ix))
                , HE.onMouseLeave (HE.input_ HoverOut)
                , HE.onClick (HE.input_ (Select ix))
                ]
                [ HH.div
                    [ HP.class_ (HH.ClassName "sd-admin-ui-user-label") ]
                    [ HH.text (QA.runUserId userId) ]
                , HH.div
                      [ HP.classes (HH.ClassName <$> ["sd-admin-ui-user-actions"] <> (guard (not isActive) $> "hidden")) ]
                    [ HH.a
                        [ HE.onClick (HE.input_ (DeleteUser userId)) ]
                        [ HH.text "Delete" ]
                    , HH.a
                        [ HE.onClick (HE.input_ (EditUser userId)) ]
                        [ HH.text "Permissions" ]
                    ]
                ]
      eval ∷ Query ~> DSL
      eval = case _ of
        Init next → do
          allGroups ← fetchAllGroups
          users ← allUsers
          H.modify (_ { allGroups = Just allGroups, users = Just users })
          pure next
        Refresh next → do
          allGroups ← fetchAllGroups
          users ← allUsers
          H.query unit (H.request AC.GetInput) >>= case _ of
            Just input → updateGroupFilter input
            Nothing → H.modify (_ { groupFilter = NoFilter })
          H.modify (_ { allGroups = Just allGroups, users = Just users })
          pure next
        Select ix next → do
          H.modify (_ { selected = Just ix })
          pure next
        HoverIn ix next → do
          H.modify (_ { hovered = Just ix })
          pure next
        HoverOut next → do
          H.modify (_ { hovered = Nothing })
          pure next
        FetchUsers next → do
          users ← allUsers
          H.modify (_ { users = Just users })
          pure next
        SetFilter s next → do
          H.modify (_ { filter = s })
          pure next
        SetGroupFilter path next → do
          setGroupFilter path
          _ ← H.query unit (H.action (AC.Input (QA.printGroupPath path)))
          _ ← H.query unit (H.action (AC.Close AC.CuzSelect))
          pure next
        NewUser next → do
          H.raise (RaiseDialog Dialog.NewUser)
          pure next
        DeleteUser userId next → do
          H.raise (RaiseDialog (Dialog.DeleteUser userId))
          pure next
        EditUser userId next → do
          H.raise (RaiseDialog (Dialog.UserPermissions userId))
          pure next
        HandleGroupFilter msg next → case msg of
          AC.Changed s → do
            updateGroupFilter s
            pure next
          AC.Selected _ → pure next

updateGroupFilter ∷ String → DSL Unit
updateGroupFilter = case _ of
  "" →
    H.modify (_ { groupFilter = NoFilter })
  s → case QA.parseGroupPath s of
    Right path → setGroupFilter path
    Left _ → H.modify (_ { groupFilter = InvalidGroupFilter s })

setGroupFilter ∷ QA.GroupPath → DSL Unit
setGroupFilter path = do
  allGroups ← H.gets _.allGroups
  case allGroups of
    Just groups | path `Array.elem` groups → do
      users ← fetchTransitiveUsers path
      H.modify (_ { groupFilter = GroupFilter users })
    _ →
      H.modify (_ { groupFilter = InvalidGroupFilter (QA.printGroupPath path) })
