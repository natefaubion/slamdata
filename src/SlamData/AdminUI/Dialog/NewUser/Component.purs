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

module SlamData.AdminUI.Dialog.NewUser.Component (dialog) where

import SlamData.Prelude

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
import SlamData.Dialog.Render as DR
import SlamData.Monad (Slam)
import SlamData.Quasar.Security (addUsersToGroup)
import SlamData.Render.ClassName as CN

data Query a
  = Init a
  | ChangeUserId String a
  | Add a
  | HandleGroupSelection (AC.Message String) a
  | Dismiss a

type State =
  { userId ∷ String
  , allGroups ∷ Array QA.GroupPath
  , refreshing ∷ Boolean
  , groupSelection ∷ Maybe QA.GroupPath
  , error ∷ Maybe String
  }

initialState ∷ State
initialState = { userId: "", allGroups: [], refreshing: true, groupSelection: Nothing, error: Nothing }

type ChildSlot = Unit
type Message o = Variant (refreshUsers ∷ Unit | o)
type HTML = H.ParentHTML Query (AC.Query String) ChildSlot Slam
type DSL o = H.ParentDSL State Query (AC.Query String) ChildSlot (D.Message (Message o)) Slam

dialog ∷ ∀ o. D.DialogSpec (Message o) Slam
dialog =
  D.dialog
    $ D.withTitle "New User"
    >>> D.withClass (H.ClassName "sd-admin-ui-user-new-user")
    >>> D.withInitialState initialState
    >>> D.withInitializer Init
    >>> D.withParentRender render
    >>> D.withEval eval
    >>> D.withPending _.refreshing
    >>> D.withButton
        (D.button
         $ D.withLabel "Cancel"
          >>> D.withClass CN.btn
          >>> D.withAction (const (Just Dismiss)))
    >>> D.withButton
        (D.button
          $ D.withLabel "Add user"
          >>> D.withClass CN.btnPrimary
          >>> D.withAction (const (Just Add)))
  where
    render ∷ State → HTML
    render { userId, allGroups, groupSelection, refreshing, error } =
      HH.div_ $ join
        [ pure $ HH.input
            [ HP.value userId
            , HP.placeholder "User id"
            , HP.class_ (H.ClassName "form-control")
            , HE.onValueInput (HE.input ChangeUserId)
            ]
        , pure $ HH.div
            [ HP.class_ (HH.ClassName "sd-admin-ui-user-new-user-group") ]
            [ HH.slot
                unit
                (AC.component
                   AC.defaultConfig
                     { containerClass = H.ClassName "sd-admin-ui-autocomplete"
                     , placeholder = "Add user to group"
                     , autofirst = true
                     , itemFilter = String.contains ∘ String.Pattern
                     })
                (map QA.printGroupPath allGroups)
                (HE.input HandleGroupSelection)
            ]
        , maybe [] (pure ∘ DR.renderError) error
        ]

    eval ∷ Query ~> DSL o
    eval = case _ of
      Init next → do
        refresh
        pure next
      ChangeUserId new next → do
        H.modify (_ { userId = new })
        pure next
      Add next → do
        H.gets _.groupSelection >>= case _ of
          Nothing →
            displayError "Select a valid group."
          Just group → do
            { userId } ← H.get
            if String.null (String.trim userId)
              then displayError "Enter a user id."
              else do
                void $ addUsersToGroup group [QA.UserId userId]
                H.raise (D.Bubble (V.inj (SProxy ∷ SProxy "refreshUsers") unit))
                H.raise D.Dismiss
        pure next
      HandleGroupSelection msg next → case msg of
        AC.Changed g → do
          { allGroups } ← H.get
          let
            selection = do
              path ← hush (QA.parseGroupPath g)
              guard (Array.elem path allGroups)
              pure path
          H.modify (_ { groupSelection = selection })
          pure next
        AC.Selected _ → pure next
      Dismiss next → do
        H.raise D.Dismiss
        pure next

displayError ∷ ∀ o. String → DSL o Unit
displayError s = H.modify (_ { error = Just s })

refresh ∷ ∀ o. DSL o Unit
refresh = do
  H.modify (_ { refreshing = true })
  allGroups ← Users.fetchAllGroups
  H.modify (_ { allGroups = allGroups, refreshing = false })
