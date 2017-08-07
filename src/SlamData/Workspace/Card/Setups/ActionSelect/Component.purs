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
module SlamData.Workspace.Card.Setups.ActionSelect.Component where

import SlamData.Prelude

import Data.Array as Array

import Halogen as H
import Halogen.Component.Proxy as HCP
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.Component.ChildPath (cp1, cp2)

import SlamData.Monad (Slam)
import SlamData.ActionList.Component as ALC
import SlamData.Render.Icon as I
import SlamData.Workspace.Card.Setups.Dialog as CSD

data Query s a
  = Init a
  | HandleSelect (ALC.Message (Maybe s)) a
  | HandleSelecting (Maybe s) a
  | HandleConfirm (Maybe s) a
  | HandleDismiss a
  | HandleConfirmSelecting s a
  | HandleDismissSelecting a
  | UpdateDimensions a

data Message s
  = Dismiss
  | Confirm (Maybe s)

type OptionComponent s = HCP.ProxyComponent (Const Void) (Maybe s) (Maybe s) Slam

type Label = { text ∷ String, icon ∷ Maybe I.IconHTML }

type State' s r =
  { options ∷ Array s
  , selection ∷ Maybe (s × s)
  , title ∷ String
  , deselectable ∷ Boolean
  , toSelection ∷ s → Maybe (OptionComponent s)
  , toLabel ∷ s → Label
  | r
  }

type Props s = State' s ()

type State s = State' s
  ( selecting ∷ Maybe (Selecting s)
  , previousSelection ∷ Maybe (s × s)
  )

type Selecting s =
  { component ∷ OptionComponent s
  , value ∷ Maybe s
  , option ∷ s
  }

type ChildQuery s
  = ALC.Query (Maybe s)
  ⨁ HCP.ProxyQ (Const Void) (Maybe s) (Maybe s)
  ⨁ Const Void

type ChildSlot
  = Unit
  ⊹ Unit
  ⊹ Void

type DSL s = H.ParentDSL (State s) (Query s) (ChildQuery s) ChildSlot (Message s) Slam
type HTML s = H.ParentHTML (Query s) (ChildQuery s) ChildSlot Slam

selectConf ∷ ∀ s. Eq s ⇒ ALC.MkConf (Maybe s)
selectConf = map (_ { classes = [] }) <$> ALC.defaultConf

component ∷ ∀ s. Eq s ⇒ H.Component HH.HTML (Query s) (Props s) (Message s) Slam
component =
  H.lifecycleParentComponent
    { render
    , eval
    , initialState
    , receiver: const Nothing
    , initializer: Just (H.action Init)
    , finalizer: Nothing
    }
  where
  initialState ∷ Props s → State s
  initialState { options, selection, title, toSelection, toLabel, deselectable } =
    { options
    , deselectable
    , selection
    , title
    , toSelection
    , toLabel
    , selecting: Nothing
    , previousSelection: Nothing
    }

  render ∷ State s → HTML s
  render st = case st.selecting of
    Nothing  → renderSelect st
    Just sel → renderSelecting st sel

  renderSelect ∷ State s → HTML s
  renderSelect st =
    let
      isNull = Array.null st.options
      content
        | Array.null st.options =
          [ HH.p [ HP.classes [ HH.ClassName "no-options" ] ] [ HH.text "No available options" ] ]
        | otherwise =
          [ HH.slot' cp1 unit (ALC.actionListComp selectConf []) unit (HE.input HandleSelect) ]
    in
      CSD.pickerDialog
        { onDismiss: HandleDismiss
        , onConfirm: HandleConfirm
        , selection: Just (snd <$> st.selection)
        , isSelectable: const (not isNull)
        , classes: [ HH.ClassName "sd-action-select" ]
        , title: [ HH.text st.title ]
        , content
        }

  renderSelecting ∷ State s → Selecting s → HTML s
  renderSelecting st selecting =
    CSD.pickerDialog
      { onDismiss: HandleDismissSelecting
      , onConfirm: HandleConfirmSelecting
      , selection: selecting.value
      , isSelectable: const true
      , classes: []
      , title: [ HH.text (st.toLabel selecting.option).text ]
      , content: [ HH.slot' cp2 unit selecting.component selecting.value (HE.input HandleSelecting) ]
      }

  eval ∷ Query s ~> DSL s
  eval = case _ of
    Init next → do
      updateActions
      pure next
    HandleSelect (ALC.Selected mbOption) next → do
      st ← H.get
      let
        prev = fst <$> st.previousSelection
        curr = fst <$> st.selection
      case mbOption of
        Just option | curr ≠ mbOption →
          case st.toSelection option of
            Nothing → H.modify _ { selection = Just (option × option) }
            Just comp → do
              let
                value = fromMaybe option do
                  guard (prev ≡ mbOption)
                  snd <$> st.previousSelection
                selecting = { option, value: Just value, component: comp }
              H.modify _ { selecting = Just selecting }
        _ → when st.deselectable clearSelection
      updateActions
      pure next
    HandleSelecting value next → do
      H.modify \st → st { selecting = _ { value = value } <$> st.selecting }
      pure next
    HandleDismiss next →
      H.raise Dismiss $> next
    HandleDismissSelecting next → do
      H.modify _ { selecting = Nothing }
      updateActions
      pure next
    HandleConfirm selection next →
      H.raise (Confirm selection) $> next
    HandleConfirmSelecting value next →
      H.raise (Confirm (Just value)) $> next
    UpdateDimensions next →
      H.query' cp1 unit (H.action ALC.CalculateBoundingRect) $> next

mkAction ∷ forall s. Eq s ⇒ Maybe s → (s → Label) → s → ALC.Action (Maybe s)
mkAction selection toLabel action =
  let label = toLabel action
  in
    ALC.mkDo
      { name: label.text
      , description: label.text
      , icon: label.icon
      , highlighted: selection ≡ Just action
      , disabled: false
      , action: Just action
    }

updateActions ∷ ∀ s. Eq s ⇒ DSL s Unit
updateActions = do
  st ← H.get
  let toAction = mkAction (fst <$> st.selection) st.toLabel
  void $ H.query' cp1 unit $ H.action $ ALC.UpdateActions (toAction <$> st.options)

clearSelection ∷ ∀ s. DSL s Unit
clearSelection =
  H.modify \st → st
    { selection = Nothing
    , previousSelection = st.selection
    }
