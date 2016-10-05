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

module SlamData.Workspace.Card.BuildChart.DimensionPicker.Component where

import SlamData.Prelude

import Data.List (List, (:))
import Data.List as List

import Halogen as H
import Halogen.HTML.Events.Indexed as HE
import Halogen.HTML.Indexed as HH
import Halogen.HTML.Properties.Indexed as HP
import Halogen.HTML.Properties.Indexed.ARIA as ARIA
import Halogen.Themes.Bootstrap3 as B

import SlamData.Monad (Slam)
import SlamData.Workspace.MillerColumns.Component as MC
import SlamData.Workspace.MillerColumns.TreeData as MCT

data Query s a
  = Dismiss a
  | Confirm (List s) a

type State s =
  { selection ∷ Maybe (List s)
  }

initialState ∷ ∀ s. State s
initialState =
  { selection: Nothing
  }

type ChildState s = MC.State s s

type ChildQuery s = MC.Query s

type ChildSlot = Unit

type StateP s =
  H.ParentState
    (State s)
    (ChildState s)
    (Query s)
    (ChildQuery s)
    Slam
    ChildSlot

type QueryP s =
  H.ParentQuery
    (Query s)
    (ChildQuery s)
    ChildSlot

type HTML s =
  H.ParentHTML
    (ChildState s)
    (Query s)
    (ChildQuery s)
    Slam
    ChildSlot

type DSL s =
  H.ParentDSL
    (State s)
    (ChildState s)
    (Query s)
    (ChildQuery s)
    Slam
    ChildSlot

type PickerOptions s =
  { label  ∷ s → String
  , render ∷ s → MC.ItemHTML
  , title  ∷ String
  , values ∷ MCT.Tree s
  }

pickerOptionsToItemSpec ∷ ∀ s. Eq s ⇒ PickerOptions s → MC.ItemSpec s s Slam
pickerOptionsToItemSpec { label, render, values } =
  { label: label
  , render: render
  , load: MCT.loadFromTree id values
  , id: id
  }

picker
  ∷ ∀ s
  . Eq s
  ⇒ PickerOptions s
  → H.Component (StateP s) (QueryP s) Slam
picker opts = H.parentComponent { render, eval, peek: Just (peek ∘ H.runChildF) }
  where
  itemSpec = pickerOptionsToItemSpec opts
  render ∷ State s → HTML s
  render st =
    HH.div
      [ HP.classes [ HH.className "sd-dimension-picker" ] ]
      [ HH.div
          [ HP.classes [ HH.className "sd-dimension-picker-title" ] ]
          [ HH.h1_ [ HH.text opts.title ]
          , HH.button
              [ HP.classes [ HH.className "sd-dismiss-button" ]
              , HP.title "Dismiss"
              , ARIA.label "Dismiss"
              , HE.onClick (HE.input_ Dismiss)
              ]
              [ HH.text "×"]
          ]
      , HH.div
          [ HP.classes [ HH.className "sd-dimension-picker-content" ] ]
          [ HH.slot unit \_ →
              { component: MC.component itemSpec Nothing
              , initialState: MCT.initialStateFromTree itemSpec.id opts.values
              }
          ]
      , HH.div
          [ HP.classes [ HH.className "sd-dimension-picker-toolbar" ] ]
          [ HH.button
              [ HP.classes [ B.btn, B.btnDefault ]
              , ARIA.label "Dismiss"
              , HE.onClick (HE.input_ Dismiss)
              ]
              [ HH.text "Dismiss" ]
          , HH.button
              ([ HP.classes [ B.btn, B.btnPrimary ]
              , ARIA.label ""
              ] <>
                case st.selection of
                  Just sel → [ HE.onClick (HE.input_ (Confirm (List.reverse sel))) ]
                  Nothing → [ HP.disabled true ])
              [ HH.text "Confirm" ]
          ]
      ]

  eval ∷ Query s ~> DSL s
  eval = case _ of
    Dismiss next → pure next
    Confirm _ next → pure next

  peek ∷ ∀ x. MC.Query s x -> DSL s Unit
  peek = case _ of
    MC.Populate sel _ →
      H.modify (_ { selection = Just sel })
    _ →
      pure unit
