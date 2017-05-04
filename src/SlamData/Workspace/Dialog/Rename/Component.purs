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

module SlamData.Workspace.Dialog.Rename.Component where

import SlamData.Prelude

import Halogen as H
import Halogen.HTML.Events as HE
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Halogen.HTML.Properties.ARIA as ARIA
import Halogen.Themes.Bootstrap3 as B

import SlamData.Monad (Slam)
import Utils.DOM as DOM

type State = { name ∷ String }

data Query a
  = UpdateName String a
  | PreventDefaultAndSave DOM.Event a
  | Cancel a

data Message
  = Dismiss
  | Rename String

component ∷ H.Component HH.HTML Query String Message Slam
component =
  H.component
    { initialState: { name: _ }
    , render
    , eval
    , receiver: const Nothing
    }

render ∷ State → H.ComponentHTML Query
render { name } =
  HH.form
    [ HP.classes [ HH.ClassName "deck-dialog-rename" ]
    , HE.onSubmit (HE.input PreventDefaultAndSave)
    ]
    [ HH.h4_ [ HH.text  "Rename deck" ]
    , HH.div
        [ HP.classes [ HH.ClassName "deck-dialog-body" ] ]
        [ HH.div
            [ HP.classes [ B.formGroup ] ]
            [ HH.input
                [ HE.onValueInput (HE.input UpdateName)
                , HP.value name
                , HP.type_ HP.InputText
                , HP.classes [ B.formControl ]
                , ARIA.label "Deck name"
                ]
            ]
        ]
    , HH.div
        [ HP.classes [ HH.ClassName "deck-dialog-footer" ] ]
        [ HH.button
            [ HP.classes [ B.btn, B.btnDefault ]
            , HE.onClick (HE.input_ Cancel)
            , HP.type_ HP.ButtonButton
            ]
            [ HH.text "Dismiss" ]
        , HH.button
            [ HP.classes [ B.btn, B.btnPrimary ] ]
            [ HH.text "Save" ]
        ]
    ]

eval ∷ Query ~> H.ComponentDSL State Query Message Slam
eval = case _ of
  UpdateName name next →
    H.modify _ { name = name } $> next
  PreventDefaultAndSave ev next → do
    H.liftEff (DOM.preventDefault ev)
    H.raise ∘ Rename =<< H.gets _.name
    pure next
  Cancel next →
    H.raise Dismiss $> next
