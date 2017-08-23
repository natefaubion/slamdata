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

module SlamData.AdminUI.MySettings.Component where

import SlamData.Prelude

import Data.Argonaut as J
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import SlamData.LocalStorage.Class as LS
import SlamData.LocalStorage.Keys as LK
import SlamData.Monad (Slam)

data Query a
  = Init a
  | DefaultThemeChanged String a

type State =
  { homeDirectory ∷ String
  , isolateArtifacts ∷ Boolean
  , isolateArtifactsDirectory ∷ String
  , defaultTheme ∷ String
  }

defaultState ∷ State
defaultState =
  { homeDirectory: ""
  , isolateArtifacts: false
  , isolateArtifactsDirectory: ""
  , defaultTheme: "Dark"
  }

type Message = Void

type ChildSlot = Unit

type HTML = H.ComponentHTML Query
type DSL = H.ComponentDSL State Query Message Slam

component ∷ H.Component HH.HTML Query Unit Message Slam
component =
  H.component
    { render
    , eval
    , receiver: const Nothing
    , initialState: const defaultState
    }
    where
      render state =
        HH.div
          [ HP.class_ (HH.ClassName "sd-admin-ui-database") ]
          [ HH.fieldset
              [ HP.class_ (HH.ClassName "database-form-wrapper") ]
              (renderForm state)
          ]

      eval ∷ Query ~> DSL
      eval = case _ of
        Init next → do
          defaultTheme ← LS.retrieve J.decodeJson LK.adminUIDefaultTheme
          for_ defaultTheme \theme →
            H.modify (_ { defaultTheme = theme })
          pure next
        DefaultThemeChanged newTheme next → do
          LS.persist J.encodeJson LK.adminUIDefaultTheme newTheme
          H.modify (_ { defaultTheme = newTheme })
          pure next

themes ∷ Array String
themes = ["Dark", "Light"]

renderForm ∷ State → Array HTML
renderForm state =
    [ HH.fieldset
        [ HP.class_ (HH.ClassName "home-directory") ]
        [ HH.legend_ [ HH.text "Location of my home directory in the SlamData file system:" ]
        , HH.input
            [ HP.classes [ HH.ClassName "form-control" ]
            , HP.id_ "HomeDirectory"
            , HP.value state.homeDirectory
            ]
        , HH.div
            [ HP.classes [ HH.ClassName "form-group" ] ]
            [ HH.div
                [ HP.classes [ HH.ClassName "checkbox" ] ]
                [ HH.label_
                    [ HH.input
                        [ HP.checked state.isolateArtifacts
                        , HP.type_ HP.InputCheckbox
                        ]
                    , HH.text "Isolate SlamData artifacts to a specific location in the SlamData file system"
                    ]
                ]
            , HH.input
                [ HP.classes [ HH.ClassName "form-control" ]
                , HP.id_ "IsolateLocation"
                , HP.disabled (not state.isolateArtifacts)
                , HP.value state.isolateArtifactsDirectory
                ]
            , HH.p_
                [ HH.text $ fold
                    [ "If you choose this option, while you can still virtually locate decks anywhere inside the file system, "
                    , "they will always be physically stored in the above location. This allows you to keep production systems "
                    , "free of SlamData artifacts, while still centrally locating and backing them up."
                    ]
                ]
            ]
        ]
    , HH.fieldset
        [ HP.class_ (HH.ClassName "themes") ]
        [ HH.legend_ [ HH.text "Default theme for new decks:" ]
        , HH.div
            [ HP.class_ (HH.ClassName "theme-pickers") ]
            [ HH.select
                [ HP.classes [ HH.ClassName "form-control" ]
                , HP.id_ "ThemeSelection"
                , HE.onValueChange (HE.input DefaultThemeChanged)
                , HP.value state.defaultTheme
                ]
                (themes <#> \t → HH.option_ [HH.text t])
            ]
        ]
    ]
