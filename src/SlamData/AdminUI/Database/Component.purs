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

module SlamData.AdminUI.Database.Component where

import SlamData.Prelude

import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import SlamData.Monad (Slam)

data Query a
  = Init a
  | ToggleExternal Boolean a

type PostgresCon =
  { server ∷ String
  , port ∷ Int
  , username ∷ String
  , password ∷ String
  , database ∷ String
  , custom ∷ Tuple String String
  }

defaultPostgresCon ∷ PostgresCon
defaultPostgresCon =
  { server: "localhost"
  , port: 5432
  , username: ""
  , password: ""
  , database: ""
  , custom: Tuple "" ""
  }

type State =
  { isExternal ∷ Boolean
  , databaseFile ∷ String
  , postgresCon ∷ PostgresCon
  }

defaultState ∷ State
defaultState =
  { isExternal: false
  , databaseFile: ""
  , postgresCon: defaultPostgresCon
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
           -- TODO(Christoph): Consider slicing the state up
           (renderInternal state <> renderExternal state)
          ]

      eval ∷ Query ~> DSL
      eval = case _ of
        Init next → do
          pure next
        ToggleExternal b next → do
          H.modify (_ { isExternal = b })
          pure next

renderInternal ∷ State → Array HTML
renderInternal state =
  [ HH.legend
      [ HP.class_ (HH.ClassName "radio") ]
      [ HH.label_
          [ HH.input
              [ HP.checked (not state.isExternal)
              , HE.onChecked (HE.input_ (ToggleExternal false))
              , HP.type_ HP.InputRadio
              ]
          , HH.text "Store SlamData metadata inside internal database in the local file system of the server"
          ]
      ]
  , HH.fieldset
      [ HP.class_ (HH.ClassName "internal-storage")
      , HP.disabled state.isExternal
      ]
      [ HH.input
          [ HP.classes [ HH.ClassName "form-control" ]
          , HP.value state.databaseFile
          ]
      ]
  ]

renderExternal ∷ State → Array HTML
renderExternal state  =
  [ HH.legend
      [ HP.class_ (HH.ClassName "radio") ]
      [ HH.label_
          [ HH.input
              [ HP.checked state.isExternal
              , HE.onChecked (HE.input_ (ToggleExternal true))
              , HP.type_ HP.InputRadio
              ]
          , HH.text "Store SlamData metadata inside external PostgreSQL"
          ]
      ]
    , HH.fieldset
        [ HP.class_ (HH.ClassName "external-storage")
        , HP.disabled (not state.isExternal)
        ]
        [ HH.div
            [ HP.class_ (HH.ClassName "form-group") ]
            [ HH.label [ HP.for "Server" ] [ HH.text "Server" ]
            , HH.input
                [ HP.classes [ HH.ClassName "form-control" ]
                , HP.id_ "Server"
                , HP.value state.postgresCon.server
                ]
            ]
        , HH.div
            [ HP.class_ (HH.ClassName "form-group") ]
            [ HH.label [ HP.for "Port" ] [ HH.text "Port" ]
            , HH.input
                [ HP.classes [ HH.ClassName "form-control" ]
                , HP.id_ "Port"
                , HP.value (show state.postgresCon.port)
                ]
            ]
        , HH.div
            [ HP.class_ (HH.ClassName "form-group") ]
            [ HH.label
                [ HP.for "Username" ]
                [ HH.text "Username"
                , HH.input
                    [ HP.classes [ HH.ClassName "form-control" ]
                    , HP.id_ "Username"
                    , HP.value state.postgresCon.username
                    ]
                ]
            , HH.label
                [ HP.for "Password" ]
                [ HH.text "Password"
                , HH.input
                    [ HP.classes [ HH.ClassName "form-control" ]
                    , HP.id_ "Password"
                    , HP.value state.postgresCon.password
                    ]
                ]
            ]
        , HH.div
            [ HP.class_ (HH.ClassName "form-group") ]
            [ HH.label [ HP.for "Database" ] [HH.text "Database"] , HH.input
                [ HP.classes [ HH.ClassName "form-control" ]
                , HP.id_ "Database"
                , HP.value state.postgresCon.database
                ]
            ]
        , HH.div
            [ HP.class_ (HH.ClassName "form-group") ]
            [ HH.label [ HP.for "Custom" ] [HH.text "Custom"]
            , HH.input
                [ HP.classes [ HH.ClassName "form-control" ]
                , HP.value (fst state.postgresCon.custom)
                ]
            , HH.input
                [ HP.classes [ HH.ClassName "form-control" ]
                , HP.value (snd state.postgresCon.custom)
                ]
            ]
        ]
    ]
