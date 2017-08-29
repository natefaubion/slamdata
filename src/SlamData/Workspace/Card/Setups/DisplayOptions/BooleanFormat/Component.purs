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
module SlamData.Workspace.Card.Setups.DisplayOptions.BooleanFormat.Component where

import SlamData.Prelude

import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import SlamData.Render.Form.ClassNames as RFCN
import SlamData.Workspace.Card.Setups.DisplayOptions.BooleanFormat.Model as M
import SlamData.Workspace.Card.Setups.DisplayOptions.Common.ClassNames as CCN
import SlamData.Workspace.Card.Setups.DisplayOptions.Common.Query as CQ

type Query = CQ.Query M.BooleanFormat

initialFormat ∷ M.BooleanFormat
initialFormat = { true: "True", false: "False" }

type HTML = H.ComponentHTML Query

component ∷ ∀ m. H.Component HH.HTML Query (Maybe M.BooleanFormat) (Maybe M.BooleanFormat) m
component =
  H.lifecycleComponent
    { render
    , eval: CQ.evalInfallible id
    , initialState: fromMaybe initialFormat
    , receiver: const Nothing
    , initializer: Just (H.action CQ.Init)
    , finalizer: Nothing
    }

render ∷ M.BooleanFormat → HTML
render st =
  HH.div
    [ HP.class_ (H.ClassName "sd-display-options-boolean") ]
    [ HH.div
        [ HP.class_ CCN.row ]
        [ HH.label_
            [ HH.span
                [ HP.class_ RFCN.label ]
                [ HH.text "True value" ]
            , HH.input
                [ HP.class_ RFCN.input
                , HP.type_ HP.InputText
                , HP.value st.true
                , HE.onValueInput $ HE.input (CQ.Modify ∘ flip (_ { true = _ }))
                ]
            ]
        ]
    , HH.div
        [ HP.class_ CCN.row ]
        [ HH.label_
            [ HH.span
                [ HP.class_ RFCN.label ]
                [ HH.text "False value" ]
            , HH.input
                [ HP.class_ RFCN.input
                , HP.type_ HP.InputText
                , HP.value st.false
                , HE.onValueInput $ HE.input (CQ.Modify ∘ flip (_ { false = _ }))
                ]
            ]
        ]
    ]
