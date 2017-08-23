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
module SlamData.Workspace.Card.Setups.DisplayOptions.IntegerFormat.Component where

import SlamData.Prelude

import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import SlamData.Render.Form as RF
import SlamData.Render.Form.ClassNames as RFCN
import SlamData.Workspace.Card.Setups.DisplayOptions.Common.ClassNames as CCN
import SlamData.Workspace.Card.Setups.DisplayOptions.Common.Query as CQ
import SlamData.Workspace.Card.Setups.DisplayOptions.IntegerFormat.Model as M

type Query = CQ.Query M.IntegerFormat

initialFormat ∷ M.IntegerFormat
initialFormat = { prefix: "", suffix: "", thousands: ",", rounding: M.Round }

type HTML = H.ComponentHTML Query

component ∷ ∀ m. H.Component HH.HTML Query (Maybe M.IntegerFormat) (Maybe M.IntegerFormat) m
component =
  H.lifecycleComponent
    { render
    , eval: CQ.evalInfallible id
    , initialState: fromMaybe initialFormat
    , receiver: const Nothing
    , initializer: Just (H.action CQ.Init)
    , finalizer: Nothing
    }

render ∷ M.IntegerFormat → HTML
render { prefix, suffix, thousands, rounding } =
  HH.div
    [ HP.class_ (H.ClassName "sd-display-options-integer") ]
    [ HH.div
        [ HP.class_ CCN.row ]
        [ HH.label
            [ HP.class_ CCN.col ]
            [ HH.span
                [ HP.class_ RFCN.label ]
                [ HH.text "Prefix" ]
            , HH.input
                [ HP.class_ RFCN.input
                , HP.type_ HP.InputText
                , HP.value prefix
                , HE.onValueInput $ HE.input (CQ.Modify ∘ flip (_ { prefix = _ }))
                ]
            ]
        , HH.label
            [ HP.class_ CCN.col ]
            [ HH.span
                [ HP.class_ RFCN.label ]
                [ HH.text "Suffix" ]
            , HH.input
                [ HP.class_ RFCN.input
                , HP.type_ HP.InputText
                , HP.value suffix
                , HE.onValueInput $ HE.input (CQ.Modify ∘ flip (_ { suffix = _ }))
                ]
            ]
        ]
    , HH.div
        [ HP.class_ CCN.row ]
        [ HH.label
            [ HP.class_ CCN.col ]
            [ HH.span
                [ HP.class_ RFCN.label ]
                [ HH.text "Thousands separator" ]
            , HH.input
                [ HP.class_ RFCN.input
                , HP.type_ HP.InputText
                , HP.value thousands
                , HE.onValueInput $ HE.input (CQ.Modify ∘ flip (_ { thousands = _ }))
                ]
            ]
        , HH.label
            [ HP.class_ CCN.col ]
            [ HH.span
                [ HP.class_ RFCN.label ]
                [ HH.text "Rounding behaviour" ]
            , RF.renderSelect
                M.roundBehaviours
                rounding
                M.roundBehaviour
                (CQ.Modify ∘ flip (_ { rounding = _ }))
            ]
        ]
    ]
