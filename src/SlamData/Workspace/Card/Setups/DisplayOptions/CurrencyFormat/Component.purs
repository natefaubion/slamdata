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
module SlamData.Workspace.Card.Setups.DisplayOptions.CurrencyFormat.Component where

import SlamData.Prelude

import Data.String as Str
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import SlamData.Render.Form as RF
import SlamData.Workspace.Card.Setups.DisplayOptions.Common.Query as CQ
import SlamData.Workspace.Card.Setups.DisplayOptions.Common.Render as CR
import SlamData.Workspace.Card.Setups.DisplayOptions.DecimalFormat.Model as M

type Query = CQ.Query State

type State =
  { prefix ∷ String
  , suffix ∷ String
  , thousands ∷ String
  , decimal ∷ String
  , rounding ∷ M.RoundBehaviour
  , error ∷ Maybe String
  }

initialState ∷ State
initialState =
  { prefix: "$"
  , suffix: " USD"
  , thousands: ","
  , decimal: "."
  , rounding: M.Ceil
  , error: Nothing
  }

fromModel ∷ M.DecimalFormat → State
fromModel fmt =
  { prefix: fmt.prefix
  , suffix: fmt.suffix
  , thousands: fmt.thousands
  , decimal: fmt.decimal
  , rounding: fmt.rounding
  , error: Nothing
  }

toModel ∷ State → Either String M.DecimalFormat
toModel st = do
  when (Str.null st.decimal) $
    Left "A value must be provided for the decimal separator"
  pure
    { prefix: st.prefix
    , suffix: st.suffix
    , thousands: st.thousands
    , decimal: st.decimal
    , minPlaces: Just 2
    , maxPlaces: Just 2
    , rounding: st.rounding
    }

type HTML = H.ComponentHTML Query

component ∷ ∀ m. H.Component HH.HTML Query (Maybe M.DecimalFormat) (Maybe M.DecimalFormat) m
component =
  H.lifecycleComponent
    { render
    , eval: CQ.eval toModel
    , initialState: maybe initialState fromModel
    , receiver: const Nothing
    , initializer: Just (H.action CQ.Init)
    , finalizer: Nothing
    }

render ∷ State → HTML
render st =
  HH.div
    [ HP.class_ (H.ClassName "sd-display-options-currency") ]
    [ HH.div
        [ HP.class_ (H.ClassName "sd-display-options-currency-row") ]
        [ HH.label_
            [ HH.span_ [ HH.text "Prefix" ]
            , HH.input
                [ HP.class_ (H.ClassName "sd-form-input")
                , HP.type_ HP.InputText
                , HP.value st.prefix
                , HE.onValueInput $ HE.input (CQ.Modify ∘ flip (_ { prefix = _ }))
                ]
            ]
        , HH.label_
            [ HH.span_ [ HH.text "Suffix" ]
            , HH.input
                [ HP.class_ (H.ClassName "sd-form-input")
                , HP.type_ HP.InputText
                , HP.value st.suffix
                , HE.onValueInput $ HE.input (CQ.Modify ∘ flip (_ { suffix = _ }))
                ]
            ]
        ]
    , HH.div
        [ HP.class_ (H.ClassName "sd-display-options-currency-row") ]
        [ HH.label_
            [ HH.span_ [ HH.text "Thousands separator" ]
            , HH.input
                [ HP.class_ (H.ClassName "sd-form-input")
                , HP.type_ HP.InputText
                , HP.value st.thousands
                , HE.onValueInput $ HE.input (CQ.Modify ∘ flip (_ { thousands = _ }))
                ]
            ]
        , HH.label_
            [ HH.span_ [ HH.text "Decimal separator" ]
            , HH.input
                [ HP.class_ (H.ClassName "sd-form-input")
                , HP.type_ HP.InputText
                , HP.value st.decimal
                , HE.onValueInput $ HE.input (CQ.Modify ∘ flip (_ { decimal = _ }))
                ]
            ]
        ]
    , HH.label_
        [ HH.span_ [ HH.text "Rounding behaviour" ]
        , RF.renderSelect
            M.roundBehaviours
            st.rounding
            M.roundBehaviour
            (CQ.Modify ∘ flip (_ { rounding = _ }))
        ]
    , CR.renderError st.error
    ]
