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
module SlamData.Workspace.Card.Setups.DisplayOptions.DecimalFormat.Component where

import SlamData.Prelude

import Data.Int as Int
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
  , pad ∷ Boolean
  , minPlaces ∷ String
  , round ∷ Boolean
  , maxPlaces ∷ String
  , rounding ∷ M.RoundBehaviour
  , error ∷ Maybe String
  }

initialState ∷ State
initialState =
  { prefix: ""
  , suffix: ""
  , thousands: ","
  , decimal: "."
  , pad: false
  , minPlaces: ""
  , round: false
  , maxPlaces: ""
  , rounding: M.Round
  , error: Nothing
  }

fromModel ∷ M.DecimalFormat → State
fromModel fmt =
  { prefix: fmt.prefix
  , suffix: fmt.suffix
  , thousands: fmt.thousands
  , decimal: fmt.decimal
  , pad: isJust fmt.minPlaces
  , minPlaces: maybe "" show fmt.minPlaces
  , round: isJust fmt.maxPlaces
  , maxPlaces: maybe "" show fmt.maxPlaces
  , rounding: fmt.rounding
  , error: Nothing
  }

toModel ∷ State → Either String M.DecimalFormat
toModel st = do
  when (Str.null st.decimal) $
    Left "A value must be provided for the decimal separator"
  minPlaces ← validatePlaces st.pad st.minPlaces "Minimum decimal places"
  maxPlaces ← validatePlaces st.round st.maxPlaces "Maximum decimal places"
  when (isJust maxPlaces && isJust minPlaces && maxPlaces < minPlaces) $
    Left "Maximum decimal places must be greater than or equal to minimum decimal places"
  pure
    { prefix: st.prefix
    , suffix: st.suffix
    , thousands: st.thousands
    , decimal: st.decimal
    , minPlaces
    , maxPlaces
    , rounding: st.rounding
    }
  where
    validatePlaces ∷ Boolean → String → String → Either String (Maybe Int)
    validatePlaces false _ _ = Right Nothing
    validatePlaces true s name = do
      n ← Int.fromString s #
        note (name <> " must be a number")
      when (n < 0) $
        Left (name <> " must be a positive number")
      pure (Just n)

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
    [ HP.class_ (H.ClassName "sd-display-options-decimal") ]
    $   [ renderPrefixSuffix st
        , renderSeparators st
        , HH.label_
            [ HH.span_ [ HH.text "Minimum decimal places" ]
            , HH.div
                [ HP.class_ (H.ClassName "sd-form-input-group") ]
                [ HH.span
                    [ HP.class_ (H.ClassName "sd-form-input-addon") ]
                    [ HH.input
                        [ HP.type_ HP.InputCheckbox
                        , HP.checked st.pad
                        , HE.onChecked $ HE.input (CQ.Modify ∘ flip (_ { pad = _ }))
                        ]
                    ]
                , HH.input
                    [ HP.class_ (H.ClassName "sd-form-input")
                    , HP.type_ HP.InputNumber
                    , HP.pattern "[0-9]"
                    , HP.value st.minPlaces
                    , HP.enabled st.pad
                    , HE.onValueInput $ HE.input (CQ.Modify ∘ flip (_ { minPlaces = _ }))
                    ]
                ]
            ]
        -- TODO: move inputs out of label for these cases, and link to the checkbox input
        , HH.label_
            [ HH.span_ [ HH.text "Maximum decimal places" ]
            , HH.div
                [ HP.class_ (H.ClassName "sd-form-input-group") ]
                [ HH.span
                    [ HP.class_ (H.ClassName "sd-form-input-addon") ]
                    [ HH.input
                        [ HP.type_ HP.InputCheckbox
                        , HP.checked st.round
                        , HE.onChecked $ HE.input (CQ.Modify ∘ flip (_ { round = _ }))
                        ]
                    ]
                , HH.input
                    [ HP.class_ (H.ClassName "sd-form-input")
                    , HP.type_ HP.InputNumber
                    , HP.pattern "[0-9]"
                    , HP.value st.maxPlaces
                    , HP.enabled st.round
                    , HE.onValueInput $ HE.input (CQ.Modify ∘ flip (_ { maxPlaces = _ }))
                    ]
                , if st.round
                  then
                    RF.renderSelect'
                      [ HP.enabled st.round ]
                      M.roundBehaviours
                      st.rounding
                      M.roundBehaviour
                      (CQ.Modify ∘ flip (_ { rounding = _ }))
                  else
                    HH.text ""
                ]
            ]
        , CR.renderError st.error
        ]

renderPrefixSuffix ∷ State → HTML
renderPrefixSuffix st =
  HH.div
    [ HP.class_ (H.ClassName "sd-display-options-decimal-row") ]
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

renderSeparators ∷ State → HTML
renderSeparators st =
  HH.div
    [ HP.class_ (H.ClassName "sd-display-options-decimal-row") ]
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
