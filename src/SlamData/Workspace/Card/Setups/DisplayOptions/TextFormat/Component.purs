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
module SlamData.Workspace.Card.Setups.DisplayOptions.TextFormat.Component where

import SlamData.Prelude

import Data.Int as Int
import Data.String as Str
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import SlamData.Workspace.Card.Setups.DisplayOptions.Common.Query as CQ
import SlamData.Workspace.Card.Setups.DisplayOptions.Common.Render as CR
import SlamData.Workspace.Card.Setups.DisplayOptions.TextFormat.Model as M

type Query = CQ.Query State

type State =
  { maxLength ∷ String
  , truncated ∷ String
  , error ∷ Maybe String
  }

initialState ∷ State
initialState =
  { maxLength: "100"
  , truncated: "..."
  , error: Nothing
  }

fromModel ∷ M.TextFormat → State
fromModel { maxLength, truncated } =
  { maxLength: show maxLength
  , truncated
  , error: Nothing
  }

toModel ∷ State → Either String M.TextFormat
toModel st = do
  maxLength ← Int.fromString st.maxLength #
    note "Maximum length must be a number"
  when (maxLength < 1) $
    Left "Maximum length must be a positive number greater than zero"
  when (Str.length st.truncated > maxLength) $
    Left "Truncation mark must be shorter than the maximum allowable length"
  pure { maxLength, truncated: st.truncated }

type HTML = H.ComponentHTML Query

component ∷ ∀ m. H.Component HH.HTML Query (Maybe M.TextFormat) (Maybe M.TextFormat) m
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
render { maxLength, truncated, error } =
  HH.div
    [ HP.class_ (H.ClassName "sd-display-options-text") ]
    [ HH.label_
        [ HH.span_ [ HH.text "Maximum length" ]
        , HH.input
            [ HP.class_ (H.ClassName "sd-form-input")
            , HP.type_ HP.InputNumber
            , HP.pattern "[0-9]"
            , HP.value maxLength
            , HE.onValueInput $ HE.input (CQ.Modify ∘ flip (_ { maxLength = _ }))
            ]
        ]
    , HH.label_
        [ HH.span_ [ HH.text "Truncation mark" ]
        , HH.input
            [ HP.class_ (H.ClassName "sd-form-input")
            , HP.type_ HP.InputText
            , HP.value truncated
            , HE.onValueInput $ HE.input (CQ.Modify ∘ flip (_ { truncated = _ }))
            ]
        ]
    , CR.renderError error
    ]
