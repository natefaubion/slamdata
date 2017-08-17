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
module SlamData.Workspace.Card.Setups.DisplayOptions.DateFormat.Component where

import SlamData.Prelude

import Data.Foldable (any)
import Data.Formatter.DateTime as FDT
import Data.Map as M
import Data.String as Str
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import SlamData.Render.Form as RF
import SlamData.Workspace.Card.Setups.DisplayOptions.Common.Query as CQ
import SlamData.Workspace.Card.Setups.DisplayOptions.Common.Render as CR
import SlamData.Workspace.Card.Setups.DisplayOptions.DateFormat.Preset as P

type Query = CQ.Query State

type State =
  { preset ∷ P.Preset
  , custom ∷ String
  , error ∷ Maybe String
  }

initialState ∷ State
initialState =
  { preset: P.ISO8601
  , custom: ""
  , error: Nothing
  }

fromModel ∷ FDT.Formatter → State
fromModel fmt =
  case M.lookup fmt P.presetsByFormat of
    Nothing →
      { preset: P.Custom
      , custom: FDT.printFormatter fmt
      , error: Nothing
      }
    Just preset →
      { preset
      , custom: ""
      , error: Nothing
      }

toModel ∷ State → Either String FDT.Formatter
toModel st = case P.presetFormat st.preset of
  Nothing
    | Str.null (Str.trim st.custom) →
        Left "The custom format cannot be left empty when the 'Custom' preset is selected"
    | otherwise → do
        fmt ← FDT.parseFormatString st.custom #
          lmap ("There was a problem parsing the custom format: " <> _)
        when (any (not isDateElement) fmt) $
          Left ("The custom date format contains a time element")
        pure fmt
  Just fmt →
    pure fmt

isDateElement ∷ FDT.FormatterCommand → Boolean
isDateElement = case _ of
  FDT.YearFull → true
  FDT.YearTwoDigits → true
  FDT.YearAbsolute → true
  FDT.MonthFull → true
  FDT.MonthShort → true
  FDT.MonthTwoDigits → true
  FDT.DayOfMonthTwoDigits → true
  FDT.DayOfMonth → true
  FDT.DayOfWeek → true
  FDT.Placeholder _ → true
  _ → false

type HTML = H.ComponentHTML Query

component ∷ ∀ m. H.Component HH.HTML Query (Maybe FDT.Formatter) (Maybe FDT.Formatter) m
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
    [ HP.class_ (H.ClassName "sd-display-options-date") ]
    $ join
        [ pure $ renderPresetDropdown st
        , guard (st.preset == P.Custom) $> renderCustomField st
        , pure $ CR.renderError st.error
        ]

renderPresetDropdown ∷ State → HTML
renderPresetDropdown st =
  HH.label_
    [ HH.span_ [ HH.text "Format preset" ]
    , RF.renderSelect P.presets st.preset P.preset (CQ.Modify ∘ changePreset)
    ]

renderCustomField ∷ State → HTML
renderCustomField st =
  HH.label_
    [ HH.span_ [ HH.text "Custom format" ]
    , HH.input
        [ HP.class_ (H.ClassName "sd-form-input")
        , HP.type_ HP.InputText
        , HP.value st.custom
        , HE.onValueInput $ HE.input (CQ.Modify ∘ flip (_ { custom = _ }))
        ]
    ]

changePreset ∷ P.Preset → State → State
changePreset preset st =
  st
    { preset = preset
    , custom =
        if preset == P.Custom
        then maybe st.custom FDT.printFormatter (P.presetFormat st.preset)
        else ""
    }
