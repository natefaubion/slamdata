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
import Data.Lens as Lens
import Data.List.NonEmpty as NEL
import Data.Map as M
import Data.String as Str
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import SlamData.Render.Form as RF
import SlamData.Render.Form.ClassNames as RFCN
import SlamData.Workspace.Card.Setups.DisplayOptions.Common.ClassNames as CCN
import SlamData.Workspace.Card.Setups.DisplayOptions.Common.Query as CQ
import SlamData.Workspace.Card.Setups.DisplayOptions.Common.Render as CR
import SlamData.Workspace.Card.Setups.DisplayOptions.DateFormat.Preset as P

type Query = CQ.Query State

type State =
  { format ∷ Either String P.Preset
  , error ∷ Maybe String
  }

initialState ∷ State
initialState =
  { format: Right P.ISO8601
  , error: Nothing
  }

fromModel ∷ FDT.Formatter → State
fromModel fmt =
  { format:
      maybe'
        (\_ → Left (FDT.printFormatter fmt))
        Right
        (M.lookup fmt P.presetsByFormat)
  , error: Nothing
  }

toModel ∷ State → Either String FDT.Formatter
toModel st = case st.format of
  Right preset →
    pure (P.presetFormat preset)
  Left custom → do
    when (Str.null (Str.trim custom)) $
      Left "The custom format cannot be left empty when the 'Custom' preset is selected"
    fmt ← FDT.parseFormatString custom #
      lmap ("There was a problem parsing the custom format: " <> _)
    when (any (not (P.isDateElement || P.isPlaceholder)) fmt) $
      Left ("The custom date format contains a time element")
    pure fmt

type HTML = H.ComponentHTML Query

component ∷ ∀ m. String → H.Component HH.HTML Query (Maybe FDT.Formatter) (Maybe FDT.Formatter) m
component uniqueId =
  H.lifecycleComponent
    { render: render uniqueId
    , eval: CQ.eval toModel
    , initialState: maybe initialState fromModel
    , receiver: const Nothing
    , initializer: Just (H.action CQ.Init)
    , finalizer: Nothing
    }

render ∷ String → State → HTML
render uniqueId st =
  HH.div
    [ HP.class_ (H.ClassName "sd-display-options-date") ]
    $ join
        [ pure $ renderPresetDropdown (either (const Nothing) Just st.format)
        , either (pure ∘ renderCustomField) mempty st.format
        , pure $ CR.renderError st.error
        ]

renderPresetDropdown ∷ Maybe P.Preset → HTML
renderPresetDropdown preset =
  HH.div
    [ HP.class_ CCN.row ]
    [ HH.label_
        [ HH.span
            [ HP.class_ RFCN.label ]
            [ HH.text "Format preset" ]
        , RF.renderSelect mpresets preset mpreset (CQ.Modify ∘ changePreset)
        ]
    ]

renderCustomField ∷ String → HTML
renderCustomField custom =
  HH.div
    [ HP.class_ CCN.row ]
    [ HH.label_
        [ HH.span
            [ HP.class_ RFCN.label ]
            [ HH.text "Custom format" ]
        , HH.input
            [ HP.class_ RFCN.input
            , HP.type_ HP.InputText
            , HP.value custom
            , HE.onValueInput $
                HE.input (CQ.Modify ∘ \v st → st { format = Left v })
            ]
        ]
    ]

changePreset ∷ Maybe P.Preset → State → State
changePreset preset st =
  st
    { format =
        maybe
          (either Left (Left ∘ FDT.printFormatter ∘ P.presetFormat) st.format)
          Right
          preset
    }

-- | In "the algebra of algebraic data types" `Maybe t` is `t + 1`, we're using
-- | this to extend the `Preset` label-prism with one extra option (`Nothing`)
-- | which is used to represent the custom format choice.
mpreset ∷ Lens.Prism' String (Maybe P.Preset)
mpreset = Lens.prism' to from
  where
    to ∷ Maybe P.Preset → String
    to = case _ of
      Just p → Lens.review P.preset p
      Nothing → "Custom"
    from ∷ String → Maybe (Maybe P.Preset)
    from s = (Just <$> Lens.preview P.preset s)
      <|> case s of
        "Custom" → Just Nothing
        _ → Nothing

mpresets ∷ NEL.NonEmptyList (Maybe P.Preset)
mpresets = map Just P.presets `NEL.snoc` Nothing
