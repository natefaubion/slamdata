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
module SlamData.Workspace.Card.Setups.DisplayOptions.DateTimeFormat.Component where

import SlamData.Prelude

import Data.Formatter.DateTime as FDT
import Data.Lens as Lens
import Data.List ((:))
import Data.List as L
import Data.List.NonEmpty as NEL
import Data.List.Safe as SL
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
import SlamData.Workspace.Card.Setups.DisplayOptions.TimeFormat.State as TF

type Query = CQ.Query State

type Side = Either Unit Unit

leftSide ∷ Side
leftSide = Left unit

rightSide ∷ Side
rightSide = Right unit

side ∷ Lens.Prism' String Side
side = Lens.prism' to from
  where
    to = case _ of
      Left _ → "Left"
      Right _ → "Right"
    from = case _ of
      "Left" → Just leftSide
      "Right" → Just rightSide
      _ → Nothing

sides ∷ NEL.NonEmptyList Side
sides = SL.toNEL $ leftSide SL.: rightSide SL.: SL.nil

type Options =
  { date ∷ P.Preset
  , time ∷ TF.State
  , timeSide ∷ Side
  , separator ∷ String
  }

initialOptions ∷ Options
initialOptions =
  { date: P.ISO8601
  , time: TF.initialState
  , timeSide: rightSide
  , separator: ", "
  }

optionsToModel ∷ Options → FDT.Formatter
optionsToModel opts =
  let
    datePart = P.presetFormat opts.date
    timePart = TF.toModel opts.time
    sepPart = FDT.Placeholder opts.separator
  in
    if opts.timeSide == leftSide
      then timePart <> sepPart : datePart
      else datePart <> sepPart : timePart


type State =
  { format ∷ Either Options String
  , error ∷ Maybe String
  }

initialState ∷ State
initialState =
  { format: Left initialOptions
  , error: Nothing
  }

fromModel ∷ FDT.Formatter → State
fromModel fmt =
  let
    inferredOptions = inferOptions fmt
    inferredFormat = optionsToModel inferredOptions
  in
    { format:
        if inferredFormat == fmt
          then Left inferredOptions
          else Right (FDT.printFormatter fmt)
    , error: Nothing
    }

toModel ∷ State → Either String FDT.Formatter
toModel st = case st.format of
  Left opts →
    Right (optionsToModel opts)
  Right custom → do
    when (Str.null (Str.trim custom)) $
      Left "The custom format cannot be left empty when the 'Custom' preset is selected"
    fmt ← FDT.parseFormatString custom #
      lmap ("There was a problem parsing the custom format: " <> _)
    pure fmt

updateFormat ∷ (Options → Options) → State → State
updateFormat f st = st { format = lmap f st.format }

toggleCustom ∷ Boolean → State → State
toggleCustom b st =
  if b
    then st { format = Right (either (FDT.printFormatter ∘ optionsToModel) id st.format) }
    else st { format = Left (either id inferOptions' st.format) }

inferOptions ∷ FDT.Formatter → Options
inferOptions fmt =
  let
    dateStart = L.findIndex P.isDateElement fmt
    dateEnd = L.findLastIndex P.isDateElement fmt
    timeStart = L.findIndex isTimeElement fmt
    timeEnd = L.findLastIndex isTimeElement fmt
  in
    { date: fromMaybe P.ISO8601 do
        ds ← dateStart
        de ← dateEnd
        M.lookup (L.slice ds (de + 1) fmt) P.presetsByFormat
    , time: fromMaybe TF.initialState do
        ts ← timeStart
        te ← timeEnd
        let time = L.slice ts (te + 1) fmt
        pure
          { hours24: L.elem FDT.Hours24 time
          , seconds: L.elem FDT.SecondsTwoDigits time
          }
    , timeSide:
        if isJust timeEnd && timeEnd < dateStart
          then leftSide
          else rightSide
    , separator:
        fromMaybe ", "
          $ join (extractPlaceholder <$> dateEnd <*> timeStart)
          <|> join (extractPlaceholder <$> timeEnd <*> dateStart)
    }
  where
    isTimeElement ∷ FDT.FormatterCommand → Boolean
    isTimeElement = not (P.isDateElement || P.isPlaceholder)
    extractPlaceholder ∷ Int → Int → Maybe String
    extractPlaceholder i j
      | j - i == 2 =
          case L.index fmt (i + 1) of
            Just (FDT.Placeholder s) → Just s
            _ → Nothing
      | otherwise = Nothing

inferOptions' ∷ String → Options
inferOptions' =
  either (const initialOptions) inferOptions ∘ FDT.parseFormatString

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
    [ HP.class_ (H.ClassName "sd-display-options-datetime") ]
    [ renderPresetDropdown (fromOptions _.date st.format)
    , renderTimeSide (fromOptions _.timeSide st.format)
    , renderTimeOptions (fromOptions _.time st.format)
    , renderDateTimeSeparator (fromOptions _.separator st.format)
    , renderCustomField uniqueId (either (Left ∘ FDT.printFormatter ∘ optionsToModel) Right st.format)
    , CR.renderError st.error
    ]
  where
    fromOptions ∷ ∀ a. (Options → a) → Either Options String → Either a a
    fromOptions f = either (Right ∘ f) (Left ∘ f ∘ inferOptions')

renderPresetDropdown ∷ Either P.Preset P.Preset → HTML
renderPresetDropdown p =
  HH.div
    [ HP.class_ CCN.row ]
    [ HH.label_
        [ HH.span
            [ HP.class_ RFCN.label ]
            [ HH.text "Date format" ]
        , RF.renderSelect'
            [ HP.enabled (isRight p) ]
            P.presets
            (either id id p)
            P.preset
            (CQ.Modify ∘ updateFormat ∘ flip (_ { date = _ }))
        ]
    ]

renderTimeSide ∷ Either Side Side → HTML
renderTimeSide value =
  HH.div
    [ HP.class_ CCN.row ]
    [ HH.label_
        [ HH.span
            [ HP.class_ RFCN.label ]
            [ HH.text "Show time on" ]
        , RF.renderSelect'
            [ HP.enabled (isRight value) ]
            sides
            (either id id value)
            side
            (CQ.Modify ∘ updateFormat ∘ flip (_ { timeSide = _ }))
        ]
    ]

renderTimeOptions ∷ Either TF.State TF.State → HTML
renderTimeOptions opts =
  HH.label
    [ HP.class_ CCN.row ]
    [ HH.span
        [ HP.class_ RFCN.label ]
        [ HH.text "Time options" ]
    , HH.ul
        [ HP.class_ RFCN.options ]
        [ HH.li_
            [ RF.renderCheckbox'
                [ HP.enabled (isRight opts) ]
                "24-hour clock"
                (either _.hours24 _.hours24 opts)
                (CQ.Modify ∘ updateFormat ∘ flip (_ { time { hours24 = _ } }))
            ]
        , HH.li_
            [ RF.renderCheckbox'
                [ HP.enabled (isRight opts) ]
                "Show seconds"
                (either _.seconds _.seconds opts)
                (CQ.Modify ∘ updateFormat ∘ flip (_ { time { seconds = _ } }))
            ]
        ]
    ]

renderDateTimeSeparator ∷ Either String String → HTML
renderDateTimeSeparator value =
  HH.label
    [ HP.class_ CCN.row ]
    [ HH.span
        [ HP.class_ RFCN.label ]
        [ HH.text "Date/time separator" ]
    , HH.input
        [ HP.class_ RFCN.input
        , HP.type_ HP.InputText
        , HP.value (either id id value)
        , HP.enabled (isRight value)
        , HE.onValueInput $ HE.input (CQ.Modify ∘ updateFormat ∘ flip (_ { separator = _ }))
        ]
    ]

renderCustomField ∷ String → Either String String → HTML
renderCustomField uniqueId value =
  let
    checkboxId = uniqueId <> "-custom-toggle"
  in
    HH.div
      [ HP.class_ CCN.row ]
      [ HH.label
          [ HP.for checkboxId ]
          [ HH.span
              [ HP.class_ RFCN.label ]
              [ HH.text "Custom format" ]
          ]
      , HH.div_
          [ HH.span
              [ HP.class_ RFCN.inputAddon ]
              [ HH.input
                  [ HP.id_ checkboxId
                  , HP.type_ HP.InputCheckbox
                  , HP.checked (isRight value)
                  , HE.onChecked $ HE.input (CQ.Modify ∘ toggleCustom)
                  ]
              ]
          , HH.input
              [ HP.class_ RFCN.input
              , HP.type_ HP.InputText
              , HP.value (either id id value)
              , HP.enabled (isRight value)
              , HE.onValueInput $ HE.input (CQ.Modify ∘ flip (\st v → st { format = Right v }))
              ]
          ]
      ]
