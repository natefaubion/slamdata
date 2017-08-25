{-
Copyright 2016 SlamData, Inc.

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

module SlamData.Workspace.Card.Setups.Inputs where

import SlamData.Prelude

import Data.Argonaut as J
import Data.Array as Array
import Data.Lens ((^.))
import Data.List as List

import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.HTML.Properties.ARIA as ARIA

import SlamData.Form.Select (Select(..), stringVal, class OptionVal)
import SlamData.Render.ClassName as CN
import SlamData.Render.Icon as I
import SlamData.Workspace.Card.Setups.Dimension as D
import SlamData.Workspace.Card.Setups.DimensionPicker.Component as DPC
import SlamData.Workspace.Card.Setups.DimensionPicker.JCursor (groupJCursors)
import SlamData.Workspace.Card.Setups.Transform as T

import Utils (showPrettyJCursor, showJCursorTip)
import Utils.DOM as DOM

type Select' a = Boolean × Select a

data SelectAction a
  = Open (Array a)
  | Choose (Maybe a)

type PickerConfig a i =
  SelectConfig
    ( showValue ∷ a → String
    , query ∷ SelectAction a → H.Action i
    )

type PickerOptions a f =
  { options ∷ Array a
  , select ∷ f (Const Unit)
  }

type AggregationConfig a i =
  SelectConfig
    ( query ∷ SelectAction a → H.Action i
    )

type SelectConfig r =
  { disableWhen ∷ Int → Boolean
  , defaultWhen ∷ Int → Boolean
  , ariaLabel ∷ Maybe String
  , defaultOption ∷ String
  | r
  }

primary
  ∷ ∀ i
  . Maybe String
  → (SelectAction J.JCursor → H.Action i)
  → PickerConfig J.JCursor i
primary lbl =
  { disableWhen: (_ < 2)
  , defaultWhen: (_ < 1)
  , ariaLabel: lbl
  , defaultOption: "Choose " ⊕ fromMaybe "source" lbl
  , showValue: showPrettyJCursor
  , query: _
  }

secondary
  ∷ ∀ i
  . Maybe String
  → (SelectAction J.JCursor → H.Action i)
  → PickerConfig J.JCursor i
secondary lbl =
  { disableWhen: (_ < 1)
  , defaultWhen: const true
  , ariaLabel: lbl
  , defaultOption: "Choose " ⊕ fromMaybe "source" lbl
  , showValue: show
  , query: _
  }

dropdown
  ∷ ∀ a i
  . OptionVal a
  ⇒ Maybe String
  → (SelectAction a → H.Action i)
  → PickerConfig a i
dropdown =
  { disableWhen: (_ < 1)
  , defaultWhen: const false
  , ariaLabel: _
  , defaultOption: ""
  , showValue: stringVal
  , query: _
  }

aggregation ∷ ∀ a i. OptionVal a ⇒ Maybe String → (SelectAction a → H.Action i) → PickerConfig a i
aggregation =
  { disableWhen: (_ < 1)
  , defaultWhen: const false
  , ariaLabel: _
  , defaultOption: ""
  , showValue: stringVal
  , query: _
  }

pickerInput
  ∷ ∀ a i p
  . PickerConfig a i
  → Select a
  → H.HTML p i
pickerInput conf (Select { options, value }) =
  let
    len = Array.length options
    isDefault = isNothing value
    isDisabled = conf.disableWhen len
  in
   HH.div [ HP.classes [ HH.ClassName "sd-picker-input" ] ]
     [ HH.button
         ([ HP.classes
            $ [ CN.formControl, HH.ClassName "sd-picker-main-button" ]
            ⊕ ( HH.ClassName "default" <$ guard isDefault )
          , HP.disabled isDisabled
          , ARIA.label (fromMaybe "" conf.ariaLabel)
          ]
          <> (HE.onClick (HE.input_ (conf.query (Open options))) <$ guard (not isDisabled)))
         ( foldMap (pure ∘ HH.p_ ∘ pure ∘ HH.text ∘ flip append ":") (value *> conf.ariaLabel)
           ⊕ [ HH.text (maybe conf.defaultOption conf.showValue (guard (not isDefault) *> value)) ]
         )
     , if conf.defaultWhen len && not isDefault
          then
            HH.button
              [ HP.classes [ HH.ClassName "sd-dismiss-button" ]
              , HE.onClick (HE.input_ (conf.query (Choose Nothing)))
              , HP.disabled isDisabled
              ]
              [ I.closeSm ]
          else
            HH.text ""
      ]

selectInput
  ∷ ∀ a i p
  . Eq a
  ⇒ PickerConfig a i
  → Select a
  → H.HTML p i
selectInput conf (Select { options, value }) =
  let
    len = Array.length options
    defaultWhen = conf.defaultWhen len
    isDefault = isNothing value
    isDisabled = conf.disableWhen len
    renderedOptions = map renderOption options
  in
    HH.select
      [ HP.classes [ CN.formControl ]
      , HP.disabled isDisabled
      , HE.onSelectedIndexChange \ix →
          case defaultWhen, ix of
            true, 0 → Just (conf.query (Choose Nothing) unit)
            true, _ → Array.index options (ix - 1) <#> \val → conf.query (Choose (Just val)) unit
            _   , _ → Array.index options ix <#> \val → conf.query (Choose (Just val)) unit
      ]
      if defaultWhen
        then
          [ HH.option
              [ HP.selected isDefault ]
              [ HH.text conf.defaultOption ]
          ] ⊕ renderedOptions
        else
          renderedOptions

  where
  renderOption option =
    HH.option
      [ HP.selected (Just option ≡ value) ]
      [ HH.text (conf.showValue option) ]

pickerWithSelect
  ∷ ∀ a b i p
  . Eq b
  ⇒ PickerConfig a i
  → Select a
  → PickerConfig b i
  → Select b
  → H.HTML p i
pickerWithSelect conf1 sel1@(Select { options, value }) conf2 sel2 =
  let
    isDisabled = conf1.disableWhen (Array.length options)
  in
    HH.div [ HP.classes [ HH.ClassName "sd-picker-with-select" ] ]
      [ pickerInput conf1 sel1
      , if isDisabled
          then selectInput (conf2 { disableWhen = const true }) sel2
          else selectInput conf2 sel2
      ]

dimensionPicker ∷ ∀ f. Foldable f ⇒ f (J.JCursor) → String → DPC.PickerOptions (Either J.JCursor J.JCursor)
dimensionPicker options title =
  { title
  , label: DPC.labelNode showJCursorTip
  , render: DPC.renderNode showJCursorTip
  , values: groupJCursors (List.fromFoldable options)
  , isSelectable: DPC.isLeafPath
  }

type DimensionOptions a b i =
  { configurable ∷ Boolean
  , formattable ∷ Boolean
  , dimension ∷ D.Dimension a b
  , showLabel ∷ a → String
  , showDefaultLabel ∷ b → String
  , showValue ∷ b → String
  , onLabelChange ∷ String → Maybe (i Unit)
  , onDismiss ∷ DOM.MouseEvent → Maybe (i Unit)
  , onConfigure ∷ DOM.MouseEvent → Maybe (i Unit)
  , onSetupFormatting ∷ DOM.MouseEvent → Maybe (i Unit)
  , onMouseDown ∷ DOM.MouseEvent → Maybe (i Unit)
  , onClick ∷ DOM.MouseEvent → Maybe (i Unit)
  , onLabelClick ∷ DOM.MouseEvent → Maybe (i Unit)
  , disabled ∷ Boolean
  , dismissable ∷ Boolean
  , labelless ∷ Boolean
  }

dimensionButton ∷ ∀ a b p i . DimensionOptions a b i → H.HTML p i
dimensionButton opts =
  HH.div
    [ HP.classes
        $ [ HH.ClassName "sd-dimension-button" ]
        ⊕ ( guard opts.disabled $> HH.ClassName "sd-dimension-button-disabled")
    ]
    $ ( guard (not opts.labelless) $>
        HH.div
        [ HP.classes [ HH.ClassName "sd-dimension-button-label" ] ]
        [ HH.input
            [ HP.type_ HP.InputText
            , HP.disabled opts.disabled
            , HP.value labelText
            , HE.onValueInput opts.onLabelChange
            , HE.onClick opts.onLabelClick
            , HP.placeholder defaultLabel
            ]
        ] ) ⊕
     [ HH.div
        [ HP.class_ toolbarClass ]
        $ join
          [ guard opts.dismissable $>
              HH.button
                [ HP.classes [ HH.ClassName "sd-dismiss-button" ]
                , HP.title "Dismiss"
                , ARIA.label "Dismiss"
                , HE.onClick opts.onDismiss
                , HP.disabled opts.disabled
                ]
                [ I.closeSm ]
          , guard (opts.configurable && not (D.isStatic value)) $>
              HH.button
                [ HP.classes
                    [ HH.ClassName "sd-configure-button"
                    , HH.ClassName "sd-discrete-button"
                    ]
                , HP.title "Configure"
                , ARIA.label "Configure"
                , HE.onClick opts.onConfigure
                , HP.disabled opts.disabled
                ]
                [ I.cog ]
          , guard opts.formattable $>
              HH.button
                [ HP.classes
                    [ HH.ClassName "sd-formatting-options-button"
                    , HH.ClassName "sd-discrete-button"
                    ]
                , HP.title "Formatting options"
                , ARIA.label "Formatting options"
                , HE.onClick opts.onSetupFormatting
                , HP.disabled opts.disabled
                ]
                [ I.editSm ]
          ]
      , HH.button
          [ HP.class_ buttonClass
          , HE.onMouseDown opts.onMouseDown
          , HE.onClick opts.onClick
          , HP.disabled opts.disabled
          ]
          case value of
            D.Static str → [ renderValue str ]
            D.Projection Nothing v → [ renderValue (opts.showValue v) ]
            D.Projection (Just t) v → [ renderTransform t, renderValue (opts.showValue v) ]
      ]
  where
  value = opts.dimension ^. D._value
  label = opts.dimension ^. D._category

  toolbarClass =
    HH.ClassName
      if opts.labelless
        then "sd-dimension-button-toolbar-wo-label"
        else "sd-dimension-button-toolbar"

  buttonClass =
    HH.ClassName
      if opts.labelless
        then "sd-dimension-button-display-wo-label"
        else "sd-dimension-button-display"

  labelText = case label of
    Just (D.Static str) → str
    _ → ""

  defaultLabel = case value of
    D.Static str → str
    D.Projection _ p → opts.showDefaultLabel p

  renderTransform t =
    HH.span
      [ HP.classes [ HH.ClassName "sd-dimension-button-transform" ] ]
      [ HH.text (T.prettyPrintTransform t) ]

  renderValue v =
    HH.span
      [ HP.classes [ HH.ClassName "sd-dimension-button-value" ] ]
      [ HH.text v ]
