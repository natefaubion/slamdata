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
module SlamData.Workspace.Card.Setups.DisplayOptions.Component.Render where

import SlamData.Prelude

import Data.Lens (Prism', preview, review)
import Halogen as H
import Halogen.Component.ChildPath as CP
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Halogen.HTML.Events as HE
import SlamData.Monad (Slam)
import SlamData.Render.Form as RF
import SlamData.Workspace.Card.Setups.Dialog as CSD
import SlamData.Workspace.Card.Setups.DisplayOptions.BooleanFormat.Component as BooleanFormat
import SlamData.Workspace.Card.Setups.DisplayOptions.Component.ChildSlot as CS
import SlamData.Workspace.Card.Setups.DisplayOptions.Component.Query as Q
import SlamData.Workspace.Card.Setups.DisplayOptions.Component.State as S
import SlamData.Workspace.Card.Setups.DisplayOptions.CurrencyFormat.Component as CurrencyFormat
import SlamData.Workspace.Card.Setups.DisplayOptions.DateFormat.Component as DateFormat
import SlamData.Workspace.Card.Setups.DisplayOptions.DateTimeFormat.Component as DateTimeFormat
import SlamData.Workspace.Card.Setups.DisplayOptions.DecimalFormat.Component as DecimalFormat
import SlamData.Workspace.Card.Setups.DisplayOptions.IntegerFormat.Component as IntegerFormat
import SlamData.Workspace.Card.Setups.DisplayOptions.Model as M
import SlamData.Workspace.Card.Setups.DisplayOptions.TextFormat.Component as TextFormat
import SlamData.Workspace.Card.Setups.DisplayOptions.TimeFormat.Component as TimeFormat
import SlamData.Render.ClassName as CN

type HTML = H.ParentHTML Q.Query CS.ChildQuery CS.ChildSlot Slam

render ∷ S.State → HTML
render st@{ alignment, style, format, formatValue } =
  CSD.pickerDialog
    { onDismiss: Q.Raise Q.Dismiss
    , onConfirm: Q.Raise ∘ Q.Confirm
    , selection: S.toModel st
    , isSelectable: const true
    , classes: [ HH.ClassName "sd-display-options" ]
    , title: [ HH.text "Formatting options" ]
    , content:
        [ HH.div
            [ HP.class_ (H.ClassName "sd-display-options-top") ]
            [ renderAlignment alignment
            , renderStyle style
            ]
        , renderFormatBlock format formatValue
        ]
    }

renderAlignment ∷ { horz ∷ M.Alignment, vert ∷ M.Alignment } → HTML
renderAlignment { horz, vert } =
  HH.div
    [ HP.classes [ CN.panel, CN.panelDefault, H.ClassName "sd-display-options-alignment" ] ]
    [ HH.div
        [ HP.class_ CN.panelHeading ]
        [ HH.text "Alignment" ]
    , HH.div
        [ HP.class_ CN.panelBody ]
        [ HH.label_
            [ HH.span_ [ HH.text "Horizontal" ]
            , RF.renderSelect M.alignmentOptions horz M.horzAlign Q.SetHorzAlignment
            ]
        , HH.label_
            [ HH.span_ [ HH.text "Vertical" ]
            , RF.renderSelect M.alignmentOptions vert M.vertAlign Q.SetVertAlignment
            ]
        ]
    ]

renderStyle ∷ M.Style → HTML
renderStyle style =
  HH.div
    [ HP.classes [ CN.panel, CN.panelDefault, H.ClassName "sd-display-options-styles" ] ]
    [ HH.div
        [ HP.class_ CN.panelHeading ]
        [ HH.text "Styles" ]
    , HH.div
        [ HP.class_ CN.panelBody ]
        [ HH.div_
            [ HH.ul_
                [ HH.li_ [ renderOpt M.Strong "Bold" ]
                , HH.li_ [ renderOpt M.Emphasis "Italic" ]
                , HH.li_ [ renderOpt M.Underline "Underline" ]
                ]
            ]
        ]
    ]
  where
    renderOpt ∷ M.StyleOption → String → HTML
    renderOpt opt label =
      RF.renderCheckbox label (M.hasStyle opt style) (Q.ToggleStyle opt)

renderFormatBlock ∷ M.Format → Maybe M.FormatOptions → HTML
renderFormatBlock fmt fmtV =
  HH.div
    [ HP.class_ (H.ClassName "sd-display-options-formatting") ]
    [ HH.label
        [ HP.class_ (H.ClassName "sd-display-options-formatting-dropdown") ]
        [ HH.span_ [ HH.text "Value type" ]
        , RF.renderSelect M.formats fmt M.format Q.SetFormat
        ]
    , renderFormatOptions fmt fmtV
    ]

renderFormatOptions ∷ M.Format → Maybe M.FormatOptions → HTML
renderFormatOptions fmt fmtV =
  case fmt of
    M.Default →
      HH.text ""
    M.Currency →
      embed CS.cpCurrencyFormat CurrencyFormat.component M._CurrencyFormat
    M.Decimal →
      embed CS.cpDecimalFormat DecimalFormat.component M._DecimalFormat
    M.Integer →
      embed CS.cpIntegerFormat IntegerFormat.component M._IntegerFormat
    M.Date →
      embed CS.cpDateFormat DateFormat.component M._DateFormat
    M.Time →
      embed CS.cpTimeFormat TimeFormat.component M._TimeFormat
    M.DateTime →
      embed CS.cpDateTimeFormat DateTimeFormat.component M._DateTimeFormat
    M.Text →
      embed CS.cpTextFormat TextFormat.component M._TextFormat
    M.Boolean →
      embed CS.cpBooleanFormat BooleanFormat.component M._BooleanFormat
  where
    embed
      ∷ ∀ f a
      . CP.ChildPath f CS.ChildQuery Unit CS.ChildSlot
      → H.Component HH.HTML f (Maybe a) (Maybe a) Slam
      → Prism' M.FormatOptions a
      → HTML
    embed cp comp prism =
      HH.div
        [ HP.classes [ CN.panel, CN.panelDefault ] ]
        [ HH.div
            [ HP.class_ CN.panelBody ]
            [ HH.slot' cp unit comp
                (preview prism =<< fmtV)
                (HE.input Q.HandleFormatChange ∘ map (review prism))
            ]
        ]
