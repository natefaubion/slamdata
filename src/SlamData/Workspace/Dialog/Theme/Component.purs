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

module SlamData.Workspace.Dialog.Theme.Component where

import SlamData.Prelude

import Data.Lens.Fold (preview)
import Data.Lens.Prism (review)
import Data.URI as URI
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import SlamData.Monad (Slam)
import SlamData.Render.ClassName as CN
import SlamData.Theme.Option as Option
import SlamData.Theme.Theme as Theme
import Utils.DOM as DOM

type State =
  { custom ∷ String
  , option ∷ Option.Option
  , isSubmitting ∷ Boolean
  }

initialState ∷ Maybe Theme.Theme → State
initialState mt =
  { custom: maybe mempty Theme.getCustomValue mt
  , option: maybe Option.Default Option.fromTheme mt
  , isSubmitting: false
  }

toTheme ∷ State → Maybe Theme.Theme
toTheme s = case s.option of
  Option.Light →
    Just Theme.Light
  Option.Dark →
    Just Theme.Dark
  Option.Custom →
		-- Assuming HTML5 form validation is working (which it should)
		URI.runParseURIRef s.custom # hush <#> Theme.Custom
  _ →
    Nothing

data Query a
  = PreventDefaultAndSave DOM.Event a
  | UpdateOption Option.Option a
  | UpdateCustom String a
  | Cancel a

data Message
  = Dismiss
  | Theme (Maybe Theme.Theme)

component ∷ H.Component HH.HTML Query (Maybe Theme.Theme) Message Slam
component =
  H.component
    { initialState
    , render
    , eval
    , receiver: const Nothing
    }

render ∷ State → H.ComponentHTML Query
render { custom, option, isSubmitting } =
  let
    nameThemeOption ∷ String
    nameThemeOption = "themeOption"

    nameThemeCustomURL ∷ String
    nameThemeCustomURL = "themeCustomURL"

    isCustomOption ∷ Boolean
    isCustomOption = option == Option.Custom

    renderOption ∷ ∀ p i. Option.Option → H.HTML p i
    renderOption o =
      HH.option
        [ HP.value $ review Option.valuePrism' o
        , HP.selected $ option == o
        ]
        [ HH.text $ Option.toLabel o ]
  in
    HH.form
      [ HP.class_ $ HH.ClassName "deck-dialog-theme"
      , HE.onSubmit $ HE.input PreventDefaultAndSave
      ]
      [ HH.h4_ [ HH.text "Theme deck" ]
      , HH.fieldset
          [ HP.class_ $ HH.ClassName "deck-dialog-body"
          , HP.disabled $ isSubmitting
          ]
          [ HH.label
              [ HP.for nameThemeOption ]
              [ HH.text "Theme" ]
          , HH.select
              [ HP.class_ CN.formControl
              , HP.id_ nameThemeOption
              , HP.name nameThemeOption
              , HP.required true
              , HE.onValueChange
                  $ HE.input
                  $ UpdateOption ∘ fromMaybe Option.Default ∘ preview Option.valuePrism'
              ]
              $ renderOption <$> Option.options
          , HH.label
              [ HP.for nameThemeCustomURL ]
              [ HH.text "Custom Theme URL" ]
          , HH.input
              [ HP.class_ CN.formControl
              , HP.type_ HP.InputUrl
              , HP.id_ nameThemeCustomURL
              , HP.name nameThemeCustomURL
              , HP.placeholder "Custom CSS URL"
                -- Absolute URL pattern -- prevents bad submits
              , HP.pattern """^https?://([a-zA-Z0-9]([a-zA-ZäöüÄÖÜ0-9\-]{0,61}[a-zA-Z0-9])?\.)+[a-zA-Z]{2,6}/\S*$"""
              , HP.required isCustomOption
              , HP.disabled $ not isCustomOption
              , HE.onValueInput $ HE.input UpdateCustom
              ]
          ]
    , HH.div
        [ HP.classes [ HH.ClassName "deck-dialog-footer" ] ]
        [ HH.button
            [ HP.classes [ CN.btn, CN.btnDefault ]
            , HE.onClick $ HE.input_ Cancel
            , HP.type_ HP.ButtonButton
            ]
            [ HH.text "Dismiss" ]
        , HH.button
            [ HP.classes [ CN.btn, CN.btnPrimary ]
            , HP.type_ HP.ButtonSubmit
            , HP.disabled isSubmitting
            ]
            [ HH.text "Save" ]
        ]
      ]

eval ∷ Query ~> H.ComponentDSL State Query Message Slam
eval = case _ of
  PreventDefaultAndSave ev next → do
    H.liftEff (DOM.preventDefault ev)
    maybeTheme ← H.gets toTheme
    H.raise (Theme maybeTheme)
    pure next
  UpdateOption option next →
    H.modify _ { option = option } $> next
  UpdateCustom custom next →
    H.modify _ { custom = custom } $> next
  Cancel next →
    H.raise Dismiss $> next
