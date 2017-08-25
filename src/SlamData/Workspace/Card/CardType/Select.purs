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

module SlamData.Workspace.Card.CardType.Select where

import SlamData.Prelude

import Data.Variant (inj, on)
import Halogen.HTML as H
import SlamData.Render.Icon as I

_dropdown = SProxy ∷ SProxy "dropdown"
_checkbox = SProxy ∷ SProxy "checkbox"
_radio = SProxy ∷ SProxy "radio"

type SelectR r =
  ( dropdown ∷ Unit
  , checkbox ∷ Unit
  , radio ∷ Unit
  | r)

type Select r = Variant (SelectR r)

all ∷ ∀ r. Array (Select r)
all = [ dropdown, checkbox, radio ]

dropdown ∷ ∀ r. Variant (dropdown ∷ Unit|r)
dropdown = inj _dropdown unit

checkbox ∷ ∀ r. Variant (checkbox ∷ Unit|r)
checkbox = inj _checkbox unit

radio ∷ ∀ r. Variant (radio ∷ Unit|r)
radio = inj _radio unit

print ∷ ∀ r. (Variant r → String) → Select r → String
print cb = cb
  # on _dropdown (const "dropdown")
  # on _checkbox (const "checkbox")
  # on _radio (const "radio")

encode ∷ ∀ r. (Variant r → String) → Select r → String
encode cb = cb
  # on _dropdown (const "dropdown-setup")
  # on _checkbox (const "checkbox-setup")
  # on _radio (const "radio-setup")

icon ∷ ∀ r. (Variant r → I.IconHTML) → Select r → I.IconHTML
icon cb = cb
  # on _dropdown (const $ I.IconHTML I.cardsSetupFormInputDropdown)
  # on _checkbox (const $ I.IconHTML I.cardsSetupFormInputCheckbox)
  # on _radio (const $ I.IconHTML I.cardsSetupFormInputRadio)

name ∷ ∀ r. (Variant r → String) → Select r → String
name cb = cb
  # on _dropdown (const "Dropdown")
  # on _checkbox (const "Checkbox Group")
  # on _radio (const "Radio Group")

parse ∷ ∀ r. String → String ⊹ Select r
parse = case _ of
  "dropdown" → Right dropdown
  "checkbox" → Right checkbox
  "radio" → Right radio
  ty → Left $ ty ⊕ " is unknown select card type"

consumerInteractable ∷ ∀ r. (Variant r → Boolean) → Select r → Boolean
consumerInteractable cb = cb
  # on _dropdown ff
  # on _radio ff
  # on _checkbox ff

cardClasses ∷ ∀ r. (Variant r → Array H.ClassName) → Select r → Array H.ClassName
cardClasses cb = cb
  # on _dropdown clss
  # on _radio clss
  # on _checkbox clss
  where
  clss _ = [ H.ClassName "sd-form-input-setup" ]

maximumCountOfEntries ∷ Select () → Int
maximumCountOfEntries = case_
  # on _dropdown (const 1000)
  # on _radio (const 1000)
  # on _checkbox (const 1000)

maximumCountOfSelectedValues ∷ Select () → Int
maximumCountOfSelectedValues = case_
  # on _dropdown (const 1)
  # on _radio (const 1)
  # on _checkbox (const top)

contractToSelect ∷ ∀ r. Contractable r (SelectR ()) ⇒ Variant r → Maybe (Select ())
contractToSelect = contract
