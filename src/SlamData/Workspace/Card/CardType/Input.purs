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

module SlamData.Workspace.Card.CardType.Input where

import SlamData.Prelude

import Data.Variant (inj, on)
import Halogen.HTML as H
import SlamData.Render.Icon as I

_text = SProxy ∷ SProxy "text"
_numeric = SProxy ∷ SProxy "numeric"
_date = SProxy ∷ SProxy "date"
_time = SProxy ∷ SProxy "time"
_datetime = SProxy ∷ SProxy "datetime"

type InputR r =
  ( text ∷ Unit
  , numeric ∷ Unit
  , date ∷ Unit
  , time ∷ Unit
  , datetime ∷ Unit
  | r)

type Input r = Variant (InputR r)

all ∷ ∀ r. Array (Input r)
all =
  [ text
  , numeric
  , date
  , time
  , datetime
  ]

text ∷ ∀ r. Variant (text ∷ Unit|r)
text = inj _text unit

numeric ∷ ∀ r. Variant (numeric ∷ Unit|r)
numeric = inj _numeric unit

date ∷ ∀ r. Variant (date ∷ Unit|r)
date = inj _date unit

time ∷ ∀ r. Variant (time ∷ Unit|r)
time = inj _time unit

datetime ∷ ∀ r. Variant (datetime ∷ Unit|r)
datetime = inj _datetime unit

print ∷ ∀ r. (Variant r → String) → Input r → String
print cb = cb
  # on _text (const "text")
  # on _numeric (const "numeric")
  # on _date (const "date")
  # on _time (const "time")
  # on _datetime (const "datetime")

encode ∷ ∀ r. (Variant r → String) → Input r → String
encode cb = cb
  # on _text (const "text-setup")
  # on _numeric (const "numeric-setup")
  # on _date (const "date-setup")
  # on _time (const "time-setup")
  # on _datetime (const "datetime-setup")

icon ∷ ∀ r. (Variant r → I.IconHTML) → Input r → I.IconHTML
icon cb = cb
  # on _text (const $ I.IconHTML I.cardsSetupFormInputText)
  # on _numeric (const $ I.IconHTML I.cardsSetupFormInputNumeric)
  # on _date (const $ I.IconHTML I.cardsSetupFormInputDate)
  # on _time (const $ I.IconHTML I.cardsSetupFormInputTime)
  # on _datetime (const $ I.IconHTML I.cardsSetupFormInputDatetime)

name ∷ ∀ r. (Variant r → String) → Input r → String
name cb = cb
  # on _text (const "Text Input")
  # on _numeric (const "Numeric Input")
  # on _date (const "Date Input")
  # on _time (const "Time Input")
  # on _datetime (const "Date/Time Input")

parse ∷ ∀ r. String → String ⊹ Input r
parse = case _ of
  "text" → Right text
  "numeric" → Right numeric
  "date" → Right date
  "time" → Right time
  "datetime" → Right datetime
  ty → Left $ ty ⊕ " is unknown input card type"

consumerInteractable ∷ ∀ r. (Variant r → Boolean) → Input r → Boolean
consumerInteractable cb = cb
  # on _text ff
  # on _numeric ff
  # on _date ff
  # on _time ff
  # on _datetime ff

cardClasses ∷ ∀ r. (Variant r → Array H.ClassName) → Input r → Array H.ClassName
cardClasses cb = cb
  # on _text clss
  # on _numeric clss
  # on _date clss
  # on _time clss
  # on _datetime clss
  where
  clss _ = [ H.ClassName "sd-form-input-setup" ]

contractToInput ∷ ∀ r. Contractable r (InputR ()) ⇒ Variant r → Maybe (Input ())
contractToInput = contract
