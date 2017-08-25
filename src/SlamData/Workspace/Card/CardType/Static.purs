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

module SlamData.Workspace.Card.CardType.Static where

import SlamData.Prelude

import Data.Variant (inj, on)
import Halogen.HTML as H
import SlamData.Render.Icon as I

_static = SProxy ∷ SProxy "static"

type StaticR r = (static ∷ Unit|r)
type Static r = Variant (StaticR r)

all ∷ ∀ r. Array (Static r)
all = [ static ]

static ∷ ∀ r. Variant (static ∷ Unit|r)
static = inj _static unit

print ∷ ∀ r. (Variant r → String) → Static r → String
print cb = cb # on _static (const "static")

encode ∷ ∀ r. (Variant r → String) → Static r → String
encode cb = cb # on _static (const "static-setup")

icon ∷ ∀ r. (Variant r → I.IconHTML) → Static r → I.IconHTML
icon cb = cb # on _static (const $ I.IconHTML I.cardsSetupFormInputStatic)

name ∷ ∀ r. (Variant r → String) → Static r → String
name cb = cb # on _static (const "Static Text")

parse ∷ ∀ r. String → String ⊹ Static r
parse = case _ of
  "static" → Right static
  ty → Left $ " is not static text card type"

consumerInteractable ∷ ∀ r. (Variant r → Boolean) → Static r → Boolean
consumerInteractable cb = cb # on _static ff

cardClasses ∷ ∀ r. (Variant r → Array H.ClassName) → Static r → Array H.ClassName
cardClasses cb = cb # on _static (const [ H.ClassName "sd-form-input-setup" ] )

contractToStatic ∷ ∀ r. Contractable r (StaticR ()) ⇒ Variant r → Maybe (Static ())
contractToStatic = contract
