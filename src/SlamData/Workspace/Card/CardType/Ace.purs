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

module SlamData.Workspace.Card.CardType.Ace where

import SlamData.Prelude

import Data.Variant (inj, on)
import Halogen.HTML as H
import Halogen.HTML as HH
import SlamData.Render.Icon as I

_aceMarkdown = SProxy ∷ SProxy "aceMarkdown"
_aceSql = SProxy ∷ SProxy "aceSql"

aceMarkdown ∷ ∀ r. Variant (aceMarkdown ∷ Unit|r)
aceMarkdown = inj _aceMarkdown unit

aceSql ∷ ∀ r. Variant (aceSql ∷ Unit|r)
aceSql = inj _aceSql unit

type AceR r =
  ( aceMarkdown ∷ Unit
  , aceSql ∷ Unit
  | r)

type Ace r = Variant (AceR r)

all ∷ ∀ r. Array (Ace r)
all = [ aceMarkdown, aceSql ]

name ∷ ∀ r. (Variant r → String) → Ace r → String
name cb = cb
  # on _aceMarkdown (const "Setup Markdown")
  # on _aceSql (const "Query")

cardClasses ∷ ∀ r. (Variant r → Array H.ClassName) → Ace r → Array H.ClassName
cardClasses cb = cb
  # on _aceMarkdown (const $ [ HH.ClassName "sd-card-ace", HH.ClassName "sd-card-markdown" ])
  # on _aceSql (const $ [ HH.ClassName "sd-card-ace", HH.ClassName "sd-card-sql" ])

mode ∷ ∀ r. (Variant r → String) → Ace r → String
mode cb = cb
  # on _aceMarkdown (const "ace/mode/markdown")
  # on _aceSql (const "ace/mode/sql")

print ∷ ∀ r. (Variant r → String) → Ace r → String
print cb = cb
  # on _aceMarkdown (const "ace-markdown")
  # on _aceSql (const "ace-sql")

parse ∷ ∀ r. String → String ⊹ Ace r
parse = case _ of
  "ace-markdown" → Right aceMarkdown
  "ace-sql" → Right aceSql
  ty → Left $ ty ⊕ " is unknown ace type"

icon ∷ ∀ r. (Variant r → I.IconHTML) → Ace r → I.IconHTML
icon cb = cb
  # on _aceMarkdown (const $ I.IconHTML I.cardsSetupMarkdown)
  # on _aceSql (const $ I.IconHTML I.cardsQuery)

consumerInteractable ∷ ∀ r. (Variant r → Boolean) → Ace r → Boolean
consumerInteractable cb = cb
  # on _aceMarkdown ff
  # on _aceSql ff

contractToAce ∷ ∀ r. Contractable r (AceR ()) ⇒ Variant r → Maybe (Ace ())
contractToAce = contract
