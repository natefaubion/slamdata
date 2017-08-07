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

module SlamData.Render.Form where

import SlamData.Prelude

import Data.Array as Array
import Data.Lens as Lens
import Data.List.NonEmpty as NEL
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP

renderCheckbox
  ∷ ∀ a f p
  . String
  → Boolean
  → (Boolean → H.Action f)
  → H.HTML p f
renderCheckbox label checked query =
  HH.label_
    [ HH.input
        [ HP.type_ HP.InputCheckbox
        , HP.checked checked -- checked (M.hasStyle opt style)
        , HE.onChecked (HE.input query) -- \b → Modify (\st → st { style = M.toggleStyle opt b st.style }))
        ]
    , HH.span_ [ HH.text label ]
    ]

renderSelect
  ∷ ∀ a f p
  . Eq a
  ⇒ NEL.NonEmptyList a
  → a
  → Lens.Prism' String a
  → (a → H.Action f)
  → H.HTML p f
renderSelect values value prism query =
  HH.select
    [ HE.onValueChange (map (flip query unit) ∘ Lens.preview prism)
    , HP.class_ (H.ClassName "sd-form-input")
    ]
    $ map renderOption (Array.fromFoldable values)
  where
    renderOption opt =
      HH.option
        [ HP.selected (value == opt) ]
        [ HH.text (Lens.review prism opt) ]
