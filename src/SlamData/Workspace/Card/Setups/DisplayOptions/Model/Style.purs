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
module SlamData.Workspace.Card.Setups.DisplayOptions.Model.Style where

import SlamData.Prelude

import Data.Codec.Argonaut.Common as CA
import Data.Codec.Argonaut.Generic as CAG
import Data.Set as S
import Utils (_Set)

data StyleOption = Emphasis | Strong | Underline

derive instance eqStyleOption ∷ Eq StyleOption
derive instance ordStyleOption ∷ Ord StyleOption
derive instance genericStyleOption ∷ Generic StyleOption _

codecStyleOption ∷ CA.JsonCodec StyleOption
codecStyleOption = CAG.nullarySum "StyleOption"

newtype Style = Style (S.Set StyleOption)

derive instance newtypeStyle ∷ Newtype Style _
derive newtype instance eqStyle ∷ Eq Style
derive newtype instance ordStyle ∷ Ord Style
derive newtype instance semigroupStyle ∷ Semigroup Style
derive newtype instance monoidStyle ∷ Monoid Style

hasStyle ∷ StyleOption → Style → Boolean
hasStyle s (Style ss) = S.member s ss

toggleStyle ∷ StyleOption → Boolean → Style → Style
toggleStyle s enable (Style ss) =
  Style
    if enable
      then S.insert s ss
      else S.delete s ss

codecStyle ∷ CA.JsonCodec Style
codecStyle = _Newtype ∘ _Set $ CA.array codecStyleOption
