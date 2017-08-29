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
module SlamData.Workspace.Card.Setups.DisplayOptions.Model.Size where

import SlamData.Prelude

import Data.Codec.Argonaut.Common as CA
import Data.Codec.Argonaut.Generic as CAG
import Data.Lens as Lens
import Data.List.NonEmpty as NEL
import Data.List.Safe ((:))
import Data.List.Safe as SL

data Size = Small | Medium | Large

derive instance eqSize ∷ Eq Size
derive instance ordSize ∷ Ord Size
derive instance genericSize ∷ Generic Size _

codecSize ∷ CA.JsonCodec Size
codecSize = CAG.nullarySum "Size"

sizes ∷ NEL.NonEmptyList Size
sizes = SL.toNEL $ Small : Medium : Large : SL.nil

size ∷ Lens.Prism' String Size
size = Lens.prism' to from
  where
    to = case _ of
      Small → "Small"
      Medium → "Medium"
      Large → "Large"
    from = case _ of
      "Small" → Just Small
      "Medium" → Just Medium
      "Large" → Just Large
      _ → Nothing
