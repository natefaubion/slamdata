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
module SlamData.Workspace.Card.Setups.DisplayOptions.Model.Alignment where

import SlamData.Prelude

import Data.Codec.Argonaut.Common as CA
import Data.Codec.Argonaut.Generic as CAG
import Data.Lens as Lens
import Data.List.NonEmpty as NEL
import Data.List.Safe ((:))
import Data.List.Safe as SL

data Alignment = AlignStart | AlignMiddle | AlignEnd

derive instance eqAlignment ∷ Eq Alignment
derive instance ordAlignment ∷ Ord Alignment
derive instance genericAlignment ∷ Generic Alignment _

codecAlignment ∷ CA.JsonCodec Alignment
codecAlignment = CAG.nullarySum "Alignment"

alignmentOptions ∷ NEL.NonEmptyList Alignment
alignmentOptions = SL.toNEL $ AlignStart : AlignMiddle : AlignEnd : SL.nil

type Alignments = { horz ∷ Alignment, vert ∷ Alignment }

defaultAlignments ∷ Alignments
defaultAlignments = { horz: AlignStart, vert: AlignStart }

codecAlignments ∷ CA.JsonCodec Alignments
codecAlignments = CA.object "Alignment record" (CA.record
  # CA.recordProp (SProxy ∷ SProxy "horz") codecAlignment
  # CA.recordProp (SProxy ∷ SProxy "vert") codecAlignment)

horzAlign ∷ Lens.Prism' String Alignment
horzAlign = Lens.prism' to from
  where
    to = case _ of
      AlignStart → "Left"
      AlignMiddle → "Center"
      AlignEnd → "Right"
    from = case _ of
      "Left" → Just AlignStart
      "Center" → Just AlignMiddle
      "Right" → Just AlignEnd
      _ → Nothing

vertAlign ∷ Lens.Prism' String Alignment
vertAlign = Lens.prism' to from
  where
    to = case _ of
      AlignStart → "Top"
      AlignMiddle → "Middle"
      AlignEnd → "Bottom"
    from = case _ of
      "Top" → Just AlignStart
      "Middle" → Just AlignMiddle
      "Bottom" → Just AlignEnd
      _ → Nothing
