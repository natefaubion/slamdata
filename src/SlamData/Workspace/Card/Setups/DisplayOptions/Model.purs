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
module SlamData.Workspace.Card.Setups.DisplayOptions.Model where

import SlamData.Prelude

import Data.Codec.Argonaut.Common as CA
import Data.Codec.Argonaut.Generic as CAG
import Data.Lens as Lens
import Data.List.NonEmpty as NEL
import Data.List.Safe ((:))
import Data.List.Safe as SL
import Data.Set as S

data Alignment = AlignStart | AlignMiddle | AlignEnd

derive instance eqAlignment ∷ Eq Alignment
derive instance ordAlignment ∷ Ord Alignment
derive instance genericAlignment ∷ Generic Alignment _

codecAlignment ∷ CA.JsonCodec Alignment
codecAlignment = CAG.nullarySum "Alignment"

alignments ∷ NEL.NonEmptyList Alignment
alignments = SL.toNEL $ AlignStart : AlignMiddle : AlignEnd : SL.nil

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

data StyleOption = Emphasis | Strong | Underline

derive instance eqStyleOption ∷ Eq StyleOption
derive instance ordStyleOption ∷ Ord StyleOption
derive instance genericStyleOption ∷ Generic StyleOption _

codecStyleOption ∷ CA.JsonCodec StyleOption
codecStyleOption = CAG.nullarySum "StyleOption"

newtype Style = Style (S.Set StyleOption)

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
codecStyle =
  dimap
    (\(Style ss) → S.toUnfoldable ss)
    (Style ∘ S.fromFoldable)
    (CA.array codecStyleOption)

data FormatOptions = Automatic

derive instance eqFormatOptions ∷ Eq FormatOptions
derive instance ordFormatOptions ∷ Ord FormatOptions
derive instance genericFormatOptions ∷ Generic FormatOptions _

codecFormatOptions ∷ CA.JsonCodec FormatOptions
codecFormatOptions = CAG.nullarySum "FormatOptions"

type DisplayOptionsR =
  { alignment ∷ { horz ∷ Alignment, vert ∷ Alignment }
  , style ∷ Style
  , format ∷ FormatOptions
  }

newtype DisplayOptions = DisplayOptions DisplayOptionsR

derive instance eqDisplayOptions ∷ Eq DisplayOptions
derive instance ordDisplayOptions ∷ Ord DisplayOptions
derive instance newtypeDisplayOptions ∷ Newtype DisplayOptions _

codecAlignmentRec ∷ CA.JsonCodec { horz ∷ Alignment, vert ∷ Alignment }
codecAlignmentRec = CA.object "Alignment record" (CA.record
  # CA.recordProp (SProxy ∷ SProxy "horz") codecAlignment
  # CA.recordProp (SProxy ∷ SProxy "vert") codecAlignment)

codecDisplayOptions ∷ CA.JsonCodec DisplayOptions
codecDisplayOptions = _Newtype $ CA.object "DisplayOptions" (CA.record
  # CA.recordProp (SProxy ∷ SProxy "alignment") codecAlignmentRec
  # CA.recordProp (SProxy ∷ SProxy "style") codecStyle
  # CA.recordProp (SProxy ∷ SProxy "format") codecFormatOptions)

initialDisplayOptions ∷ DisplayOptions
initialDisplayOptions =
  DisplayOptions
    { alignment: { horz: AlignStart, vert: AlignStart }
    , style: mempty
    , format: Automatic
    }

genDisplayOptions ∷ ∀ m. MonadGen m ⇒ m DisplayOptions
genDisplayOptions = pure initialDisplayOptions
