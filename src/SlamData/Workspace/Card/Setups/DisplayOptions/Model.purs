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
module SlamData.Workspace.Card.Setups.DisplayOptions.Model
  ( module SlamData.Workspace.Card.Setups.DisplayOptions.Model
  , module A
  , module F
  , module S
  , module Z
  ) where

import SlamData.Prelude

import Data.Codec.Argonaut.Common as CA
import SlamData.Workspace.Card.Setups.DisplayOptions.Model.Alignment as A
import SlamData.Workspace.Card.Setups.DisplayOptions.Model.Format as F
import SlamData.Workspace.Card.Setups.DisplayOptions.Model.Style as S
import SlamData.Workspace.Card.Setups.DisplayOptions.Model.Size as Z

type DisplayOptionsR =
  { alignment ∷ A.Alignments
  , style ∷ S.Style
  , size ∷ Z.Size
  , format ∷ F.FormatOptions
  }

newtype DisplayOptions = DisplayOptions DisplayOptionsR

derive instance eqDisplayOptions ∷ Eq DisplayOptions
derive instance ordDisplayOptions ∷ Ord DisplayOptions
derive instance newtypeDisplayOptions ∷ Newtype DisplayOptions _

codecDisplayOptions ∷ CA.JsonCodec DisplayOptions
codecDisplayOptions = _Newtype $ CA.object "DisplayOptions" (CA.record
  # CA.recordProp (SProxy ∷ SProxy "alignment") A.codecAlignments
  # CA.recordProp (SProxy ∷ SProxy "style") S.codecStyle
  # CA.recordProp (SProxy ∷ SProxy "size") Z.codecSize
  # CA.recordProp (SProxy ∷ SProxy "format") F.codecFormatOptions)

initialDisplayOptions ∷ DisplayOptions
initialDisplayOptions =
  DisplayOptions
    { alignment: A.defaultAlignments
    , style: mempty
    , size: Z.Medium
    , format: F.DefaultFormat
    }

genDisplayOptions ∷ ∀ m. MonadGen m ⇒ m DisplayOptions
genDisplayOptions = pure initialDisplayOptions
