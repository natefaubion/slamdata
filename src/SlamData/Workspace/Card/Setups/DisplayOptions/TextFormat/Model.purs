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
module SlamData.Workspace.Card.Setups.DisplayOptions.TextFormat.Model where

import SlamData.Prelude

import Data.String as Str
import Data.Codec.Argonaut.Common as CA

type TextFormat =
  { maxLength ∷ Int
  , truncated ∷ String
  }

codec ∷ CA.JsonCodec TextFormat
codec = CA.object "TextFormat" (CA.record
  # CA.recordProp (SProxy ∷ SProxy "maxLength") CA.int
  # CA.recordProp (SProxy ∷ SProxy "truncated") CA.string)

render ∷ TextFormat → String → String
render { maxLength, truncated } s
  | Str.length s > maxLength =
      Str.trim (Str.take (maxLength - Str.length truncated) s) <> truncated
  | otherwise = s
