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
module SlamData.Workspace.Card.Setups.DisplayOptions.DecimalFormat.Model
  ( DecimalFormat
  , codec
  , render
  , module Exports
  ) where

import SlamData.Prelude

import Data.Codec.Argonaut.Common as CA
import Data.Number as Num
import Data.String as Str
import SlamData.Workspace.Card.Setups.DisplayOptions.IntegerFormat.Model as IntegerFormat
import SlamData.Workspace.Card.Setups.DisplayOptions.Model.RoundBehaviour (RoundBehaviour(..), roundBehaviours, roundBehaviour) as Exports
import SlamData.Workspace.Card.Setups.DisplayOptions.Model.RoundBehaviour as RB
import Utils (showPrettyNumber)

type DecimalFormat =
  { prefix ∷ String
  , suffix ∷ String
  , thousands ∷ String
  , decimal ∷ String
  , minPlaces ∷ Maybe Int
  , maxPlaces ∷ Maybe Int
  , rounding ∷ RB.RoundBehaviour
  }

codec ∷ CA.JsonCodec DecimalFormat
codec = CA.object "DecimalFormat" (CA.record
  # CA.recordProp (SProxy ∷ SProxy "prefix") CA.string
  # CA.recordProp (SProxy ∷ SProxy "suffix") CA.string
  # CA.recordProp (SProxy ∷ SProxy "thousands") CA.string
  # CA.recordProp (SProxy ∷ SProxy "decimal") CA.string
  # CA.recordProp (SProxy ∷ SProxy "minPlaces") (CA.maybe CA.int)
  # CA.recordProp (SProxy ∷ SProxy "maxPlaces") (CA.maybe CA.int)
  # CA.recordProp (SProxy ∷ SProxy "rounding") RB.codec)

render ∷ DecimalFormat → Number → String
render fmt n
  | n >= 1e21 = show n -- TODO-gb: handle scientific format, best route is via purescript-precise most likely=
  | [integral, decimal] ← Str.split (Str.Pattern ".") (show n) =
      formatDecimal fmt integral decimal
  | otherwise = showPrettyNumber n

formatDecimal ∷ DecimalFormat → String → String → String
formatDecimal fmt integral decimal =
  fmt.prefix
    <> IntegerFormat.renderGroupedDigits { sep: fmt.thousands, value: integral }
    <> decimalPart
    <> fmt.suffix
  where
    decimalPart
      | decimal == "0" && isNothing fmt.minPlaces = ""
      | otherwise =
          fmt.decimal <>
            pad
              (fromMaybe 0 fmt.minPlaces)
              (round fmt.rounding fmt.maxPlaces decimal)

round ∷ RB.RoundBehaviour → Maybe Int → String → String
round rounding (Just digits) s
  | Str.length s > digits =
      case Str.splitAt digits s of
        Just { before, after }
          | Just d ← Num.fromString (before <> "." <> after) →
              RB.render rounding d
        _ → s
round _ _ s = s

pad ∷ Int → String → String
pad size s
  | Str.length s < size = pad size (s <> "0")
  | otherwise = s
