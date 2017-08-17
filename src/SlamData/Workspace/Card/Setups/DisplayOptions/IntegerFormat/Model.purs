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
module SlamData.Workspace.Card.Setups.DisplayOptions.IntegerFormat.Model
  ( module SlamData.Workspace.Card.Setups.DisplayOptions.IntegerFormat.Model
  , module Exports
  ) where

import SlamData.Prelude

import Data.Codec.Argonaut.Common as CA
import Data.String as Str
import SlamData.Workspace.Card.Setups.DisplayOptions.Model.RoundBehaviour (RoundBehaviour(..), roundBehaviours, roundBehaviour) as Exports
import SlamData.Workspace.Card.Setups.DisplayOptions.Model.RoundBehaviour as RB

type IntegerFormat =
  { prefix ∷ String
  , suffix ∷ String
  , thousands ∷ String
  , rounding ∷ RB.RoundBehaviour
  }

codec ∷ CA.JsonCodec IntegerFormat
codec = CA.object "IntegerFormat" (CA.record
  # CA.recordProp (SProxy ∷ SProxy "prefix") CA.string
  # CA.recordProp (SProxy ∷ SProxy "suffix") CA.string
  # CA.recordProp (SProxy ∷ SProxy "thousands") CA.string
  # CA.recordProp (SProxy ∷ SProxy "rounding") RB.codec)

render ∷ IntegerFormat → Number → String
render { prefix, suffix, thousands, rounding } n
  | n >= 1e21 = show n -- TODO-gb: handle scientific format, best route is via purescript-precise most likely
  | otherwise =
      prefix
        <> renderGroupedDigits { sep: thousands, value: RB.render rounding n }
        <> suffix

renderGroupedDigits ∷ { sep ∷ String, value ∷ String } → String
renderGroupedDigits { sep, value }
  | Str.null sep = value
  | otherwise = go sep "" value
      where
        go ∷ String → String → String → String
        go sep acc s
          | Str.length s <= 3 = extendAcc sep acc s
          | otherwise =
              let i = Str.length s - 3
              in go sep (extendAcc sep acc (Str.drop i s)) (Str.take i s)
        extendAcc ∷ String → String → String → String
        extendAcc sep acc s = if Str.null acc then s else s <> sep <> acc
