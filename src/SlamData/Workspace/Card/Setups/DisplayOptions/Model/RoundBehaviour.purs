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
module SlamData.Workspace.Card.Setups.DisplayOptions.Model.RoundBehaviour where

import SlamData.Prelude

import Data.Codec.Argonaut.Common as CA
import Data.Codec.Argonaut.Generic as CAG
import Data.Lens as Lens
import Data.List.NonEmpty as NEL
import Data.List.Safe ((:))
import Data.List.Safe as SL
import Data.String as Str
import Math as Math

data RoundBehaviour = Round | Ceil | Floor | Truncate

derive instance eqRoundBehaviour ∷ Eq RoundBehaviour
derive instance ordRoundBehaviour ∷ Ord RoundBehaviour
derive instance genericRoundBehaviour ∷ Generic RoundBehaviour _

codec ∷ CA.JsonCodec RoundBehaviour
codec = CAG.nullarySum "RoundBehaviour"

render ∷ RoundBehaviour → Number → String
render rb n =
  print $ n # case rb of
    Round → Math.round
    Ceil → Math.ceil
    Floor → Math.floor
    Truncate → Math.trunc
  where
    print n =
      let s = show n
      in Str.take (Str.length s - 2) s

roundBehaviours ∷ NEL.NonEmptyList RoundBehaviour
roundBehaviours = SL.toNEL
  $ Round
  : Ceil
  : Floor
  : Truncate
  : SL.nil

roundBehaviour ∷ Lens.Prism' String RoundBehaviour
roundBehaviour = Lens.prism' to from
  where
    to = case _ of
      Round → "Round"
      Ceil → "Ceil"
      Floor → "Floor"
      Truncate → "Truncate"
    from = case _ of
      "Round" → Just Round
      "Ceil" → Just Ceil
      "Floor" → Just Floor
      "Truncate" → Just Truncate
      _ → Nothing
