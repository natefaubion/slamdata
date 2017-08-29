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
module SlamData.Workspace.Card.Setups.DisplayOptions.BooleanFormat.Model where

import SlamData.Prelude

import Data.Codec.Argonaut.Common as CA

type BooleanFormat =
  { true ∷ String
  , false ∷ String
  }

codec ∷ CA.JsonCodec BooleanFormat
codec = CA.object "BooleanFormat" (CA.record
  # CA.recordProp (SProxy ∷ SProxy "true") CA.string
  # CA.recordProp (SProxy ∷ SProxy "false") CA.string)

render ∷ BooleanFormat → Boolean → String
render fmt b
  | b = fmt.true
  | otherwise = fmt.false
