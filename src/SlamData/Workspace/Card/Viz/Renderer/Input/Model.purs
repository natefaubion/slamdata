{-
Copyright 2016 SlamData, Inc.

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

module SlamData.Workspace.Card.Viz.Renderer.Input.Model where

import SlamData.Prelude

import Data.Argonaut as J
import Data.Argonaut.JCursor.Gen (genJCursor)
import Data.Codec as C
import Data.Codec.Argonaut as CA
import SlamData.Workspace.Card.CardType.Input as Inp
import Test.StrongCheck.Gen as Gen
import Test.StrongCheck.Arbitrary (arbitrary)

type Model =
  { formInputType ∷ Inp.Input ()
  , value ∷ String
  , cursor ∷ J.JCursor
  }

initialModel ∷ Model
initialModel =
  { formInputType: Inp.text
  , value: ""
  , cursor: J.JCursorTop
  }

eq_ ∷ Model → Model → Boolean
eq_ r1 r2 =
  r1.formInputType ≡ r2.formInputType
  ∧ r1.value ≡ r2.value
  ∧ r1.cursor ≡ r2.cursor

gen ∷ Gen.Gen Model
gen = do
  formInputType ← Gen.allInArray Inp.all
  cursor ← genJCursor
  value ← arbitrary
  pure { cursor
       , value
       , formInputType
       }

codec ∷ CA.JsonCodec Model
codec = CA.object "InputRenderer" $ CA.record
  # CA.recordProp (SProxy ∷ SProxy "formInputType") inpCodec
  # CA.recordProp (SProxy ∷ SProxy "value") CA.string
  # CA.recordProp (SProxy ∷ SProxy "cursor") genCodec
  where
  inpCodec =
    C.basicCodec
      (\j → lmap CA.TypeMismatch $ J.decodeJson j >>= Inp.parse)
      (J.encodeJson ∘ Inp.print case_)

  genCodec ∷ ∀ a. J.EncodeJson a ⇒ J.DecodeJson a ⇒ CA.JsonCodec a
  genCodec = C.basicCodec (lmap CA.TypeMismatch ∘ J.decodeJson) J.encodeJson
