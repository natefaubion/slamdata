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

module SlamData.Workspace.Card.Viz.Renderer.Select.Model where

import SlamData.Prelude

import Data.Argonaut as J
import Data.Codec as C
import Data.Codec.Argonaut as CA
import Data.Set as Set

import SlamData.Workspace.Card.CardType.Select as Sel
import SlamData.Workspace.Card.Setups.Semantics as Sem

import Test.StrongCheck.Gen as Gen
import Test.StrongCheck.Data.Argonaut (runArbJCursor)
import Test.StrongCheck.Arbitrary (arbitrary)

type Model =
  { formInputType ∷ Sel.Select ()
  , selected ∷ Set.Set Sem.Semantics
  , cursor ∷ J.JCursor
  }

eq_ ∷ Model → Model → Boolean
eq_ r1 r2 =
  r1.formInputType ≡ r2.formInputType
  ∧ r1.selected ≡ r2.selected
  ∧ r1.cursor ≡ r2.cursor

gen ∷ Gen.Gen Model
gen = do
  formInputType ← Gen.allInArray Sel.all
  cursor ← runArbJCursor <$> arbitrary
  selected ← map (Set.fromFoldable) $ Gen.arrayOf arbitrary
  pure { cursor
       , selected
       , formInputType
       }

codec ∷ CA.JsonCodec Model
codec = CA.object "SelectRenderer" $ CA.record
  # CA.recordProp (SProxy ∷ SProxy "formInputType") selCodec
  # CA.recordProp (SProxy ∷ SProxy "selected")
      (dimap Set.toUnfoldable Set.fromFoldable $ CA.array genCodec)
  # CA.recordProp (SProxy ∷ SProxy "cursor") genCodec
  where
  selCodec =
    C.basicCodec
      (\j → lmap CA.TypeMismatch $ J.decodeJson j >>= Sel.parse)
      (J.encodeJson ∘ Sel.print case_)

  genCodec ∷ ∀ a. J.EncodeJson a ⇒ J.DecodeJson a ⇒ CA.JsonCodec a
  genCodec = C.basicCodec (lmap CA.TypeMismatch ∘ J.decodeJson) J.encodeJson

  semCodec ∷ CA.JsonCodec Sem.Semantics
  semCodec = C.basicCodec (lmap CA.TypeMismatch ∘ J.decodeJson) J.encodeJson
