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

module Data.ListMap where

import SlamData.Prelude hiding (empty)

import Data.Argonaut (class EncodeJson, class DecodeJson)
import Data.Codec.Argonaut as CA
import Data.Codec.Argonaut.Common as CAC
import Data.Foldable as F
import Data.List as L
import Data.Unfoldable (class Unfoldable)
import Test.StrongCheck.Gen as Gen

newtype ListMap k v = ListMap (L.List (k × v))

derive instance functorListMap ∷ Functor (ListMap k)
derive instance newtypeListMap ∷ Newtype (ListMap k v) _
derive newtype instance encodeJsonListMap
  ∷ ( EncodeJson k, EncodeJson v )
  ⇒ EncodeJson (ListMap k v)
derive newtype instance decodeJsonListMap
  ∷ ( DecodeJson k, DecodeJson v )
  ⇒ DecodeJson (ListMap k v)

listMapCodec
  ∷ ∀ k v
  . CA.JsonCodec k
  → CA.JsonCodec v
  → CA.JsonCodec (ListMap k v)
listMapCodec kCodec vCodec =
  _Newtype $ CAC.list $ CAC.tuple kCodec vCodec

empty ∷ ∀ k v. ListMap k v
empty = ListMap L.Nil

lookup ∷ ∀ k v. (k → k → Boolean) → k → ListMap k v → Maybe v
lookup eq__ k (ListMap a) = map snd $ L.find (eq__ k ∘ fst) a

member ∷ ∀ k v. (k → k → Boolean) → k → ListMap k v → Boolean
member eq__ k lm = isJust $ lookup eq__ k lm

insert ∷ ∀ k v. (k → k → Boolean) → k → v → ListMap k v → ListMap k v
insert eq__ k v (ListMap lm) =
  ListMap $ L.Cons (k × v) $ L.filter (not ∘ eq__ k ∘ fst) lm

fromFoldable ∷ ∀ f k v. Foldable f ⇒ f (k × v) → ListMap k v
fromFoldable = ListMap ∘ L.fromFoldable

toUnfoldable ∷ ∀ u k v. Unfoldable u ⇒ ListMap k v → u (k × v)
toUnfoldable (ListMap lm) = L.toUnfoldable lm

union ∷ ∀ k v. (k → k → Boolean) → ListMap k v → ListMap k v → ListMap k v
union eq__  (ListMap l) r =
  foldl foldFn r l
  where
  foldFn lm (k × v) =
    insert eq__ k v lm

eq_ ∷ ∀ k v. (k → k → Boolean) → (v → v → Boolean) → ListMap k v → ListMap k v → Boolean
eq_ eqK eqV (ListMap l) (ListMap r) =
  F.and $ L.zipWith (\(lk × lv) (rk × rv) → eqK lk rk ∧ eqV lv rv) l r

type Module k v =
  { lookup ∷ k → ListMap k v → Maybe v
  , member ∷ k → ListMap k v → Boolean
  , insert ∷ k → v → ListMap k v → ListMap k v
  , empty ∷ ListMap k v
  , union ∷ ListMap k v → ListMap k v → ListMap k v
  , eq_ ∷ ( v → v → Boolean ) → ListMap k v → ListMap k v → Boolean
  }

openModule ∷ ∀ k v. (k → k → Boolean) → Module k v
openModule eq__ =
  { lookup: lookup eq__
  , member: member eq__
  , empty: empty
  , insert: insert eq__
  , union: union eq__
  , eq_: eq_ eq__
  }

gen ∷ ∀ k v. Gen.Gen k → Gen.Gen v → Gen.Gen (ListMap k v)
gen genK genV =
  map (ListMap ∘ L.fromFoldable)
  $ Gen.arrayOf (Tuple <$> genK <*> genV)
