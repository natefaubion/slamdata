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

module SlamData.Workspace.Card.Eval.Class where

import SlamData.Prelude
import Data.List (List)
import SlamData.Workspace.Eval.Deck as Deck
import SlamData.Workspace.Card.Port (Out)

import Unsafe.Coerce (unsafeCoerce)

class DeckEvalDSL m where
  childDecks ∷ m (List Deck.Id)
  evalDeck ∷ Deck.Id → m (Deck.Model × Out)
  evalDecks ∷ ∀ f a. Traversable f ⇒ (Deck.Model → Out → a) → f Deck.Id → m (f a)

data ParEvalDecks' f a b =
  ParEvalDecks
    (∀ m c d. Applicative m ⇒ (c → m d) → f c → m (f d))
    (Deck.Model → Out → a)
    (f Deck.Id)
    (f a → b)

data ParEvalDecks b

instance functorParEvalDecks ∷ Functor ParEvalDecks where
  map g = unParEvalDecks (\(ParEvalDecks t f as co) → coParEvalDecks (ParEvalDecks t f as (g <$> co)))

coParEvalDecks ∷ ∀ f a b. ParEvalDecks' f a b → ParEvalDecks b
coParEvalDecks = unsafeCoerce

unParEvalDecks ∷ ∀ b r. (∀ f a. ParEvalDecks' f a b → r) → ParEvalDecks b → r
unParEvalDecks = unsafeCoerce
