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

module SlamData.Workspace.Card.Eval.Machine
  ( Machine
  , Step(..)
  , extract
  , step
  , stateful
  , stepper
  ) where

import SlamData.Prelude

type Machine m a b = a → m (Step m a b)

data Step m a b = Step b (Machine m a b)

instance functorStep ∷ Functor m ⇒ Functor (Step m a) where
  map f (Step x m) = Step (f x) (m >>> map (map f))

extract ∷ ∀ m a b. Step m a b → b
extract (Step x _) = x

step ∷ ∀ m a b. Step m a b → a → m (Step m a b)
step (Step _ m) = m

stateful ∷ ∀ m a b s. Applicative m ⇒ (s → a → m (s × b)) → s → Machine m a b
stateful f s a = next <$> f s a
  where
  next (s' × b) = Step b (stateful f s')

stepper ∷ ∀ m a b. Functor m ⇒ (a → m b) → Machine m a b
stepper f a = next <$> f a
  where
  next b = Step b (stepper f)
