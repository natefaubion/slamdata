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

module SlamData.Quasar.Class where

import SlamData.Prelude

import Halogen.Query (HalogenM)

import Quasar.Advanced.QuasarAF as QF

class QuasarDSL m where
  liftQuasar ∷ QF.QuasarAFC ~> m

instance quasarDSLMaybeT ∷ (Monad m, QuasarDSL m) ⇒ QuasarDSL (MaybeT m) where
  liftQuasar = lift ∘ liftQuasar

instance quasarDSLExceptT ∷ (Monad m, QuasarDSL m) ⇒ QuasarDSL (ExceptT e m) where
  liftQuasar = lift ∘ liftQuasar

instance quasarDSLHalogenM ∷ (Monad m, QuasarDSL m) ⇒ QuasarDSL (HalogenM s f g p o m) where
  liftQuasar = lift ∘ liftQuasar

class QuasarDSL m ⇐ ParQuasarDSL m where
  sequenceQuasar ∷ ∀ f a. Traversable f ⇒ f (QF.QuasarAFC a) → m (f a)

instance parQuasarDSLMaybeT ∷ (Monad m, ParQuasarDSL m) ⇒ ParQuasarDSL (MaybeT m) where
  sequenceQuasar = lift ∘ sequenceQuasar

instance parQuasarDSLExceptT ∷ (Monad m, ParQuasarDSL m) ⇒ ParQuasarDSL (ExceptT e m) where
  sequenceQuasar = lift ∘ sequenceQuasar

instance parQuasarDSLHalogenM ∷ (Monad m, ParQuasarDSL m) ⇒ ParQuasarDSL (HalogenM s f g p o m) where
  sequenceQuasar = lift ∘ sequenceQuasar
