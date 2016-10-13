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

module SlamData.Monad.Par where

import SlamData.Prelude
import Unsafe.Coerce (unsafeCoerce)

data ParF f x y a = ParF (x -> y -> a) (f x) (f y)
data Par (f :: * -> *) a

mkPar :: forall f x y a. ParF f x y a → Par f a
mkPar = unsafeCoerce

unPar :: forall f x y a r. (ParF f x y a → r) → Par f a → r
unPar = unsafeCoerce

instance functorPar ∷ Functor (Par f) where
  map f = unPar case _ of
    ParF g a b → mkPar (ParF (\x y → f (g x y)) a b)

