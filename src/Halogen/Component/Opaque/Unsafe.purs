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

-- | NOTE: This module lets you create recursive hierarchies of components,
-- | but is ultimately unsound. To trigger the unsound behavior, one would need
-- | to construct multiple components that share the *same parent algebra* yet
-- | with *different child algebras*. Then one could potentially *peek* on
-- | such a component, and then issue the query to another component. If it
-- | happened that this query was indeed a child query, it would be possible
-- | to fail at run time.
-- |
-- | We do not provide anyway to construct such bad queries, but since it is
-- | possible to observe and reissue them, it is unsafe.

module Halogen.Component.Opaque.Unsafe
 ( opaque
 , opaqueState
 , opaqueQuery
 , runOpaqueQuery
 , peekOpaqueQuery
 , OpaqueState
 , OpaqueQuery
 ) where

import Prelude
import Data.Functor.Coproduct (coproduct, left)
import Halogen (ParentState, ParentQuery, Component, parentState)
import Unsafe.Coerce (unsafeCoerce)

foreign import data OpaqueState :: * -> *

foreign import data OpaqueQuery :: (* -> *) -> * -> *

opaque
  :: forall s s' f f' g p
   . Component (ParentState s s' f f' g p) (ParentQuery f f' p) g
  -> Component (OpaqueState s) (OpaqueQuery f) g
opaque = unsafeCoerce

opaqueState :: forall s. s -> OpaqueState s
opaqueState = unsafeCoerce <<< parentState

opaqueQuery :: forall f a. f a -> OpaqueQuery f a
opaqueQuery = unsafeCoerce <<< left

runOpaqueQuery :: forall f a r. r -> (f a -> r) -> OpaqueQuery f a -> r
runOpaqueQuery nil f = coproduct f (const nil) <<< unsafeCoerce

peekOpaqueQuery :: forall f a m. (Monad m) => (f a -> m Unit) -> OpaqueQuery f a -> m Unit
peekOpaqueQuery = runOpaqueQuery (pure unit)
