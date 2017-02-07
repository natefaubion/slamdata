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

module Halogen.Component.Utils where

import Prelude

import Control.Monad.Aff (Aff, Canceler, forkAff, later')
import Control.Monad.Aff.AVar (AVAR, AVar, makeVar, putVar, takeVar)
import Control.Monad.Aff.Bus as Bus
import Control.Monad.Aff.EventLoop as EventLoop
import Control.Monad.Aff.Class (class MonadAff, liftAff)

import Data.Either as E
import Data.Int as Int
import Data.Time.Duration (Milliseconds(..))

import Halogen as H
import Halogen.Query.EventSource as ES

import Math as Math

sendAfter
  ∷ ∀ s f g p o m eff
  . MonadAff (avar ∷ AVAR | eff) m
  ⇒ Milliseconds
  → (∀ a. a → f a)
  → H.HalogenM s f g p o m (Canceler (avar ∷ AVAR | eff))
sendAfter ms action = do
  cancelerVar ← H.liftAff makeVar
  H.subscribe $ oneTimeEventSource ms action cancelerVar
  H.liftAff $ takeVar cancelerVar

oneTimeEventSource
  ∷ ∀ f m eff
  . MonadAff (avar ∷ AVAR | eff) m
  ⇒ Milliseconds
  → (∀ a. a → f a)
  → AVar (Canceler (avar ∷ AVAR | eff))
  → H.EventSource f m
oneTimeEventSource (Milliseconds n) action cancelerVar = ES.EventSource do
  let
    producer = ES.produceAff \emit → liftAff do
      let
        delayedEmitter = later' (Int.floor $ Math.max n zero) do
          emit $ E.Left $ action ES.Done
          emit $ E.Right unit
      putVar cancelerVar =<< forkAff delayedEmitter
  pure { producer, done: pure unit }

busEventSource
  ∷ ∀ f m a r eff
  . MonadAff (avar ∷ AVAR | eff) m
  ⇒ (a → f ES.SubscribeStatus)
  → Bus.Bus (read ∷ Bus.Cap | r) a
  → ES.EventSource f m
busEventSource k bus =
  affEventSource k (Bus.read bus)

affEventSource
  ∷ ∀ f m a eff
  . MonadAff (avar ∷ AVAR | eff) m
  ⇒ (a → f ES.SubscribeStatus)
  → Aff (avar ∷ AVAR | eff) a
  → ES.EventSource f m
affEventSource k source = ES.EventSource do
  breaker ← liftAff makeVar
  let
    producer = ES.produceAff \emit → liftAff do
      loop ← EventLoop.forever do
        a ← source
        forkAff $ emit $ E.Left (k a)
      putVar breaker loop.breaker
      loop.run
    done = liftAff (EventLoop.break' =<< takeVar breaker)
  pure { producer, done }
