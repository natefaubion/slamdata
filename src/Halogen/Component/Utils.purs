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

import Control.Coroutine.Stalling as SCR
import Control.Monad.Aff (Aff, Canceler, forkAff, later', runAff)
import Control.Monad.Aff.AVar (AVAR, makeVar, putVar, takeVar)
import Control.Monad.Aff.Free (class Affable, fromAff)
import Control.Monad.Eff.Class (liftEff)

import Data.Either as E
import Data.Time (Milliseconds(..))

import Halogen as H
import Halogen.Query.EventSource as He

import Utils.AffableProducer (produce)

withCanceler
  :: forall a e g
   . (Bind g, Affable (avar :: AVAR|e) g)
  => (Canceler (avar :: AVAR|e) -> g Unit)
  -> Aff (avar :: AVAR|e) a
  -> g a
withCanceler act aff = do
  v <- fromAff makeVar
  canceler <- fromAff $ forkAff do
    res <- aff
    putVar v res
  act canceler
  fromAff $ takeVar v

liftWithCanceler
  :: forall s f e a
   . (Canceler (avar :: AVAR|e)-> Unit -> f Unit)
  -> Aff (avar :: AVAR|e) a
  -> H.ComponentDSL s f (Aff (avar :: AVAR|e)) a
liftWithCanceler f aff = do
  withCanceler (\c -> sendAfter zero $ f c unit) aff

liftWithCanceler'
  :: forall s s' f f' p a e
   . (Canceler (avar :: AVAR|e) -> Unit -> f Unit)
  -> Aff (avar :: AVAR|e) a
  -> H.ParentDSL s s' f f' (Aff (avar :: AVAR|e)) p a
liftWithCanceler' f aff = do
  withCanceler (\c -> sendAfter' zero $ f c unit) aff

sendAfter
  :: forall s f e
   . Milliseconds
  -> f Unit
  -> H.ComponentDSL s f (Aff (avar :: AVAR|e)) Unit
sendAfter ms action =
  H.subscribe $ oneTimeEventSource ms action

sendAfter'
  :: forall s s' f f' p e
   . Milliseconds
  -> f Unit
  -> H.ParentDSL s s' f f' (Aff (avar :: AVAR|e)) p Unit
sendAfter' ms action =
  H.subscribe' $ oneTimeEventSource ms action

raise
  :: forall s f e
   . f Unit
  -> H.ComponentDSL s f (Aff (avar :: AVAR|e)) Unit
raise = sendAfter (Milliseconds 0.0)

raise'
  :: forall s s' f f' p e
   . f Unit
  -> H.ParentDSL s s' f f' (Aff (avar :: AVAR|e)) p Unit
raise' = sendAfter' (Milliseconds 0.0)

oneTimeEventSource
  :: forall f e
   . Milliseconds
  -> f Unit
  -> He.EventSource f (Aff (avar :: AVAR|e))
oneTimeEventSource (Milliseconds n) action =
  He.EventSource
  $ SCR.producerToStallingProducer
  $ produce \emit ->
      runAff (const $ pure unit) (const $ pure unit)
      $ later' (Data.Int.floor $ Math.max n zero)
      $ liftEff do
        emit $ E.Left action
        emit $ E.Right unit
