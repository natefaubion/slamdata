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

module SlamData.Notification
  ( Notification(..)
  , NotificationOptions
  , notify_
  , info_
  , warn_
  , error_
  ) where

import SlamData.Prelude

import Control.Monad.Aff.AVar (AVAR)
import Control.Monad.Aff.Bus (Bus, Cap, write)
import Control.Monad.Aff.Free (class Affable, fromAff)

import Data.Time (Milliseconds)

data Notification
  = Info String
  | Warning String
  | Error String

type NotificationOptions =
  { notification ∷ Notification
  , detail ∷ Maybe String
  , timeout ∷ Maybe Milliseconds
  }

notify_
  ∷ ∀ r m eff
  . (Affable (avar ∷ AVAR | eff) m)
  ⇒ Notification
  → Maybe String
  → Maybe Milliseconds
  → Bus (write ∷ Cap | r) NotificationOptions
  → m Unit
notify_ notification detail timeout bus =
  fromAff $ write { notification, detail, timeout } bus

info_
  ∷ ∀ r m eff
  . (Affable (avar ∷ AVAR | eff) m)
  ⇒ String
  → Maybe String
  → Maybe Milliseconds
  → Bus (write ∷ Cap | r) NotificationOptions
  → m Unit
info_ = notify_ <<< Info

warn_
  ∷ ∀ r m eff
  . (Affable (avar ∷ AVAR | eff) m)
  ⇒ String
  → Maybe String
  → Maybe Milliseconds
  → Bus (write ∷ Cap | r) NotificationOptions
  → m Unit
warn_ = notify_ <<< Warning

error_
  ∷ ∀ r m eff
  . (Affable (avar ∷ AVAR | eff) m)
  ⇒ String
  → Maybe String
  → Maybe Milliseconds
  → Bus (write ∷ Cap | r) NotificationOptions
  → m Unit
error_ = notify_ <<< Error
