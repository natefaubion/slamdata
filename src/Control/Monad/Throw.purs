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

module Control.Monad.Throw
  ( module Control.Monad.Throw
  , module Control.Monad.Error.Class
  )
  where

import Prelude
import Control.Monad.Eff.Exception as Exn
import Control.Monad.Error.Class (class MonadThrow, throwError)
import Data.Either (Either, either)
import Data.Maybe (Maybe, maybe)

note ∷ ∀ m e a. MonadThrow e m ⇒ e → Maybe a → m a
note err = maybe (throwError err) pure

noteError ∷ ∀ m a. MonadThrow Exn.Error m ⇒ String → Maybe a → m a
noteError = note <<< Exn.error

rethrow ∷ ∀ m e a. MonadThrow e m ⇒ Either e a → m a
rethrow = either throwError pure
