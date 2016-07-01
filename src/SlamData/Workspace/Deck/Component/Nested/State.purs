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

module SlamData.Workspace.Deck.Component.Nested.State
  ( Driver(..)
  , State
  , initialState
  ) where

import SlamData.Prelude
import DOM.HTML.Types (HTMLElement)
import Halogen.Driver as HD

import SlamData.Effects (SlamDataRawEffects)
import SlamData.Workspace.Deck.Component.Query as DCQ

-- Newtyped because of impredicativity
newtype Driver = Driver (HD.Driver DCQ.QueryP SlamDataRawEffects)

type State =
  { el ∷ Maybe HTMLElement
  , driver ∷ Maybe Driver
  }

initialState ∷ State
initialState =
  { el: Nothing
  , driver: Nothing
  }
