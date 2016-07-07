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

module SlamData.Workspace.Card.Next.Component.State
  ( State
  , initialState
  , _input
  , _filterString
  ) where

import SlamData.Prelude

import Data.Lens (LensP, lens)

import SlamData.Workspace.Card.Port (Port)

type State =
  { input ∷ Maybe Port
  , filterString ∷ String
  }

initialState :: State
initialState =
  { input: Nothing
  , filterString: ""
  }

_input ∷ ∀ a r. LensP { input ∷ a | r } a
_input = lens _.input (_ { input = _ })

_filterString ∷ ∀ a r. LensP { filterString ∷ a | r } a
_filterString = lens _.filterString (_ { filterString = _ })
