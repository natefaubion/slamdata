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

module SlamData.Workspace.Component.State where

import SlamData.Prelude

import Data.Lens (LensP, lens)

import SlamData.Workspace.AccessType (AccessType(..))
import SlamData.Workspace.StateMode (StateMode(..))

import Utils.Path (DirPath)

type State =
  { accessType ∷ AccessType
  , loaded ∷ Boolean
  , version ∷ Maybe String
  , path ∷ Maybe DirPath
  , stateMode ∷ StateMode
  }

initialState ∷ Maybe String → State
initialState version =
  { accessType: Editable
  , loaded: false
  , version
  , path: Nothing
  , stateMode: Loading
  }

_accessType ∷ ∀ a r. LensP {accessType ∷ a|r} a
_accessType = lens _.accessType _{accessType = _}

_loaded ∷ ∀ a r. LensP {loaded ∷ a|r} a
_loaded = lens _.loaded _{loaded = _}

_version ∷ ∀ a r. LensP {version ∷ a|r} a
_version = lens _.version _{version = _}

_path ∷ ∀ a r. LensP {path ∷ a|r} a
_path = lens _.path _{path = _}

_stateMode ∷ LensP State StateMode
_stateMode = lens _.stateMode _{stateMode = _}
