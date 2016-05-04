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

module SlamData.Notebook.Component.State where

import SlamData.Prelude

import Data.BrowserFeatures (BrowserFeatures)
import Data.Lens (LensP, lens)

import SlamData.Notebook.AccessType (AccessType(..))
import SlamData.Notebook.Card.CardId (CardId)

import Utils.Path (DirPath)

type State =
  { accessType ∷ AccessType
  , browserFeatures ∷ BrowserFeatures
  , loaded ∷ Boolean
  , viewingCard ∷ Maybe CardId
  , version ∷ Maybe String
  , parentHref ∷ Maybe String
  , path ∷ Maybe DirPath
  }


initialState ∷ { browserFeatures ∷ BrowserFeatures, version ∷ Maybe String } → State
initialState {browserFeatures, version} =
  { accessType: Editable
  , browserFeatures: browserFeatures
  , loaded: false
  , viewingCard: Nothing
  , version: version
  , parentHref: Nothing
  , path: Nothing
  }

_accessType ∷ ∀ a r. LensP {accessType ∷ a|r} a
_accessType = lens _.accessType _{accessType = _}

_browserFeatures ∷ ∀ a r. LensP {browserFeatures ∷ a|r} a
_browserFeatures = lens _.browserFeatures _{browserFeatures = _}

_loaded ∷ ∀ a r. LensP {loaded ∷ a|r} a
_loaded = lens _.loaded _{loaded = _}

_viewingCard ∷ ∀ a r. LensP {viewingCard ∷ a|r} a
_viewingCard = lens _.viewingCard _{viewingCard = _}

_version ∷ ∀ a r. LensP {version ∷ a|r} a
_version = lens _.version _{version = _}

_parentHref ∷ ∀ a r. LensP {parentHref ∷ a |r} a
_parentHref = lens _.parentHref _{parentHref = _}

_path ∷ ∀ a r. LensP {path ∷ a |r} a
_path = lens _.path _{path = _}
