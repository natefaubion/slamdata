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

module SlamData.Workspace.Card.Open.Component.State
  ( State
  , StateP
  , initialState
  , _selected
  , _levelOfDetails
  ) where

import SlamData.Prelude

import Data.Lens (LensP, lens)
import Data.List (List)

import Halogen (ParentState)

import SlamData.FileSystem.Resource as R
import SlamData.Workspace.Card.Common.EvalQuery (CardEvalQuery)
import SlamData.Workspace.LevelOfDetails (LevelOfDetails(..))
import SlamData.Monad (Slam)

import SlamData.Workspace.MillerColumns.Component as MC

import Utils.Path (AnyPath)

type State =
  { selected ∷ Maybe (List AnyPath)
  , levelOfDetails ∷ LevelOfDetails
  }

initialState ∷ State
initialState =
  { selected: Nothing
  , levelOfDetails: High
  }

_selected ∷ ∀ a r. LensP { selected ∷ a|r} a
_selected = lens (_.selected) (_{selected = _})

_levelOfDetails ∷ ∀ a r. LensP {levelOfDetails ∷ a|r} a
_levelOfDetails = lens (_.levelOfDetails) (_{levelOfDetails = _})

type StateP =
  ParentState
    State (MC.State R.Resource AnyPath)
    CardEvalQuery (MC.Query AnyPath)
    Slam
    Unit
