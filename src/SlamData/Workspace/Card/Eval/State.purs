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

module SlamData.Workspace.Card.Eval.State
  ( EvalState(..)
  , initialEvalState
  , AnalysisR
  , AutoSelectR
  , _Analysis
  , _Axes
  , _Records
  , _Resource
  , _LastUsedResource
  , _AutoSelect
  , _ActiveTab
  ) where

import SlamData.Prelude

import Data.Argonaut (Json)
import Data.Array as Array
import Data.Lens (Prism', prism', Traversal', wander)
import Data.Set as Set

import SlamData.Workspace.Card.Model as CM
import SlamData.Workspace.Card.Port (Resource)
import SlamData.Workspace.Card.Setups.Axis (Axes)
import SlamData.Workspace.Card.Setups.Semantics as Sem

type AnalysisR =
  { resource ∷ Resource
  , axes ∷ Axes
  , records ∷ Array Json
  }

type AutoSelectR =
  { lastUsedResource ∷ Resource
  , autoSelect ∷ Set.Set Sem.Semantics
  }

data EvalState
  = Analysis AnalysisR
  | AutoSelect AutoSelectR
  | ActiveTab Int

initialEvalState ∷ CM.AnyCardModel → Maybe EvalState
initialEvalState = case _ of
  CM.Tabs { tabs } → ActiveTab <$> (guard (Array.length tabs > 0) $> 0)
  _ → Nothing

_Analysis ∷ Prism' EvalState AnalysisR
_Analysis = prism' Analysis case _ of
  Analysis x → Just x
  _ → Nothing

_Axes ∷ Traversal' EvalState Axes
_Axes = wander \f s → case s of
  Analysis r@{ axes } → Analysis ∘ r { axes = _} <$> f axes
  _ → pure s

_Records ∷ Traversal' EvalState (Array Json)
_Records = wander \f s → case s of
  Analysis r@{ records } → Analysis ∘ r { records = _} <$> f records
  _ → pure s

_Resource ∷ Traversal' EvalState Resource
_Resource = wander \f s → case s of
  Analysis r@{ resource } → Analysis ∘ r { resource = _} <$> f resource
  _ → pure s

_LastUsedResource ∷ Traversal' EvalState Resource
_LastUsedResource = wander \f s → case s of
  AutoSelect r@{ lastUsedResource } → AutoSelect ∘ r { lastUsedResource = _ } <$> f lastUsedResource
  _ → pure s

_AutoSelect ∷ Traversal' EvalState (Set.Set Sem.Semantics)
_AutoSelect = wander \f s → case s of
  AutoSelect r@{ autoSelect } → AutoSelect ∘ r { autoSelect = _ } <$> f autoSelect
  _ → pure s

_ActiveTab ∷ Prism' EvalState Int
_ActiveTab = prism' ActiveTab case _ of
  ActiveTab n → Just n
  _ → Nothing
