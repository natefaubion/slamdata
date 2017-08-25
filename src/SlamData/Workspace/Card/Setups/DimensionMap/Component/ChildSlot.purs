{-
Copyright 2017 SlamData, Inc.

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

module SlamData.Workspace.Card.Setups.DimensionMap.Component.ChildSlot where

import SlamData.Prelude

import Halogen.Component.ChildPath (ChildPath, cp1, cp2)

import SlamData.Workspace.Card.Setups.ActionSelect.Component as AS
import SlamData.Workspace.Card.Setups.DimensionPicker.Component as DP
import SlamData.Workspace.Card.Setups.DimensionPicker.JCursor (JCursorNode)
import SlamData.Workspace.Card.Setups.Transform (Transform)

type ChildSlot
  = Unit
  ⊹ Unit
  ⊹ Void

type ChildQuery
  = DP.Query JCursorNode
  ⨁ AS.Query Transform
  ⨁ Const Void

type Path a b = ChildPath a ChildQuery b ChildSlot

cpPicker ∷ Path (DP.Query JCursorNode) Unit
cpPicker = cp1

cpTransform ∷ Path (AS.Query Transform) Unit
cpTransform = cp2
