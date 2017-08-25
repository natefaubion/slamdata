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

module SlamData.Workspace.Card.Setups.DimensionPicker.Column where

import SlamData.Prelude

import Control.Comonad.Cofree as CF

import Data.Argonaut as J
import Data.List as L

import SlamData.Workspace.Card.Setups.PivotTable.Model (Column(..))
import SlamData.Workspace.Card.Setups.DimensionPicker.JCursor (unfoldJCursor)
import SlamData.Workspace.Card.Setups.DimensionPicker.Node (discriminateNodes)
import SlamData.Workspace.MillerColumns.TreeData (constructTree)

type ColumnNode = Either Column Column

groupColumns ∷ L.List Column → CF.Cofree L.List ColumnNode
groupColumns cs =
  let
    root = Column J.JCursorTop
  in
    discriminateNodes
      (constructTree unfoldColumn root cs)

unfoldColumn :: Column → Maybe (Tuple Column Column)
unfoldColumn col = case col of
  All → Just $ Tuple All $ Column J.JCursorTop
  Column value →
    unfoldJCursor value <#> \(Tuple _ rest) → Tuple col (Column rest)

showColumn ∷ (J.JCursor → String) → Column → String
showColumn f (Column value ) = f value
showColumn _ All = "*"

flattenColumns ∷ ColumnNode → Column
flattenColumns = either id id
