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

module SlamData.Workspace.Card.Setups.PivotTable.Component.Query where

import Halogen.Component.Utils.Drag (DragEvent)
import SlamData.Workspace.Card.Setups.ActionSelect.Component as AS
import SlamData.Workspace.Card.Setups.DimensionPicker.Column (ColumnNode)
import SlamData.Workspace.Card.Setups.DimensionPicker.Component as DPC
import SlamData.Workspace.Card.Setups.DimensionPicker.JCursor (JCursorNode)
import SlamData.Workspace.Card.Setups.DisplayOptions.Component as Display
import SlamData.Workspace.Card.Setups.Transform as T
import SlamData.Workspace.Card.Setups.Axis as Ax
import SlamData.Workspace.Card.Setups.PivotTable.Model as M
import Utils.DOM as DOM

data ForDimension
  = ForGroupBy Int
  | ForColumn Int

data Query a
  = AddGroupBy a
  | AddColumn a
  | Remove ForDimension a
  | ChangeLabel ForDimension String a
  | Configure ForDimension a
  | SetupFormatting ForDimension a
  | OrderStart ForDimension DOM.MouseEvent a
  | Ordering ForDimension DragEvent a
  | OrderOver ForDimension a
  | OrderOut ForDimension a
  | HandleGroupByPicker (DPC.Message JCursorNode) a
  | HandleColumnPicker (DPC.Message ColumnNode) a
  | HandleTransformPicker ForDimension (AS.Message T.Transform) a
  | HandleFormatting ForDimension Display.Message a
  | SetAxes Ax.Axes a
  | Load M.Model a

type Message = M.Model
