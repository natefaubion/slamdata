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

module SlamData.Workspace.Card.Setups.Chart.PivotTable.Component.State
  ( State
  , OrderingOpts
  , Selecting(..)
  , PickerTree
  , initialState
  , modelFromState
  , stateFromModel
  , _columns
  , _dimensions
  , reorder
  , setColumnTransform
  , setGroupByTransform
  , selectColumnValues
  , selectGroupByValues
  ) where

import SlamData.Prelude

import Control.Comonad.Cofree (Cofree)

import Data.Argonaut (JCursor)
import Data.Array as Array
import Data.Lens as Lens
import Data.List (List)
import Data.List as List
import Data.StrMap as SM

import SlamData.Workspace.Card.Port.VarMap as VM
import SlamData.Workspace.Card.Setups.Axis (Axes, initialAxes)
import SlamData.Workspace.Card.Setups.Chart.PivotTable.Component.Query (ForDimension)
import SlamData.Workspace.Card.Setups.Chart.PivotTable.Model as PTM
import SlamData.Workspace.Card.Setups.Dimension as D
import SlamData.Workspace.Card.Setups.DimensionPicker.Column (groupColumns)
import SlamData.Workspace.Card.Setups.DimensionPicker.JCursor (groupJCursors)
import SlamData.Workspace.Card.Setups.DimensionPicker.Variables (groupVariables, Variables, VariablesNode)
import SlamData.Workspace.Card.Setups.Transform as T

data Selecting
  = SelectGroupBy (PickerTree (Variables JCursor))
  | SelectColumn (PickerTree (Variables PTM.Column))
  | SelectTransform ForDimension (Maybe T.Transform) (Array T.Transform)

type PickerTree a = Cofree List (Either a a)

type State =
  { axes ∷ Axes
  , vars ∷ VM.VarMap
  , fresh ∷ Int
  , dimensions ∷ Array (Int × PTM.GroupByDimension)
  , columns ∷ Array (Int × PTM.ColumnDimension)
  , orderingColumn ∷ Maybe OrderingOpts
  , orderingDimension ∷ Maybe OrderingOpts
  , selecting ∷ Maybe Selecting
  }

type OrderingOpts =
  { source ∷ Int
  , over ∷ Maybe Int
  , offset ∷ Number
  }

initialState ∷ State
initialState =
  { axes: initialAxes
  , vars: SM.empty
  , fresh: 0
  , dimensions: []
  , columns: []
  , orderingColumn: Nothing
  , orderingDimension: Nothing
  , selecting: Nothing
  }

modelFromState ∷ State → PTM.Model
modelFromState st =
  { dimensions: map snd st.dimensions
  , columns: map snd st.columns
  }

stateFromModel ∷ PTM.Model → State → State
stateFromModel r st =
  let
    l1 = Array.length r.dimensions
    l2 = Array.length r.columns
    dims = Array.mapWithIndex Tuple r.dimensions
    cols = Array.mapWithIndex (\i → Tuple (i + l1)) r.columns
    fresh = l1 + l2
  in
    st { dimensions = dims, columns = cols, fresh = fresh }

_columns ∷ Lens.Lens' State (Array (Int × PTM.ColumnDimension))
_columns = Lens.lens _.columns _ { columns = _ }

_dimensions ∷ Lens.Lens' State (Array (Int × PTM.GroupByDimension))
_dimensions = Lens.lens _.dimensions _ { dimensions = _ }

reorder ∷ ∀ a. Int → Int → Array (Int × a) → Array (Int × a)
reorder tag1 tag2 arr | tag1 == tag2 = arr
reorder tag1 tag2 arr =
  let
    span = Array.span (not ∘ eq tag1 ∘ fst) arr
    init = span.init
    subj = Array.take 1 span.rest
    rest = Array.drop 1 span.rest
    ix1  = Array.findIndex (eq tag2 ∘ fst) init
    ix2  = add 1 <$> Array.findIndex (eq tag2 ∘ fst) rest
  in case ix1, ix2 of
    Just i1, _ → Array.take i1 init <> subj <> Array.drop i1 init <> rest
    _, Just i2 → init <> Array.take i2 rest <> subj <> Array.drop i2 rest
    _, _ → arr

modifyDimension ∷ ∀ a b. Lens.Lens' State (Array (Int × D.Dimension a b)) → (D.Dimension a b → D.Dimension a b) → Int → State → State
modifyDimension dimLens f tag = Lens.over dimLens (map go)
  where
  go (tag' × a) | tag == tag' = tag × f a
  go a = a

setColumnTransform ∷ Maybe T.Transform → Int → State → State
setColumnTransform = modifyDimension _columns ∘ Lens.set (D._value ∘ D._transform)

setGroupByTransform ∷ Maybe T.Transform → Int → State → State
setGroupByTransform = modifyDimension _dimensions ∘ Lens.set (D._value ∘ D._transform)

selectColumnValues ∷ List VM.Var → Axes → Cofree List (VariablesNode PTM.Column)
selectColumnValues vars axes =
  groupVariables vars
    $ groupColumns
    $ List.Cons PTM.All
    $ List.fromFoldable
    $ map PTM.Column
    $ axes.category <> axes.value <> axes.time <> axes.date <> axes.datetime

selectGroupByValues ∷ List VM.Var → Axes → Cofree List (VariablesNode JCursor)
selectGroupByValues vars axes =
  groupVariables vars
    $ groupJCursors
    $ List.fromFoldable
    $ axes.category <> axes.value <> axes.time <> axes.date <> axes.datetime
