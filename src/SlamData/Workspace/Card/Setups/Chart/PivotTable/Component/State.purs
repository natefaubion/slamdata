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
  , Dimensions
  , initialState
  , modelFromState
  , stateFromModel
  , reorder
  , setColumnAggregation
  , selectColumnValues
  , selectDimensionValues
  ) where

import SlamData.Prelude

import Control.Comonad.Cofree (Cofree)

import Data.Argonaut (JCursor)
import Data.Array as Array
import Data.List (List, (:))
import Data.List as List

import SlamData.Workspace.Card.Setups.Axis (Axes, initialAxes)
import SlamData.Workspace.Card.Setups.Chart.Aggregation as Ag
import SlamData.Workspace.Card.Setups.Chart.PivotTable.Model (Model, Column(..))
import SlamData.Workspace.Card.Setups.DimensionPicker.Column (groupColumns)
import SlamData.Workspace.Card.Setups.DimensionPicker.JCursor (groupJCursors)

data Selecting
  = Dim (Dimensions JCursor)
  | Col (Dimensions Column)

type Dimensions a = Cofree List (Either a a)

type State =
  { axes ∷ Axes
  , fresh ∷ Int
  , dimensions ∷ Array (Int × JCursor)
  , columns ∷ Array (Int × Column)
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
  , fresh: 0
  , dimensions: []
  , columns: []
  , orderingColumn: Nothing
  , orderingDimension: Nothing
  , selecting: Nothing
  }

modelFromState ∷ State → Model
modelFromState st =
  { dimensions: map snd st.dimensions
  , columns: map snd st.columns
  }

stateFromModel ∷ Model → State → State
stateFromModel r st =
  let
    l1 = Array.length r.dimensions
    l2 = Array.length r.columns
    dims = Array.mapWithIndex Tuple r.dimensions
    cols = Array.mapWithIndex (\i → Tuple (i + l1)) r.columns
    fresh = l1 + l2
  in
    st { dimensions = dims, columns = cols, fresh = fresh }

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

setColumnAggregation ∷ Int → Maybe Ag.Aggregation → State → State
setColumnAggregation tag ag st = st { columns = map go st.columns }
  where
  go (tag' × Column col) | tag == tag' = tag × Column (col { valueAggregation = ag })
  go a = a

selectColumnValues ∷ Axes → Cofree List (Either Column Column)
selectColumnValues axes =
  groupColumns
    (Count : List.fromFoldable
      (map (Column ∘ { value: _, valueAggregation: Nothing })
        (Array.sort
          (axes.category
           <> axes.time
           <> axes.value
           <> axes.date
           <> axes.datetime))))

selectDimensionValues ∷ Axes → Cofree List (Either JCursor JCursor)
selectDimensionValues axes =
  groupJCursors
    (List.fromFoldable
      (Array.sort
        (axes.category
         <> axes.time
         <> axes.value)))
