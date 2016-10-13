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

module SlamData.Workspace.Card.BuildChart.PivotTable.Component.State
  ( State
  , StateP
  , OrderingOpts
  , Selecting(..)
  , initialState
  , modelFromState
  , stateFromModel
  , reorder
  , setColumnAggregation
  ) where

import SlamData.Prelude

import Data.Argonaut (JCursor)
import Data.Array as Array

import Halogen (ParentState)

import SlamData.Monad (Slam)
import SlamData.Workspace.LevelOfDetails (LevelOfDetails(..))
import SlamData.Workspace.Card.BuildChart.Aggregation as Ag
import SlamData.Workspace.Card.BuildChart.Axis (Axes, initialAxes)
import SlamData.Workspace.Card.BuildChart.PivotTable.Component.ChildSlot as PCS
import SlamData.Workspace.Card.BuildChart.PivotTable.Component.Query (QueryC)
import SlamData.Workspace.Card.BuildChart.PivotTable.Model (Model, Column(..))

type StateP =
  ParentState State PCS.ChildState QueryC PCS.ChildQuery Slam PCS.ChildSlot

data Selecting = Dim | Col

type State =
  { axes ∷ Axes
  , levelOfDetails ∷ LevelOfDetails
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
  , levelOfDetails: High
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
