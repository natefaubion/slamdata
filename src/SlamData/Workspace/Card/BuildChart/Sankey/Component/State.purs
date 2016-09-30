module SlamData.Workspace.Card.BuildChart.Sankey.Component.State where

import SlamData.Prelude

import Data.Argonaut (JCursor)
import Data.Lens (LensP, lens)

import Halogen (ParentState)

import SlamData.Monad (Slam)
import SlamData.Form.Select (Select, emptySelect)
import SlamData.Workspace.LevelOfDetails (LevelOfDetails(..))
import SlamData.Workspace.Card.BuildChart.Sankey.Component.ChildSlot as CS
import SlamData.Workspace.Card.BuildChart.Sankey.Component.Query (QueryC, Selection)
import SlamData.Workspace.Card.BuildChart.Aggregation (Aggregation)
import SlamData.Workspace.Card.BuildChart.Axis (Axes, initialAxes)
import SlamData.Workspace.Card.BuildChart.Inputs (Select', PickerOptions)

type State =
  { axes ∷ Axes
  , levelOfDetails ∷ LevelOfDetails
  , source ∷ Select JCursor
  , target ∷ Select JCursor
  , value ∷ Select JCursor
  , valueAgg ∷ Select' Aggregation
  , picker ∷ Maybe (PickerOptions JCursor Selection)
  }

initialState ∷ State
initialState =
  { axes: initialAxes
  , levelOfDetails: High
  , source: emptySelect
  , target: emptySelect
  , value: emptySelect
  , valueAgg: false × emptySelect
  , picker: Nothing
  }

type StateP =
  ParentState State CS.ChildState QueryC CS.ChildQuery Slam CS.ChildSlot

_value ∷ ∀ r a. LensP { value ∷ a | r } a
_value = lens _.value _{ value = _ }

_valueAgg ∷ ∀ r a. LensP { valueAgg ∷ a | r } a
_valueAgg = lens _.valueAgg _{ valueAgg = _ }

_source ∷ ∀ r a. LensP { source ∷ a | r } a
_source = lens _.source _{ source = _ }

_target ∷ ∀ r a. LensP { target ∷ a | r } a
_target = lens _.target _{ target = _ }

showPicker
  ∷ (Const Unit JCursor → Selection (Const Unit))
  → Array JCursor
  → State
  → State
showPicker f options =
  _ { picker = Just { options, select: f (Const unit) } }