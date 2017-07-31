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

module SlamData.Workspace.Card.Setups.Chart.PivotTable.Component
  ( pivotTableBuilderComponent
  ) where

import SlamData.Prelude

import Data.Argonaut as J
import Data.Array as Array
import Data.Foldable as F
import Data.Lens ((^?), (.~), _Just)
import Data.Lens as Lens
import Halogen as H
import Halogen.Component.Utils.Drag as Drag
import SlamData.Workspace.Card.CardType as CT
import SlamData.Workspace.Card.CardType.ChartType as CHT
import SlamData.Workspace.Card.Component as CC
import SlamData.Workspace.Card.Eval.State (_Axes)
import SlamData.Workspace.Card.Model as Card
import SlamData.Workspace.Card.Setups.ActionSelect.Component as AS
import SlamData.Workspace.Card.Setups.Axis as Ax
import SlamData.Workspace.Card.Setups.Chart.PivotTable.Component.ChildSlot as PCS
import SlamData.Workspace.Card.Setups.Chart.PivotTable.Component.Query (Query(..), ForDimension(..))
import SlamData.Workspace.Card.Setups.Chart.PivotTable.Component.Render as PR
import SlamData.Workspace.Card.Setups.Chart.PivotTable.Component.State as PS
import SlamData.Workspace.Card.Setups.Chart.PivotTable.Model as PTM
import SlamData.Workspace.Card.Setups.Dimension as D
import SlamData.Workspace.Card.Setups.DimensionPicker.Column (flattenColumns)
import SlamData.Workspace.Card.Setups.DimensionPicker.Component as DPC
import SlamData.Workspace.Card.Setups.DimensionPicker.JCursor (flattenJCursors)
import SlamData.Workspace.Card.Setups.Transform as T
import SlamData.Workspace.LevelOfDetails (LevelOfDetails(..))
import Utils.Lens as UL

type DSL = CC.InnerCardParentDSL PS.State Query PCS.ChildQuery PCS.ChildSlot

type HTML = CC.InnerCardParentHTML Query PCS.ChildQuery PCS.ChildSlot

pivotTableBuilderComponent ∷ CC.CardOptions → CC.CardComponent
pivotTableBuilderComponent =
  CC.makeCardComponent (CT.ChartOptions CHT.PivotTable) $ H.parentComponent
    { render: PR.render
    , eval: coproduct evalCard evalOptions
    , initialState: const PS.initialState
    , receiver: const Nothing
    }

evalCard ∷ CC.CardEvalQuery ~> DSL
evalCard = case _ of
  CC.Activate next →
    pure next
  CC.Deactivate next →
    pure next
  CC.Save k →
    map (k ∘ Card.BuildPivotTable ∘ PS.modelFromState) H.get
  CC.Load card next → do
    case card of
      Card.BuildPivotTable model →
        H.modify (PS.stateFromModel model)
      _ → pure unit
    pure next
  CC.ReceiveInput _ _ next →
    pure next
  CC.ReceiveOutput _ _ next →
    pure next
  CC.ReceiveState evalState next → do
    for_ (evalState ^? _Axes) \axes → do
      H.modify _ { axes = axes }
    pure next
  CC.ReceiveDimensions dims reply → do
    _ ← H.query' PCS.cpTransform unit (H.action AS.UpdateDimensions)
    pure $ reply
      if dims.width < 540.0 || dims.height < 360.0
        then Low
        else High

evalOptions ∷ Query ~> DSL
evalOptions = case _ of
  AddGroupBy next → do
    st ← H.get
    let vals = PS.selectGroupByValues st.axes
    H.modify _ { selecting = Just (PS.SelectGroupBy vals) }
    pure next
  AddColumn next → do
    st ← H.get
    let vals = PS.selectColumnValues st.axes
    H.modify _ { selecting = Just (PS.SelectColumn vals) }
    pure next
  Remove (ForGroupBy slot) next → do
    H.modify \st →
      st { dimensions = Array.filter (not ∘ eq slot ∘ fst) st.dimensions }
    H.raise CC.modelUpdate
    pure next
  Remove (ForColumn slot) next → do
    H.modify \st →
      st { columns = Array.filter (not ∘ eq slot ∘ fst) st.columns }
    H.raise CC.modelUpdate
    pure next
  ChangeLabel fd label next → do
    let
      label'
        | label ≡ "" = Nothing
        | otherwise  = Just (D.Static label)
    case fd of
      ForGroupBy slot →
        H.modify (PS._dimensions ∘ UL.lookup slot ∘ D._category .~ label')
      ForColumn slot →
        H.modify (PS._columns ∘ UL.lookup slot ∘ Lens._2 ∘ D._category .~ label')
    H.raise CC.modelUpdate
    pure next
  Configure (ForGroupBy slot) next → do
    st ← H.get
    let
      groupBy = st.dimensions ^? UL.lookup slot ∘ D._value
      selection = join $ groupBy ^? _Just ∘ D._transform
      options = case groupBy of
        Just (D.Projection mbTr cursor) →
          transformOptions (T.axisTransforms (Ax.axisType cursor st.axes) mbTr) mbTr
        _ → mempty
      selecting = PS.SelectTransform (ForGroupBy slot) selection options
    H.modify _ { selecting = Just selecting }
    pure next
  Configure (ForColumn slot) next → do
    st ← H.get
    let
      col = st.columns ^? UL.lookup slot ∘ Lens._2 ∘ D._value
      selection = join $ col ^? _Just ∘ D._transform
      options = case col of
        Just (D.Projection mbTr (PTM.Column cursor)) →
          transformOptions (T.axisTransforms (Ax.axisType cursor st.axes) mbTr) mbTr
        Just (D.Projection mbTr PTM.All) | rootAxes st.axes →
          transformOptions (T.axisTransforms (Ax.axisType J.JCursorTop st.axes) mbTr) mbTr
        _ → mempty
      selecting = PS.SelectTransform (ForColumn slot) selection options
    H.modify _ { selecting = Just selecting }
    pure next
  SetupFormatting slot next → do
    pure next
  OrderStart (ForGroupBy slot) ev next → do
    let
      opts =
        { source: slot
        , over: Nothing
        , offset: 0.0
        }
    H.subscribe $ Drag.dragEventSource ev \drag →
      Just (right (Ordering (ForGroupBy slot) drag H.Listening))
    H.modify _ { orderingDimension = Just opts }
    pure next
  Ordering (ForGroupBy slot) ev next → do
    st ← H.get
    for_ st.orderingDimension \opts →
      case ev of
        Drag.Move _ d →
          H.modify _ { orderingDimension = Just opts { offset = d.offsetY } }
        Drag.Done _ →
          case opts.over of
            Just slot' → do
              H.modify _
                { orderingDimension = Nothing
                , dimensions = PS.reorder slot slot' st.dimensions
                }
              H.raise CC.modelUpdate
            Nothing →
              H.modify _ { orderingDimension = Nothing }
    pure next
  OrderOver (ForGroupBy slot) next → do
    st ← H.get
    for_ st.orderingDimension \opts →
      H.modify _ { orderingDimension = Just (opts { over = Just slot }) }
    pure next
  OrderOut (ForGroupBy slot) next → do
    st ← H.get
    for_ st.orderingDimension \opts →
      H.modify _ { orderingDimension = Just (opts { over = Nothing }) }
    pure next
  OrderStart (ForColumn slot) ev next → do
    let
      opts =
        { source: slot
        , over: Nothing
        , offset: 0.0
        }
    H.subscribe $ Drag.dragEventSource ev \drag →
      Just (right (Ordering (ForColumn slot) drag H.Listening))
    H.modify _ { orderingColumn = Just opts }
    pure next
  Ordering (ForColumn slot) ev next → do
    st ← H.get
    for_ st.orderingColumn \opts →
      case ev of
        Drag.Move _ d →
          H.modify _ { orderingColumn = Just opts { offset = d.offsetX } }
        Drag.Done _ →
          case opts.over of
            Just slot' → do
              H.modify _
                { orderingColumn = Nothing
                , columns = PS.reorder slot slot' st.columns
                }
              H.raise CC.modelUpdate
            Nothing →
              H.modify _ { orderingColumn = Nothing }
    pure next
  OrderOver (ForColumn slot) next → do
    st ← H.get
    for_ st.orderingColumn \opts →
      H.modify _ { orderingColumn = Just (opts { over = Just slot }) }
    pure next
  OrderOut (ForColumn slot) next → do
    st ← H.get
    for_ st.orderingColumn \opts →
      H.modify _ { orderingColumn = Just (opts { over = Nothing }) }
    pure next
  HandleGroupByPicker msg next → do
    case msg of
      DPC.Dismiss →
        H.modify _ { selecting = Nothing }
      DPC.Confirm value → do
        st ← H.get
        let
          value' = flattenJCursors value
          cell = D.projectionWithCategory (D.defaultJCursorCategory value') value'
        H.modify _
          { fresh = st.fresh + 1
          , dimensions = Array.snoc st.dimensions (st.fresh × cell)
          , selecting = Nothing
          }
        H.raise CC.modelUpdate
    pure next
  HandleColumnPicker msg next → do
    case msg of
      DPC.Dismiss →
        H.modify _ { selecting = Nothing }
      DPC.Confirm value → do
        st ← H.get
        let
          value' = flattenColumns value
          cell = case value' of
            PTM.All | not (rootAxes st.axes) →
              D.Dimension (Just (D.Static "count")) (D.Projection (Just T.Count) PTM.All)
            _ → D.projectionWithCategory (PTM.defaultColumnCategory value') value'
        H.modify _
          { fresh = st.fresh + 1
          , columns = Array.snoc st.columns (st.fresh × PTM.Automatic × cell)
          , selecting = Nothing
          }
        H.raise CC.modelUpdate
    pure next
  HandleTransformPicker fd msg next → do
    case msg of
      AS.Dismiss →
        H.modify _ { selecting = Nothing }
      AS.Confirm mbt →
        H.modify
          $ _ { selecting = Nothing }
          ∘ case fd of
              ForGroupBy slot → PS.setGroupByTransform mbt slot
              ForColumn slot → PS.setColumnTransform mbt slot
    H.raise CC.modelUpdate
    pure next

transformOptions ∷ Array T.Transform → Maybe T.Transform → Array T.Transform
transformOptions options = case _ of
  Just t | not (F.elem t options) → Array.cons t options
  _ → options

rootAxes ∷ Ax.Axes → Boolean
rootAxes ax =
  onlyTop ax.value
  || onlyTop ax.time
  || onlyTop ax.category
  || onlyTop ax.date
  || onlyTop ax.datetime
  where
  onlyTop = eq [ J.JCursorTop ] ∘ Array.fromFoldable
