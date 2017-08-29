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

module SlamData.Workspace.Card.Setups.PivotTable.Component where

import SlamData.Prelude

import Data.Argonaut as J
import Data.Array as Array
import Data.Foldable as F
import Data.Lens ((^?), (.~), _Just)
import Data.Lens as Lens
import Halogen as H
import Halogen.Component.Utils.Drag as Drag
import Halogen.HTML as HH
import SlamData.Monad (Slam)
import SlamData.Workspace.Card.CardId as CID
import SlamData.Workspace.Card.Component as CC
import SlamData.Workspace.Card.Setups.ActionSelect.Component as AS
import SlamData.Workspace.Card.Setups.Axis as Ax
import SlamData.Workspace.Card.Setups.PivotTable.Component.Render as PR
import SlamData.Workspace.Card.Setups.Dimension as D
import SlamData.Workspace.Card.Setups.DimensionPicker.Column (flattenColumns)
import SlamData.Workspace.Card.Setups.DimensionPicker.Component as DPC
import SlamData.Workspace.Card.Setups.DimensionPicker.JCursor (flattenJCursors)
import SlamData.Workspace.Card.Setups.DisplayOptions.Component as Display
import SlamData.Workspace.Card.Setups.PivotTable.Component.ChildSlot as CS
import SlamData.Workspace.Card.Setups.PivotTable.Component.Query as Q
import SlamData.Workspace.Card.Setups.PivotTable.Component.State as ST
import SlamData.Workspace.Card.Setups.PivotTable.Model as PTM
import SlamData.Workspace.Card.Setups.Transform as T
import SlamData.Workspace.Deck.DeckId as DID
import Utils.Lens as UL

type DSL = H.ParentDSL ST.State Q.Query CS.ChildQuery CS.ChildSlot Q.Message Slam

component ∷ CC.CardOptions → H.Component HH.HTML Q.Query Unit Q.Message Slam
component options =
  H.parentComponent
    { initialState: const ST.initialState
    , render: PR.render uniqueCardId
    , eval
    , receiver: const Nothing
    }
  where
    uniqueCardId =
      foldMap DID.toString options.cursor <> CID.toString options.cardId

raiseUpdate ∷ DSL Unit
raiseUpdate = do
  st ← H.get
  H.raise $ ST.modelFromState st

eval ∷ Q.Query ~> DSL
eval = case _ of
  Q.Load m next → do
    H.modify $ ST.stateFromModel m
    pure next
  Q.SetAxes axes next → do
    H.modify _{ axes = axes }
    pure next
  Q.AddGroupBy next → do
    st ← H.get
    let vals = ST.selectGroupByValues st.axes
    H.modify _ { selecting = Just (ST.SelectGroupBy vals) }
    pure next
  Q.AddColumn next → do
    st ← H.get
    let vals = ST.selectColumnValues st.axes
    H.modify _ { selecting = Just (ST.SelectColumn vals) }
    pure next
  Q.Remove (Q.ForGroupBy slot) next → do
    H.modify \st →
      st { dimensions = Array.filter (not ∘ eq slot ∘ fst) st.dimensions }
    raiseUpdate
    pure next
  Q.Remove (Q.ForColumn slot) next → do
    H.modify \st →
      st { columns = Array.filter (not ∘ eq slot ∘ fst) st.columns }
    raiseUpdate
    pure next
  Q.ChangeLabel fd label next → do
    let
      label'
        | label ≡ "" = Nothing
        | otherwise  = Just (D.Static label)
    case fd of
      Q.ForGroupBy slot →
        H.modify (ST._dimensions ∘ UL.lookup slot ∘ D._category .~ label')
      Q.ForColumn slot →
        H.modify (ST._columns ∘ UL.lookup slot ∘ Lens._2 ∘ D._category .~ label')
    raiseUpdate
    pure next
  Q.Configure (Q.ForGroupBy slot) next → do
    st ← H.get
    let
      groupBy = st.dimensions ^? UL.lookup slot ∘ D._value
      selection = join $ groupBy ^? _Just ∘ D._transform
      options = case groupBy of
        Just (D.Projection mbTr cursor) →
          transformOptions (T.axisTransforms (Ax.axisType cursor st.axes) mbTr) mbTr
        _ → mempty
      selecting = ST.SelectTransform (Q.ForGroupBy slot) selection options
    H.modify _ { selecting = Just selecting }
    pure next
  Q.Configure (Q.ForColumn slot) next → do
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
      selecting = ST.SelectTransform (Q.ForColumn slot) selection options
    H.modify _ { selecting = Just selecting }
    pure next
  Q.SetupFormatting (Q.ForColumn slot) next → do
    st ← H.get
    for_ (st.columns ^? UL.lookup slot ∘ Lens._1) \formatting → do
      let selecting = ST.SelectFormatting (Q.ForColumn slot) formatting
      H.modify _ { selecting = Just selecting }
    pure next
  Q.SetupFormatting _ next →
    pure next
  Q.OrderStart (Q.ForGroupBy slot) ev next → do
    let
      opts =
        { source: slot
        , over: Nothing
        , offset: 0.0
        }
    H.subscribe $ Drag.dragEventSource ev \drag →
      Just (Q.Ordering (Q.ForGroupBy slot) drag H.Listening)
    H.modify _ { orderingDimension = Just opts }
    pure next
  Q.Ordering (Q.ForGroupBy slot) ev next → do
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
                , dimensions = ST.reorder slot slot' st.dimensions
                }
              raiseUpdate
            Nothing →
              H.modify _ { orderingDimension = Nothing }
    pure next
  Q.OrderOver (Q.ForGroupBy slot) next → do
    st ← H.get
    for_ st.orderingDimension \opts →
      H.modify _ { orderingDimension = Just (opts { over = Just slot }) }
    pure next
  Q.OrderOut (Q.ForGroupBy slot) next → do
    st ← H.get
    for_ st.orderingDimension \opts →
      H.modify _ { orderingDimension = Just (opts { over = Nothing }) }
    pure next
  Q.OrderStart (Q.ForColumn slot) ev next → do
    let
      opts =
        { source: slot
        , over: Nothing
        , offset: 0.0
        }
    H.subscribe $ Drag.dragEventSource ev \drag →
      Just (Q.Ordering (Q.ForColumn slot) drag H.Listening)
    H.modify _ { orderingColumn = Just opts }
    pure next
  Q.Ordering (Q.ForColumn slot) ev next → do
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
                , columns = ST.reorder slot slot' st.columns
                }
              raiseUpdate
            Nothing →
              H.modify _ { orderingColumn = Nothing }
    pure next
  Q.OrderOver (Q.ForColumn slot) next → do
    st ← H.get
    for_ st.orderingColumn \opts →
      H.modify _ { orderingColumn = Just (opts { over = Just slot }) }
    pure next
  Q.OrderOut (Q.ForColumn slot) next → do
    st ← H.get
    for_ st.orderingColumn \opts →
      H.modify _ { orderingColumn = Just (opts { over = Nothing }) }
    pure next
  Q.HandleGroupByPicker msg next → do
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
        raiseUpdate
    pure next
  Q.HandleColumnPicker msg next → do
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
          , columns = Array.snoc st.columns (st.fresh × Display.initialDisplayOptions × cell)
          , selecting = Nothing
          }
        raiseUpdate
    pure next
  Q.HandleTransformPicker fd msg next → do
    case msg of
      AS.Dismiss →
        H.modify _ { selecting = Nothing }
      AS.Confirm mbt → do
        H.modify
          $ _ { selecting = Nothing }
          ∘ case fd of
              Q.ForGroupBy slot → ST.setGroupByTransform mbt slot
              Q.ForColumn slot → ST.setColumnTransform mbt slot
        raiseUpdate
    pure next
  Q.HandleFormatting fd msg next → do
    case msg of
      Display.Dismiss →
        H.modify _ { selecting = Nothing }
      Display.Confirm opts → do
        H.modify
          $ _ { selecting = Nothing }
          ∘ case fd of
              Q.ForColumn slot → ST.setColumnDisplayOptions opts slot
              Q.ForGroupBy _ → id
        raiseUpdate
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
