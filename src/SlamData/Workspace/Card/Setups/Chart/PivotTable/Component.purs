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
import Data.Int (toNumber)
import Data.Lens ((^?))

import CSS as C
import Halogen as H
import Halogen.Component.Utils.Drag as Drag
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.HTML.Properties.ARIA as ARIA
import Halogen.HTML.CSS as HC

import SlamData.Form.Select as S
import SlamData.Workspace.Card.Setups.Chart.Aggregation as Ag
import SlamData.Workspace.Card.Setups.DimensionPicker.Component as DPC
import SlamData.Workspace.Card.Setups.DimensionPicker.Column (flattenColumns)
import SlamData.Workspace.Card.Setups.DimensionPicker.JCursor (flattenJCursors)
import SlamData.Workspace.Card.Setups.Chart.PivotTable.Component.ChildSlot as PCS
import SlamData.Workspace.Card.Setups.Chart.PivotTable.Component.Query (Query(..))
import SlamData.Workspace.Card.Setups.Chart.PivotTable.Component.State as PS
import SlamData.Workspace.Card.Setups.Chart.PivotTable.Model (Column(..))
import SlamData.Workspace.Card.CardType as CT
import SlamData.Workspace.Card.CardType.ChartType as CHT
import SlamData.Workspace.Card.Component as CC
import SlamData.Workspace.Card.Eval.State (_Axes)
import SlamData.Workspace.Card.Model as Card
import SlamData.Workspace.LevelOfDetails (LevelOfDetails(..))

type DSL = CC.InnerCardParentDSL PS.State Query PCS.ChildQuery PCS.ChildSlot

type HTML = CC.InnerCardParentHTML Query PCS.ChildQuery PCS.ChildSlot

pivotTableBuilderComponent ∷ CC.CardOptions → CC.CardComponent
pivotTableBuilderComponent =
  CC.makeCardComponent (CT.ChartOptions CHT.PivotTable) $ H.parentComponent
    { render
    , eval: coproduct evalCard evalOptions
    , initialState: const PS.initialState
    , receiver: const Nothing
    }

render ∷ PS.State → HTML
render st =
  HH.div
    [ HP.classes [ HH.ClassName "sd-pivot-options" ] ]
    [ HH.div
        [ HP.classes [ HH.ClassName "sd-pivot-options-corner" ] ]
        [ HH.span_ [ HH.text "Dimensions" ]
        , HH.span_ [ HH.text "Columns" ]
        ]
    , HH.div
        [ HP.classes [ HH.ClassName "sd-pivot-options-dims" ] ]
        renderedDimensions
    , HH.div
        [ HP.classes [ HH.ClassName "sd-pivot-options-cols" ] ]
        renderedColumns
    , maybe (HH.text "") renderSelect st.selecting
    ]
  where
  renderSelect = case _ of
    PS.Col values →
      HH.slot' PCS.cpCol unit
        (DPC.picker
          { title: "Choose column"
          , label: DPC.labelNode showColumn
          , render: DPC.renderNode showColumn
          , values
          , isSelectable: DPC.isLeafPath
          })
        unit
        (Just ∘ right ∘ H.action ∘ HandleColPicker)
    PS.Dim values →
      HH.slot' PCS.cpDim unit
        (DPC.picker
          { title: "Choose dimension"
          , label: DPC.labelNode showJCursor
          , render: DPC.renderNode showJCursor
          , values
          , isSelectable: DPC.isLeafPath
          })
        unit
        (Just ∘ right ∘ H.action ∘ HandleDimPicker)

  renderedDimensions =
    let
      len  = Array.length st.dimensions + 1
      size = 100.0 / toNumber len
    in
      map (renderDimension size) st.dimensions <>
      [ HH.div
          [ HP.classes [ HH.ClassName "sd-pivot-options-dim" ]
          , HC.style (C.height (C.pct size))
          ]
          [ HH.div
              [ HP.classes [ HH.ClassName "sd-pivot-options-dim-inner"] ]
              [ HH.button
                  [ HP.classes [ HH.ClassName "sd-pivot-options-plus" ]
                  , HE.onClick (HE.input_ (right ∘ AddDimension))
                  , ARIA.label "Add dimension"
                  , HP.title "Add dimension"
                  ]
                  []
              ]
          ]
      ]

  renderDimension size (slot × dim) =
    HH.div
      ([ HP.classes (dimensionClasses slot)
       , HC.style (C.height (C.pct size))
       ] <> dimensionEvents slot)
      [ HH.div
          [ HP.classes [ HH.ClassName "sd-pivot-options-dim-inner"]
          , HC.style (dimensionStyles slot size)
          ]
          [ HH.button
              [ HP.classes [ HH.ClassName "sd-pivot-options-label" ]
              , HE.onMouseDown (HE.input (\e → right ∘ OrderDimensionStart slot e))
              ]
              [ HH.text (showJCursor dim) ]
          , HH.button
              [ HP.classes [ HH.ClassName "sd-dismiss-button" ]
              , HP.title "Delete dimension"
              , ARIA.label "Delete dimension"
              , HE.onClick (HE.input_ (right ∘ RemoveDimension slot))
              ]
              [ HH.text "×"]
          ]
      ]

  dimensionClasses slot =
    [ HH.ClassName "sd-pivot-options-dim" ] <>
      case st.orderingDimension of
        Just opts | opts.source == slot → [ HH.ClassName "ordering" ]
        Just opts | opts.over == Just slot → [ HH.ClassName "ordering-over" ]
        _ → []

  dimensionStyles slot size = do
    case st.orderingDimension of
      Just opts | opts.source == slot → C.top (C.px opts.offset)
      _ → pure unit

  dimensionEvents slot =
    if isJust st.orderingDimension
      then
        [ HE.onMouseOver (HE.input_ (right ∘ OrderOverDimension slot))
        , HE.onMouseOut (HE.input_ (right ∘ OrderOutDimension slot))
        ]
      else
        []

  renderedColumns =
    let
      len  = Array.length st.columns + 1
      size = 100.0 / toNumber len
    in
      map (renderColumn size) st.columns <>
      [ HH.div
          [ HP.classes [ HH.ClassName "sd-pivot-options-col" ]
          , HC.style (C.width (C.pct size))
          ]
          [ HH.div
              [ HP.classes [ HH.ClassName "sd-pivot-options-col-inner"] ]
              [ HH.div
                  [ HP.classes [ HH.ClassName "sd-pivot-options-col-value" ] ]
                  [ HH.button
                      [ HP.classes [ HH.ClassName "sd-pivot-options-plus" ]
                      , HE.onClick (HE.input_ (right ∘ AddColumn))
                      , ARIA.label "Add column"
                      , HP.title "Add column"
                      ]
                      []
                  ]
              , HH.div
                  [ HP.classes [ HH.ClassName "sd-pivot-options-col-aggregation" ] ]
                  []
              ]
          ]
      ]

  renderColumn size (slot × col) =
    HH.div
      ([ HP.classes (columnClasses slot)
       , HC.style (C.width (C.pct size))
       ] <> columnEvents slot)
      [ HH.div
          [ HP.classes [ HH.ClassName "sd-pivot-options-col-inner"]
          , HC.style (columnStyles slot size)
          ]
          [ HH.div
              [ HP.classes [ HH.ClassName "sd-pivot-options-col-value" ] ]
              [ HH.button
                  [ HP.classes [ HH.ClassName "sd-pivot-options-label" ]
                  , HE.onMouseDown (HE.input (\e → right ∘ OrderColumnStart slot e))
                  ]
                  [ HH.text (showColumn col)
                  ]
              , HH.button
                  [ HP.classes [ HH.ClassName "sd-dismiss-button" ]
                  , HP.title "Delete column"
                  , ARIA.label "Delete column"
                  , HE.onClick (HE.input_ (right ∘ RemoveColumn slot))
                  ]
                  [ HH.text "×"]
              ]
          , HH.div
              [ HP.classes [ HH.ClassName "sd-pivot-options-col-aggregation" ] ]
              case col of
                Column { valueAggregation } → [ columnSelect slot valueAggregation ]
                _ → []
          ]
      ]

  columnClasses slot =
    [ HH.ClassName "sd-pivot-options-col" ] <>
      case st.orderingColumn of
        Just opts | opts.source == slot → [ HH.ClassName "ordering" ]
        Just opts | opts.over == Just slot → [ HH.ClassName "ordering-over" ]
        _ → []

  columnStyles slot size = do
    case st.orderingColumn of
      Just opts | opts.source == slot → C.left (C.px opts.offset)
      _ → pure unit

  columnEvents slot =
    if isJust st.orderingColumn
      then
        [ HE.onMouseOver (HE.input_ (right ∘ OrderOverColumn slot))
        , HE.onMouseOut (HE.input_ (right ∘ OrderOutColumn slot))
        ]
      else
        []

  columnSelect slot ag =
    HH.div
      [ HP.classes [ HH.ClassName "list-group" ] ]
      (map (selectBtn slot ag)
        [ Nothing
        , Just Ag.Maximum
        , Just Ag.Minimum
        , Just Ag.Average
        , Just Ag.Sum
        -- Not supported by the backend
        -- , Just Ag.Product
        ])

  selectBtn slot ag ctr =
    HH.button
      [ HP.classes
          ([ HH.ClassName "list-group-item" ]
           <> (HH.ClassName "active" <$ guard (ctr == ag)))
      , HE.onClick (HE.input_ (right ∘ ChooseAggregation slot ctr))
      ]
      [ HH.text (maybe "Tabulate" S.stringVal ctr) ]

  showColumn (Column { value }) = showJCursor value
  showColumn Count = "COUNT"

  showJCursor (J.JField i c) = i <> show c
  showJCursor c = show c

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
    pure $ reply
      if dims.width < 540.0 || dims.height < 360.0
        then Low
        else High

evalOptions ∷ Query ~> DSL
evalOptions = case _ of
  AddDimension next → do
    st ← H.get
    H.modify _ { selecting = Just (PS.Dim (PS.selectDimensionValues st.axes)) }
    pure next
  AddColumn next → do
    st ← H.get
    H.modify _ { selecting = Just (PS.Col (PS.selectColumnValues st.axes)) }
    pure next
  RemoveDimension slot next → do
    H.modify \st →
      st { dimensions = Array.filter (not ∘ eq slot ∘ fst) st.dimensions }
    H.raise CC.modelUpdate
    pure next
  RemoveColumn slot next → do
    H.modify \st →
      st { columns = Array.filter (not ∘ eq slot ∘ fst) st.columns }
    H.raise CC.modelUpdate
    pure next
  OrderDimensionStart slot ev next → do
    let
      opts =
        { source: slot
        , over: Nothing
        , offset: 0.0
        }
    H.subscribe $ Drag.dragEventSource ev \drag →
      Just (right (OrderingDimension slot drag H.Listening))
    H.modify _ { orderingDimension = Just opts }
    pure next
  OrderingDimension slot ev next → do
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
  OrderOverDimension slot next → do
    st ← H.get
    for_ st.orderingDimension \opts →
      H.modify _ { orderingDimension = Just (opts { over = Just slot }) }
    pure next
  OrderOutDimension slot next → do
    st ← H.get
    for_ st.orderingDimension \opts →
      H.modify _ { orderingDimension = Just (opts { over = Nothing }) }
    pure next
  OrderColumnStart slot ev next → do
    let
      opts =
        { source: slot
        , over: Nothing
        , offset: 0.0
        }
    H.subscribe $ Drag.dragEventSource ev \drag →
      Just (right (OrderingColumn slot drag H.Listening))
    H.modify _ { orderingColumn = Just opts }
    pure next
  OrderingColumn slot ev next → do
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
  OrderOverColumn slot next → do
    st ← H.get
    for_ st.orderingColumn \opts →
      H.modify _ { orderingColumn = Just (opts { over = Just slot }) }
    pure next
  OrderOutColumn slot next → do
    st ← H.get
    for_ st.orderingColumn \opts →
      H.modify _ { orderingColumn = Just (opts { over = Nothing }) }
    pure next
  ChooseAggregation slot ag next → do
    st ← H.get
    H.modify (PS.setColumnAggregation slot ag)
    H.raise CC.modelUpdate
    pure next
  HandleDimPicker msg next → do
    case msg of
      DPC.Dismiss →
        H.modify _ { selecting = Nothing }
      DPC.Confirm value → do
        st ← H.get
        H.modify _
          { fresh = st.fresh + 1
          , dimensions = Array.snoc st.dimensions (st.fresh × (flattenJCursors value))
          , selecting = Nothing
          }
        H.raise CC.modelUpdate
    pure next
  HandleColPicker msg next → do
    case msg of
      DPC.Dismiss →
        H.modify _ { selecting = Nothing }
      DPC.Confirm value → do
        st ← H.get
        H.modify _
          { fresh = st.fresh + 1
          , columns = Array.snoc st.columns (st.fresh × (flattenColumns value))
          , selecting = Nothing
          }
        H.raise CC.modelUpdate
    pure next
