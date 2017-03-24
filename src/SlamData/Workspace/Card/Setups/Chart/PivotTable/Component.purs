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
import Data.Int (toNumber)
import Data.Lens ((^?), (.~), _Just)
import Data.StrMap as SM

import CSS as C
import Halogen as H
import Halogen.Component.Proxy as HCP
import Halogen.Component.Utils.Drag as Drag
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.HTML.Properties.ARIA as ARIA
import Halogen.HTML.CSS as HC

import SlamData.Workspace.Card.Setups.Axis as Ax
import SlamData.Workspace.Card.Setups.ActionSelect.Component as AS
import SlamData.Workspace.Card.Setups.Dimension as D
import SlamData.Workspace.Card.Setups.DimensionPicker.Component as DPC
import SlamData.Workspace.Card.Setups.DimensionPicker.Column (showColumn)
import SlamData.Workspace.Card.Setups.DimensionPicker.JCursor (showJCursor, showJCursorTip)
import SlamData.Workspace.Card.Setups.DimensionPicker.Variables (flattenVariables)
import SlamData.Workspace.Card.Setups.Chart.PivotTable.Component.ChildSlot as PCS
import SlamData.Workspace.Card.Setups.Chart.PivotTable.Component.Query (Query(..), ForDimension(..))
import SlamData.Workspace.Card.Setups.Chart.PivotTable.Component.State as PS
import SlamData.Workspace.Card.Setups.Chart.PivotTable.Model as PTM
import SlamData.Workspace.Card.Setups.Inputs as I
import SlamData.Workspace.Card.Setups.Transform as T
import SlamData.Workspace.Card.Setups.Transform.Numeric as N
import SlamData.Workspace.Card.Setups.Transform.Place.Component as TPC
import SlamData.Workspace.Card.CardType as CT
import SlamData.Workspace.Card.CardType.ChartType as CHT
import SlamData.Workspace.Card.Component as CC
import SlamData.Workspace.Card.Eval.State (_Axes)
import SlamData.Workspace.Card.Model as Card
import SlamData.Workspace.Card.Port as Port
import SlamData.Workspace.Card.Port.VarMap as VM
import SlamData.Workspace.LevelOfDetails (LevelOfDetails(..))

import Utils.Lens as UL

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
    , HH.div
        [ HP.classes [ HH.ClassName "sd-pivot-options-preview" ] ]
        []
    , maybe (HH.text "") renderSelect st.selecting
    ]
  where
  renderSelect = case _ of
    PS.SelectColumn values →
      HH.slot' PCS.cpCol unit
        (DPC.picker
          { title: "Choose column"
          , label: DPC.labelNode (either unwrap (showColumn showJCursorTip))
          , render: DPC.renderNode (either unwrap (showColumn showJCursorTip))
          , values
          , isSelectable: DPC.isLeafPath
          })
        unit
        (Just ∘ right ∘ H.action ∘ HandleColumnPicker)
    PS.SelectGroupBy values →
      HH.slot' PCS.cpDim unit
        (DPC.picker
          { title: "Choose dimension"
          , label: DPC.labelNode (either unwrap showJCursorTip)
          , render: DPC.renderNode (either unwrap showJCursorTip)
          , values
          , isSelectable: DPC.isLeafPath
          })
        unit
        (Just ∘ right ∘ H.action ∘ HandleGroupByPicker)
    PS.SelectTransform slot selection options →
      HH.slot' PCS.cpTransform unit AS.component
        { options
        , selection: (\a → a × a) <$> selection
        , title: "Choose transformation"
        , toLabel: T.prettyPrintTransform
        , toSelection: case _ of
            T.Numeric (N.Floor _) → Just $ HCP.proxy TPC.transformFloor
            T.Numeric (N.Round _) → Just $ HCP.proxy TPC.transformRound
            T.Numeric (N.Ceil _) → Just $ HCP.proxy TPC.transformCeil
            _ → Nothing
        }
        (Just ∘ right ∘ H.action ∘ HandleTransformPicker slot)

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
                  , HE.onClick (HE.input_ (right ∘ AddGroupBy))
                  , ARIA.label "Add dimension"
                  , HP.title "Add dimension"
                  ]
                  []
              ]
          ]
      ]

  renderDimension size (slot × dimension) =
    HH.div
      ([ HP.classes (dimensionClasses slot)
       , HC.style (C.height (C.pct size))
       ] <> dimensionEvents slot)
      [ HH.div
          [ HP.classes [ HH.ClassName "sd-pivot-options-dim-inner"]
          , HC.style (dimensionStyles slot size)
          ]
          [ I.dimensionButton
              { configurable: true
              , dimension
              , showLabel: absurd
              , showDefaultLabel: either unwrap showJCursor
              , showValue: either unwrap showJCursor
              , onLabelChange: HE.input (\l → right ∘ ChangeLabel (ForGroupBy slot) l)
              , onDismiss: HE.input_ (right ∘ Remove (ForGroupBy slot))
              , onConfigure: HE.input_ (right ∘ Configure (ForGroupBy slot))
              , onMouseDown: HE.input (\e → right ∘ OrderStart (ForGroupBy slot) e)
              }
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
        [ HE.onMouseOver (HE.input_ (right ∘ OrderOver (ForGroupBy slot)))
        , HE.onMouseOut (HE.input_ (right ∘ OrderOut (ForGroupBy slot)))
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
              ]
          ]
      ]

  renderColumn size (slot × dimension) =
    HH.div
      ([ HP.classes (columnClasses slot)
       , HC.style (C.width (C.pct size))
       ] <> columnEvents slot)
      [ HH.div
          [ HP.classes [ HH.ClassName "sd-pivot-options-col-inner"]
          , HC.style (columnStyles slot size)
          ]
          [ I.dimensionButton
              { configurable: true
              , dimension
              , showLabel: absurd
              , showDefaultLabel: either unwrap (showColumn showJCursor)
              , showValue: either unwrap (showColumn showJCursor)
              , onLabelChange: HE.input (\l → right ∘ ChangeLabel (ForColumn slot) l)
              , onDismiss: HE.input_ (right ∘ Remove (ForColumn slot))
              , onConfigure: HE.input_ (right ∘ Configure (ForColumn slot))
              , onMouseDown: HE.input (\e → right ∘ OrderStart (ForColumn slot) e)
              }
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
        [ HE.onMouseOver (HE.input_ (right ∘ OrderOver (ForColumn slot)))
        , HE.onMouseOut (HE.input_ (right ∘ OrderOut (ForColumn slot)))
        ]
      else
        []

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
  CC.ReceiveInput _ varMap next → do
    let vars = Port.flattenResources varMap
    st ← H.get
    when (vars ≠ st.vars) do
      H.modify _ { vars = vars }
    pure next
  CC.ReceiveOutput _ _ next →
    pure next
  CC.ReceiveState evalState next → do
    for_ (evalState ^? _Axes) \axes → do
      H.modify _ { axes = axes }
    pure next
  CC.ReceiveDimensions dims reply → do
    H.query' PCS.cpTransform unit (H.action AS.UpdateDimensions)
    pure $ reply
      if dims.width < 540.0 || dims.height < 360.0
        then Low
        else High

evalOptions ∷ Query ~> DSL
evalOptions = case _ of
  AddGroupBy next → do
    st ← H.get
    let vals = PS.selectGroupByValues (VM.variables st.vars) st.axes
    H.modify _ { selecting = Just (PS.SelectGroupBy vals) }
    pure next
  AddColumn next → do
    st ← H.get
    let vals = PS.selectColumnValues (VM.variables st.vars) st.axes
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
        H.modify (PS._columns ∘ UL.lookup slot ∘ D._category .~ label')
    H.raise CC.modelUpdate
    pure next
  Configure (ForGroupBy slot) next → do
    st ← H.get
    let
      groupBy = st.dimensions ^? UL.lookup slot ∘ D._value
      selection = join $ groupBy ^? _Just ∘ D._transform
      options = case groupBy of
        Just (D.Projection mbTr (Left (VM.Var var))) →
          transformOptions (maybe mempty (flip T.varTransforms mbTr) (SM.lookup var st.vars)) mbTr
        Just (D.Projection mbTr (Right cursor)) →
          transformOptions (T.axisTransforms (Ax.axisType cursor st.axes) mbTr) mbTr
        _ → mempty
      selecting = PS.SelectTransform (ForGroupBy slot) selection options
    H.modify _ { selecting = Just selecting }
    pure next
  Configure (ForColumn slot) next → do
    st ← H.get
    let
      col = st.columns ^? UL.lookup slot ∘ D._value
      selection = join $ col ^? _Just ∘ D._transform
      options = case col of
        Just (D.Projection mbTr (Left (VM.Var var))) →
          transformOptions (maybe mempty (flip T.varTransforms mbTr) (SM.lookup var st.vars)) mbTr
        Just (D.Projection mbTr (Right (PTM.Column cursor))) →
          transformOptions (T.axisTransforms (Ax.axisType cursor st.axes) mbTr) mbTr
        Just (D.Projection mbTr (Right PTM.All)) | rootAxes st.axes →
          transformOptions (T.axisTransforms (Ax.axisType J.JCursorTop st.axes) mbTr) mbTr
        _ → mempty
      selecting = PS.SelectTransform (ForColumn slot) selection options
    H.modify _ { selecting = Just selecting }
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
          value' = flattenVariables value
          cell = D.projectionWithCategory (either D.defaultVariableCategory D.defaultJCursorCategory value') value'
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
          value' = flattenVariables value
          cell = case value' of
            Right PTM.All | not (rootAxes st.axes) →
              D.Dimension (Just (D.Static "count")) (D.Projection (Just T.Count) (Right PTM.All))
            _ → D.projectionWithCategory (either D.defaultVariableCategory PTM.defaultColumnCategory value') value'
        H.modify _
          { fresh = st.fresh + 1
          , columns = Array.snoc st.columns (st.fresh × cell)
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
    onlyTop [ J.JCursorTop ] = true
    onlyTop _ = false
