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

module SlamData.Workspace.Card.Setups.Chart.PivotTable.Component.Render where

import SlamData.Prelude

import CSS as C
import Data.Array as Array
import Data.Int (toNumber)
import Halogen as H
import Halogen.Component.Proxy as HCP
import Halogen.HTML as HH
import Halogen.HTML.CSS as HC
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.HTML.Properties.ARIA as ARIA
import SlamData.Workspace.Card.Component as CC
import SlamData.Workspace.Card.Setups.ActionSelect.Component as AS
import SlamData.Workspace.Card.Setups.Chart.PivotTable.Component.ChildSlot as PCS
import SlamData.Workspace.Card.Setups.Chart.PivotTable.Component.Query (Query(..), ForDimension(..))
import SlamData.Workspace.Card.Setups.Chart.PivotTable.Component.State as PS
import SlamData.Workspace.Card.Setups.Chart.PivotTable.Model as PTM
import SlamData.Workspace.Card.Setups.Dimension as D
import SlamData.Workspace.Card.Setups.DimensionPicker.Column (showColumn)
import SlamData.Workspace.Card.Setups.DimensionPicker.Component as DPC
import SlamData.Workspace.Card.Setups.Inputs as I
import SlamData.Workspace.Card.Setups.Transform as T
import SlamData.Workspace.Card.Setups.Transform.Numeric as N
import SlamData.Workspace.Card.Setups.Transform.Place.Component as TPC
import Utils (showPrettyJCursor, showJCursorTip)

type HTML = CC.InnerCardParentHTML Query PCS.ChildQuery PCS.ChildSlot

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
        (renderedDimensions st.orderingColumn st.dimensions)
    , HH.div
        [ HP.classes [ HH.ClassName "sd-pivot-options-cols" ] ]
        (renderedColumns st.orderingColumn st.columns)
    , HH.div
        [ HP.classes [ HH.ClassName "sd-pivot-options-preview" ] ]
        []
    , maybe (HH.text "") renderSelect st.selecting
    ]

renderSelect ∷ PS.Selecting → HTML
renderSelect = case _ of
  PS.SelectColumn values →
    HH.slot' PCS.cpCol unit
      (DPC.picker
        { title: "Choose column"
        , label: DPC.labelNode (showColumn showJCursorTip)
        , render: DPC.renderNode (showColumn showJCursorTip)
        , values
        , isSelectable: DPC.isLeafPath
        })
      unit
      (Just ∘ right ∘ H.action ∘ HandleColumnPicker)
  PS.SelectGroupBy values →
    HH.slot' PCS.cpDim unit
      (DPC.picker
        { title: "Choose dimension"
        , label: DPC.labelNode showJCursorTip
        , render: DPC.renderNode showJCursorTip
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
      , toLabel: \t -> { text: T.prettyPrintTransform t, icon: Nothing }
      , deselectable: true
      , toSelection: case _ of
          T.Numeric (N.Floor _) → Just $ HCP.proxy TPC.transformFloor
          T.Numeric (N.Round _) → Just $ HCP.proxy TPC.transformRound
          T.Numeric (N.Ceil _) → Just $ HCP.proxy TPC.transformCeil
          _ → Nothing
      }
      (Just ∘ right ∘ H.action ∘ HandleTransformPicker slot)

renderedDimensions
  ∷ Maybe PS.OrderingOpts
  → Array (Int × PTM.GroupByDimension)
  → Array HTML
renderedDimensions orderingDimension dimensions =
  let
    len  = Array.length dimensions + 1
    size = 100.0 / toNumber len
  in
    map (renderDimension orderingDimension size) dimensions <>
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

renderDimension
  ∷ Maybe PS.OrderingOpts
  → Number
  → Int × PTM.GroupByDimension
  → HTML
renderDimension orderingDimension size (slot × dimension) =
  HH.div
    ([ HP.classes dimensionClasses
     , HC.style (C.height (C.pct size))
     ] <> dimensionEvents)
    [ HH.div
        [ HP.classes [ HH.ClassName "sd-pivot-options-dim-inner"]
        , HC.style (dimensionStyles size)
        ]
        [ I.dimensionButton
            { configurable: true
            , formattable: false
            , dimension
            , showLabel: absurd
            , showDefaultLabel: showPrettyJCursor
            , showValue: showPrettyJCursor
            , onLabelChange: HE.input (\l → right ∘ ChangeLabel (ForGroupBy slot) l)
            , onDismiss: HE.input_ (right ∘ Remove (ForGroupBy slot))
            , onConfigure: HE.input_ (right ∘ Configure (ForGroupBy slot))
            , onSetupFormatting: const Nothing
            , onMouseDown: HE.input (\e → right ∘ OrderStart (ForGroupBy slot) e)
            , onClick: const Nothing
            , onLabelClick: const Nothing
            , disabled: false
            , dismissable: true
            }
        ]
    ]
  where

    dimensionClasses =
      [ HH.ClassName "sd-pivot-options-dim" ] <>
        case orderingDimension of
          Just opts | opts.source == slot → [ HH.ClassName "ordering" ]
          Just opts | opts.over == Just slot → [ HH.ClassName "ordering-over" ]
          _ → []

    dimensionStyles size = do
      case orderingDimension of
        Just opts | opts.source == slot → C.top (C.px opts.offset)
        _ → pure unit

    dimensionEvents =
      if isJust orderingDimension
        then
          [ HE.onMouseOver (HE.input_ (right ∘ OrderOver (ForGroupBy slot)))
          , HE.onMouseOut (HE.input_ (right ∘ OrderOut (ForGroupBy slot)))
          ]
        else
          []

renderedColumns
  ∷ Maybe PS.OrderingOpts
  → Array (Int × PTM.ColumnDimension)
  → Array HTML
renderedColumns orderingColumn columns =
  let
    len  = Array.length columns + 1
    size = 100.0 / toNumber len
  in
    map (renderColumn orderingColumn size) columns <>
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

renderColumn
  ∷ Maybe PS.OrderingOpts
  → Number
  → Int × PTM.ColumnDimension
  → HTML
renderColumn orderingColumn size (slot × formatOptions × dimension@(D.Dimension label cat)) =
  HH.div
    ([ HP.classes columnClasses
     , HC.style (C.width (C.pct size))
     ] <> columnEvents)
    [ HH.div
        [ HP.classes [ HH.ClassName "sd-pivot-options-col-inner"]
        , HC.style (columnStyles size)
        ]
        [ I.dimensionButton
            { configurable: true
            , formattable: true
            , dimension
            , showLabel: absurd
            , showDefaultLabel: showColumn showPrettyJCursor
            , showValue: showColumn showPrettyJCursor
            , onLabelChange: HE.input (\l → right ∘ ChangeLabel (ForColumn slot) l)
            , onDismiss: HE.input_ (right ∘ Remove (ForColumn slot))
            , onConfigure: HE.input_ (right ∘ Configure (ForColumn slot))
            , onSetupFormatting: HE.input_ (right ∘ SetupFormatting (ForColumn slot))
            , onMouseDown: HE.input (\e → right ∘ OrderStart (ForColumn slot) e)
            , onClick: const Nothing
            , onLabelClick: const Nothing
            , disabled: false
            , dismissable: true
            }
        ]
    ]
  where

    columnClasses =
      [ HH.ClassName "sd-pivot-options-col" ] <>
        case orderingColumn of
          Just opts | opts.source == slot → [ HH.ClassName "ordering" ]
          Just opts | opts.over == Just slot → [ HH.ClassName "ordering-over" ]
          _ → []

    columnStyles size =
      case orderingColumn of
        Just opts | opts.source == slot → C.left (C.px opts.offset)
        _ → pure unit

    columnEvents =
      if isJust orderingColumn
        then
          [ HE.onMouseOver (HE.input_ (right ∘ OrderOver (ForColumn slot)))
          , HE.onMouseOut (HE.input_ (right ∘ OrderOut (ForColumn slot)))
          ]
        else
          []
