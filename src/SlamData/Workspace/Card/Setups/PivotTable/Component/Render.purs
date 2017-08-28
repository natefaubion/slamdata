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

module SlamData.Workspace.Card.Setups.PivotTable.Component.Render where

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
import SlamData.Monad (Slam)
import SlamData.Workspace.Card.Setups.ActionSelect.Component as AS
import SlamData.Workspace.Card.Setups.Dimension as D
import SlamData.Workspace.Card.Setups.DimensionPicker.Column (showColumn)
import SlamData.Workspace.Card.Setups.DimensionPicker.Component as DPC
import SlamData.Workspace.Card.Setups.DisplayOptions.Component as Display
import SlamData.Workspace.Card.Setups.Inputs as I
import SlamData.Workspace.Card.Setups.PivotTable.Component.ChildSlot as CS
import SlamData.Workspace.Card.Setups.PivotTable.Component.Query as Q
import SlamData.Workspace.Card.Setups.PivotTable.Component.State as PS
import SlamData.Workspace.Card.Setups.PivotTable.Model as PTM
import SlamData.Workspace.Card.Setups.Transform as T
import SlamData.Workspace.Card.Setups.Transform.Numeric as N
import SlamData.Workspace.Card.Setups.Transform.Place.Component as TPC
import Utils (showPrettyJCursor, showJCursorTip)

type HTML = H.ParentHTML Q.Query CS.ChildQuery CS.ChildSlot Slam

render ∷ String → PS.State → HTML
render uniqueCardId st =
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
    , maybe (HH.text "") (renderSelect uniqueCardId) st.selecting
    ]

renderSelect ∷ String → PS.Selecting → HTML
renderSelect uniqueCardId = case _ of
  PS.SelectColumn values →
    HH.slot' CS.cpCol unit
      (DPC.picker
        { title: "Choose column"
        , label: DPC.labelNode (showColumn showJCursorTip)
        , render: DPC.renderNode (showColumn showJCursorTip)
        , values
        , isSelectable: DPC.isLeafPath
        })
      unit
      (Just ∘ H.action ∘ Q.HandleColumnPicker)
  PS.SelectGroupBy values →
    HH.slot' CS.cpDim unit
      (DPC.picker
        { title: "Choose dimension"
        , label: DPC.labelNode showJCursorTip
        , render: DPC.renderNode showJCursorTip
        , values
        , isSelectable: DPC.isLeafPath
        })
      unit
      (Just ∘ H.action ∘ Q.HandleGroupByPicker)
  PS.SelectTransform slot selection options →
    HH.slot' CS.cpTransform unit AS.component
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
      (Just ∘ H.action ∘ Q.HandleTransformPicker slot)
  PS.SelectFormatting slot options →
    HH.slot'
      CS.cpFormatting
      unit
      (Display.component (uniqueCardId <> "-" <> mkDimensionId slot))
      options
      (Just ∘ H.action ∘ Q.HandleFormatting slot)

mkDimensionId ∷ Q.ForDimension → String
mkDimensionId = case _ of
  Q.ForGroupBy i → "groupby-" <> show i
  Q.ForColumn i → "col-" <> show i

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
                , HE.onClick (HE.input_ Q.AddGroupBy)
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
        , HC.style dimensionStyles
        ]
        [ I.dimensionButton
            { configurable: true
            , formattable: false
            , dimension
            , showLabel: absurd
            , showDefaultLabel: showPrettyJCursor
            , showValue: showPrettyJCursor
            , onLabelChange: HE.input (Q.ChangeLabel (Q.ForGroupBy slot))
            , onDismiss: HE.input_ (Q.Remove (Q.ForGroupBy slot))
            , onConfigure: HE.input_ (Q.Configure (Q.ForGroupBy slot))
            , onSetupFormatting: const Nothing
            , onMouseDown: HE.input (Q.OrderStart (Q.ForGroupBy slot))
            , onClick: const Nothing
            , onLabelClick: const Nothing
            , disabled: false
            , dismissable: true
            , labelless: false
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

    dimensionStyles = do
      case orderingDimension of
        Just opts | opts.source == slot → C.top (C.px opts.offset)
        _ → pure unit

    dimensionEvents =
      if isJust orderingDimension
        then
          [ HE.onMouseOver (HE.input_ (Q.OrderOver (Q.ForGroupBy slot)))
          , HE.onMouseOut (HE.input_ (Q.OrderOut (Q.ForGroupBy slot)))
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
                    , HE.onClick (HE.input_ Q.AddColumn)
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
        , HC.style columnStyles
        ]
        [ I.dimensionButton
            { configurable: true
            , formattable: true
            , dimension
            , showLabel: absurd
            , showDefaultLabel: showColumn showPrettyJCursor
            , showValue: showColumn showPrettyJCursor
            , onLabelChange: HE.input (Q.ChangeLabel (Q.ForColumn slot))
            , onDismiss: HE.input_ (Q.Remove (Q.ForColumn slot))
            , onConfigure: HE.input_ (Q.Configure (Q.ForColumn slot))
            , onSetupFormatting: HE.input_ (Q.SetupFormatting (Q.ForColumn slot))
            , onMouseDown: HE.input (Q.OrderStart (Q.ForColumn slot))
            , onClick: const Nothing
            , onLabelClick: const Nothing
            , disabled: false
            , dismissable: true
            , labelless: false
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

    columnStyles =
      case orderingColumn of
        Just opts | opts.source == slot → C.left (C.px opts.offset)
        _ → pure unit

    columnEvents =
      guard (isJust orderingColumn) *>
        [ HE.onMouseOver (HE.input_ (Q.OrderOver (Q.ForColumn slot)))
        , HE.onMouseOut (HE.input_ (Q.OrderOut (Q.ForColumn slot)))
        ]
