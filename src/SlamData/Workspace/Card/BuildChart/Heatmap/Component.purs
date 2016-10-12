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

module SlamData.Workspace.Card.BuildChart.Heatmap.Component
  ( heatmapBuilderComponent
  ) where

import SlamData.Prelude

import Data.Lens ((^?), (^.), (?~), (.~))
import Data.Lens as Lens
import Data.List as List

import Global (readFloat, isNaN)

import Halogen as H
import Halogen.HTML.Indexed as HH
import Halogen.CustomProps as Cp
import Halogen.HTML.Events.Indexed as HE
import Halogen.HTML.Properties.Indexed as HP
import Halogen.HTML.Properties.Indexed.ARIA as ARIA
import Halogen.Themes.Bootstrap3 as B

import SlamData.Monad (Slam)
import SlamData.Workspace.Card.Model as Card
import SlamData.Workspace.Card.Port as Port
import SlamData.Render.Common (row)
import SlamData.Form.Select
  ( Select
  , newSelect
  , setPreviousValueFrom
  , autoSelect
  , ifSelected
  , (⊝)
  , _value
  , fromSelected
  )
import SlamData.Workspace.LevelOfDetails (LevelOfDetails(..))
import SlamData.Workspace.Card.BuildChart.ColorScheme (colorSchemeSelect)
import SlamData.Workspace.Card.Component as CC
import SlamData.Workspace.Card.Common.Render (renderLowLOD)
import SlamData.Workspace.Card.CardType as CT
import SlamData.Workspace.Card.CardType.ChartType as CHT
import SlamData.Workspace.Card.BuildChart.Aggregation (nonMaybeAggregationSelect)

import SlamData.Workspace.Card.BuildChart.CSS as CSS
import SlamData.Workspace.Card.BuildChart.DimensionPicker.Component as DPC
import SlamData.Workspace.Card.BuildChart.DimensionPicker.JCursor (JCursorNode, groupJCursors, flattenJCursors)
import SlamData.Workspace.Card.BuildChart.Inputs as BCI
import SlamData.Workspace.Card.BuildChart.Heatmap.Component.ChildSlot as CS
import SlamData.Workspace.Card.BuildChart.Heatmap.Component.State as ST
import SlamData.Workspace.Card.BuildChart.Heatmap.Component.Query as Q
import SlamData.Workspace.Card.BuildChart.Heatmap.Model as M

type DSL =
  H.ParentDSL ST.State CS.ChildState Q.QueryC CS.ChildQuery Slam CS.ChildSlot

type HTML =
  H.ParentHTML CS.ChildState Q.QueryC CS.ChildQuery Slam CS.ChildSlot

heatmapBuilderComponent ∷ H.Component CC.CardStateP CC.CardQueryP Slam
heatmapBuilderComponent = CC.makeCardComponent
  { cardType: CT.ChartOptions CHT.Heatmap
  , component: H.parentComponent { render, eval, peek: Just (peek ∘ H.runChildF) }
  , initialState: H.parentState ST.initialState
  , _State: CC._BuildHeatmapState
  , _Query: CC.makeQueryPrism' CC._BuildHeatmapQuery
  }

render ∷ ST.State → HTML
render state =
  HH.div_
    [ renderHighLOD state
    , renderLowLOD (CT.darkCardGlyph $ CT.ChartOptions CHT.Heatmap) left state.levelOfDetails
    ]

renderHighLOD ∷ ST.State → HTML
renderHighLOD state =
  HH.div
    [ HP.classes
        $ [ CSS.chartEditor ]
        ⊕ (guard (state.levelOfDetails ≠ High) $> B.hidden)
    ]
    [ renderAbscissa state
    , renderOrdinate state
    , renderValue state
    , HH.hr_
    , renderSeries state
    , HH.hr_
    , row [ renderColorScheme state, renderIsReversedScheme state ]
    , HH.hr_
    , row [ renderMinVal state, renderMaxVal state ]
    , renderPicker state
    ]

selecting ∷ ∀ a. (a → Q.Selection BCI.SelectAction) → a → H.Action Q.QueryC
selecting f q _ = right (Q.Select (f q) unit)

renderPicker ∷ ST.State → HTML
renderPicker state = case state.picker of
  Nothing → HH.text ""
  Just { options, select } →
    HH.slot unit \_ →
      { component: DPC.picker
          { title: case select of
               Q.Abscissa _ → "Choose x-axis"
               Q.Ordinate _ → "Choose y-axis"
               Q.Value _ → "Choose measure"
               Q.Series _ → "Choose series"
               _ → ""
          , label: DPC.labelNode show
          , render: DPC.renderNode show
          , values: groupJCursors (List.fromFoldable options)
          , isSelectable: DPC.isLeafPath
          }
      , initialState: H.parentState DPC.initialState
      }

renderAbscissa ∷ ST.State → HTML
renderAbscissa state =
  HH.form
    [ HP.classes [ CSS.chartConfigureForm ]
    , Cp.nonSubmit
    ]
    [ HH.label [ HP.classes [ B.controlLabel ] ] [ HH.text "X-Axis" ]
    , BCI.pickerInput
        (BCI.primary (Just "X-Axis") (selecting Q.Abscissa))
        state.abscissa
    ]

renderOrdinate ∷ ST.State → HTML
renderOrdinate state =
  HH.form
    [ HP.classes [ CSS.chartConfigureForm ]
    , Cp.nonSubmit
    ]
    [ HH.label [ HP.classes [ B.controlLabel ] ] [ HH.text "Y-Axis" ]
    , BCI.pickerInput
        (BCI.primary (Just "Y-Axis") (selecting Q.Ordinate))
        state.ordinate
    ]

renderValue ∷ ST.State → HTML
renderValue state =
  HH.form
    [ HP.classes [ CSS.withAggregation, CSS.chartConfigureForm ]
    , Cp.nonSubmit
    ]
    [ HH.label [ HP.classes [ B.controlLabel ] ] [ HH.text "Measure" ]
    , BCI.pickerWithSelect
        (BCI.primary (Just "Measure") (selecting Q.Value))
        state.value
        (BCI.dropdown (Just "Measure aggregation") (selecting Q.ValueAgg))
        state.valueAgg
    ]

renderSeries ∷ ST.State → HTML
renderSeries state =
  HH.form
    [ HP.classes [ CSS.chartConfigureForm ]
    , Cp.nonSubmit
    ]
    [ HH.label [ HP.classes [ B.controlLabel ] ] [ HH.text "Series" ]
    , BCI.pickerInput
        (BCI.secondary (Just "Series") (selecting Q.Series))
        state.series
    ]

renderMinVal ∷ ST.State → HTML
renderMinVal state =
  HH.form
    [ HP.classes [ B.colXs6, CSS.axisLabelParam ]
    , Cp.nonSubmit
    ]
    [ HH.label [ HP.classes [ B.controlLabel ] ] [ HH.text "Min color rendering value" ]
    , HH.input
        [ HP.classes [ B.formControl ]
        , HP.value $ show $ state.minValue
        , ARIA.label "Min color rendering value"
        , HE.onValueChange $ HE.input (\s → right ∘ Q.SetMinValue s)
        ]
    ]

renderMaxVal ∷ ST.State → HTML
renderMaxVal state =
  HH.form
    [ HP.classes [ B.colXs6, CSS.axisLabelParam ]
    , Cp.nonSubmit
    ]
    [ HH.label [ HP.classes [ B.controlLabel ] ] [ HH.text "Max color rendering value" ]
    , HH.input
        [ HP.classes [ B.formControl ]
        , HP.value $ show $ state.maxValue
        , ARIA.label "Max color rendering value"
        , HE.onValueChange $ HE.input (\s → right ∘ Q.SetMaxValue s)
        ]
    ]


renderIsReversedScheme ∷ ST.State → HTML
renderIsReversedScheme state =
  HH.form
    [ HP.classes [ B.colXs6, CSS.chartDetailParam ]
    , Cp.nonSubmit
    ]
    [ HH.label [ HP.classes [ B.controlLabel ] ] [ HH.text "Reverse color scheme" ]
    , HH.input
        [ HP.inputType HP.InputCheckbox
        , HP.checked state.isSchemeReversed
        , ARIA.label "Reverse color scheme"
        , HE.onChecked $ HE.input_ (right ∘ Q.ToggleReversedScheme)
        ]
    ]


renderColorScheme ∷ ST.State → HTML
renderColorScheme state =
  HH.form
    [ HP.classes [ B.colXs6, CSS.axisLabelParam ]
    , Cp.nonSubmit
    ]
    [ HH.label [ HP.classes [ B.controlLabel ] ] [ HH.text "Color scheme" ]
    , BCI.selectInput
        (BCI.dropdown (Just "Color scheme") (selecting Q.ColorScheme))
        state.colorScheme
    ]


eval ∷ Q.QueryC ~> DSL
eval = cardEval ⨁ heatmapBuilderEval

cardEval ∷ CC.CardEvalQuery ~> DSL
cardEval = case _ of
  CC.EvalCard info output next → do
    for_ (output ^? Lens._Just ∘ Port._ChartAxes) \axes → do
      H.modify _{axes = axes}
      synchronizeChildren
    pure next
  CC.Activate next →
    pure next
  CC.Deactivate next →
    pure next
  CC.Save k → do
    st ← H.get
    let
      model =
        { abscissa: _
        , ordinate: _
        , value: _
        , valueAggregation: _
        , series: (st.series ^. _value)
        , colorScheme: _
        , isColorSchemeReversed: st.isSchemeReversed
        , minValue: st.minValue
        , maxValue: st.maxValue
        }
        <$> (st.abscissa ^. _value)
        <*> (st.ordinate ^. _value)
        <*> (st.value ^. _value)
        <*> (st.valueAgg ^. _value)
        <*> (st.colorScheme ^. _value)
    pure $ k $ Card.BuildHeatmap model
  CC.Load (Card.BuildHeatmap (Just model)) next → do
    loadModel model
    H.modify _{ minValue = model.minValue
              , maxValue = model.maxValue
              }
    pure next
  CC.Load card next →
    pure next
  CC.SetDimensions dims next → do
    H.modify
      _{levelOfDetails =
           if dims.width < 516.0 ∨ dims.height < 416.0
             then Low
             else High
       }
    pure next
  CC.ModelUpdated _ next →
    pure next
  CC.ZoomIn next →
    pure next

heatmapBuilderEval ∷ Q.Query ~> DSL
heatmapBuilderEval = case _ of
  Q.SetMinValue str next → do
    let fl = readFloat str
    unless (isNaN fl) do
      H.modify _{minValue = fl}
      CC.raiseUpdatedP' CC.EvalModelUpdate
    pure next
  Q.SetMaxValue str next → do
    let fl = readFloat str
    unless (isNaN fl) do
      H.modify _{maxValue = fl}
      CC.raiseUpdatedP' CC.EvalModelUpdate
    pure next
  Q.ToggleReversedScheme next → do
    H.modify \x → x{isSchemeReversed = not x.isSchemeReversed}
    CC.raiseUpdatedP' CC.EvalModelUpdate
    pure next
  Q.Select sel next → next <$ case sel of
    Q.Abscissa a    → updatePicker ST._abscissa Q.Abscissa a
    Q.Ordinate a    → updatePicker ST._ordinate Q.Ordinate a
    Q.Value a       → updatePicker ST._value Q.Value a
    Q.ValueAgg a    → updateSelect ST._valueAgg a
    Q.Series a      → updatePicker ST._series Q.Series a
    Q.ColorScheme a → updateSelect ST._colorScheme a
  where
  updatePicker l q = case _ of
    BCI.Open opts → H.modify (ST.showPicker q opts)
    BCI.Choose a  → H.modify (l ∘ _value .~ a) *> raiseUpdate

  updateSelect ∷ ∀ a. Lens.LensP ST.State (Select a) → BCI.SelectAction a → DSL Unit
  updateSelect l = case _ of
    BCI.Open _   → pure unit
    BCI.Choose a → H.modify (l ∘ _value .~ a) *> raiseUpdate

raiseUpdate ∷ DSL Unit
raiseUpdate = synchronizeChildren *> CC.raiseUpdatedP' CC.EvalModelUpdate


peek ∷ ∀ a. CS.ChildQuery a → DSL Unit
peek = peekPeeker ⨁ (const $ pure unit)

peekPeeker ∷ ∀ a. DPC.Query JCursorNode a → DSL Unit
peekPeeker = case _ of
  DPC.Dismiss _ →
    H.modify _{ picker = Nothing }
  DPC.Confirm value _ → do
    st ← H.get
    let
      value' = flattenJCursors value
    for_ st.picker \v → case v.select of
      Q.Abscissa _ → H.modify $ ST._abscissa ∘ _value ?~ value'
      Q.Ordinate _ → H.modify $ ST._ordinate ∘ _value ?~ value'
      Q.Value _    → H.modify $ ST._value ∘ _value ?~ value'
      Q.Series _   → H.modify $ ST._series ∘ _value ?~ value'
      _ → pure unit
    H.modify _{ picker = Nothing }
    raiseUpdate

synchronizeChildren ∷ DSL Unit
synchronizeChildren = void do
  st ← H.get
  let
    newAbscissa =
      setPreviousValueFrom (Just st.abscissa)
        $ autoSelect
        $ newSelect
        $ st.axes.category
        ⊕ st.axes.value
        ⊕ st.axes.time
        ⊕ st.axes.date
        ⊕ st.axes.datetime

    newOrdinate =
      setPreviousValueFrom (Just st.ordinate)
        $ autoSelect
        $ newSelect
        $ st.axes.category
        ⊕ st.axes.value
        ⊕ st.axes.time
        ⊕ st.axes.date
        ⊕ st.axes.datetime
        ⊝ newAbscissa

    newValue =
      setPreviousValueFrom (Just st.value)
        $ autoSelect
        $ newSelect
        $ st.axes.value
        ⊝ newAbscissa
        ⊝ newOrdinate

    newValueAggregation =
      setPreviousValueFrom (Just st.valueAgg)
        $ nonMaybeAggregationSelect

    newSeries =
      setPreviousValueFrom (Just st.series)
        $ newSelect
        $ ifSelected [newAbscissa, newOrdinate, newValue]
        $ st.axes.category
        ⊕ st.axes.time
        ⊝ newAbscissa
        ⊝ newOrdinate

    newColorScheme =
      setPreviousValueFrom (Just st.colorScheme)
        $ colorSchemeSelect

  H.modify _
    { abscissa = newAbscissa
    , ordinate = newOrdinate
    , value = newValue
    , valueAgg = newValueAggregation
    , series = newSeries
    , colorScheme = newColorScheme
    }

loadModel ∷ M.HeatmapR → DSL Unit
loadModel r =
  H.modify _
    { abscissa = fromSelected (Just r.abscissa)
    , ordinate = fromSelected (Just r.ordinate)
    , value = fromSelected (Just r.value)
    , valueAgg = fromSelected (Just r.valueAggregation)
    , series = fromSelected r.series
    , colorScheme = fromSelected (Just r.colorScheme)
    }
