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

module SlamData.Workspace.Card.BuildChart.Line.Component
  ( lineBuilderComponent
  ) where

import SlamData.Prelude

import Data.Lens ((^?), (^.), (.~), (?~))
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
  ( newSelect
  , setPreviousValueFrom
  , autoSelect
  , ifSelected
  , (⊝)
  , _value
  , fromSelected
  )
import SlamData.Workspace.LevelOfDetails (LevelOfDetails(..))
import SlamData.Workspace.Card.Component as CC
import SlamData.Workspace.Card.Common.Render (renderLowLOD)
import SlamData.Workspace.Card.CardType as CT
import SlamData.Workspace.Card.CardType.ChartType as CHT
import SlamData.Workspace.Card.BuildChart.Aggregation (nonMaybeAggregationSelect)

import SlamData.Workspace.Card.BuildChart.CSS as CSS
import SlamData.Workspace.Card.BuildChart.DimensionPicker.Component as DPC
import SlamData.Workspace.Card.BuildChart.DimensionPicker.JCursor (groupJCursors, flattenJCursors)
import SlamData.Workspace.Card.BuildChart.Inputs as BCI
import SlamData.Workspace.Card.BuildChart.Line.Component.ChildSlot as CS
import SlamData.Workspace.Card.BuildChart.Line.Component.State as ST
import SlamData.Workspace.Card.BuildChart.Line.Component.Query as Q
import SlamData.Workspace.Card.BuildChart.Line.Model as M

type DSL =
  H.ParentDSL ST.State CS.ChildState Q.QueryC CS.ChildQuery Slam CS.ChildSlot

type HTML =
  H.ParentHTML CS.ChildState Q.QueryC CS.ChildQuery Slam CS.ChildSlot

lineBuilderComponent ∷ H.Component CC.CardStateP CC.CardQueryP Slam
lineBuilderComponent = CC.makeCardComponent
  { cardType: CT.ChartOptions CHT.Line
  , component: H.parentComponent { render, eval, peek: Just (peek ∘ H.runChildF) }
  , initialState: H.parentState ST.initialState
  , _State: CC._BuildLineState
  , _Query: CC.makeQueryPrism' CC._BuildLineQuery
  }

render ∷ ST.State → HTML
render state =
  HH.div_
    [ renderHighLOD state
    , renderLowLOD (CT.darkCardGlyph $ CT.ChartOptions CHT.Line) left state.levelOfDetails
    ]

renderHighLOD ∷ ST.State → HTML
renderHighLOD state =
  HH.div
    [ HP.classes
        $ [ CSS.chartEditor ]
        ⊕ (guard (state.levelOfDetails ≠ High) $> B.hidden)
    ]
    [ renderDimension state
    , HH.hr_
    , renderValue state
    , renderSecondValue state
    , HH.hr_
    , renderSeries state
    , HH.hr_
    , renderSize state
    , row [ renderMinSize state, renderMaxSize state ]
    , HH.hr_
    , row [ renderAxisLabelAngle state ]
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
              Q.Dimension _   → "Choose dimension"
              Q.Value _       → "Choose measure #1"
              Q.SecondValue _ → "Choose measure #2"
              Q.Size _        → "Choose measure #3"
              Q.Series _      → "Choose series"
              _ → ""
          , label: DPC.labelNode show
          , render: DPC.renderNode show
          , values: groupJCursors (List.fromFoldable options)
          , isSelectable: DPC.isLeafPath
          }
      , initialState: H.parentState DPC.initialState
      }

renderDimension ∷ ST.State → HTML
renderDimension state =
  HH.form
    [ HP.classes [ CSS.chartConfigureForm ]
    , Cp.nonSubmit
    ]
    [ HH.label [ HP.classes [ B.controlLabel ] ] [ HH.text "Dimension" ]
    , BCI.pickerInput
        (BCI.primary (Just "Dimension") (selecting Q.Dimension))
        state.dimension
    ]

renderValue ∷ ST.State → HTML
renderValue state =
  HH.form
    [ HP.classes [ CSS.withAggregation, CSS.chartConfigureForm ]
    , Cp.nonSubmit
    ]
    [ HH.label [ HP.classes [ B.controlLabel ] ] [ HH.text "Measure #1" ]
    , BCI.pickerWithSelect
        (BCI.secondary (Just "Measure #1") (selecting Q.Value))
        state.value
        (BCI.aggregation (Just "Measure Aggregation #1") (selecting Q.ValueAgg))
        state.valueAgg
    ]

renderSecondValue ∷ ST.State → HTML
renderSecondValue state =
  HH.form
    [ HP.classes [ CSS.withAggregation, CSS.chartConfigureForm ]
    , Cp.nonSubmit
    ]
    [ HH.label [ HP.classes [ B.controlLabel ] ] [ HH.text "Measure #2" ]
    , BCI.pickerWithSelect
        (BCI.secondary (Just "Measure #2") (selecting Q.SecondValue))
        state.secondValue
        (BCI.aggregation (Just "Measure Aggregation #2") (selecting Q.SecondValueAgg))
        state.secondValueAgg
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

renderSize ∷ ST.State → HTML
renderSize state =
  HH.form
    [ HP.classes [ CSS.withAggregation, CSS.chartConfigureForm ]
    , Cp.nonSubmit
    ]
    [ HH.label [ HP.classes [ B.controlLabel ] ] [ HH.text "Measure #3" ]
    , BCI.pickerWithSelect
        (BCI.secondary (Just "Measure #3") (selecting Q.Size))
        state.size
        (BCI.aggregation (Just "Measure Aggregation #3") (selecting Q.SizeAgg))
        state.sizeAgg
    ]

renderAxisLabelAngle ∷ ST.State → HTML
renderAxisLabelAngle state =
  HH.form
    [ HP.classes [ B.colXs6, CSS.axisLabelParam ]
    , Cp.nonSubmit
    ]
    [ HH.label [ HP.classes [ B.controlLabel ] ] [ HH.text "Label angle" ]
    , HH.input
        [ HP.classes [ B.formControl ]
        , HP.value $ show $ state.axisLabelAngle
        , ARIA.label "Axis label angle"
        , HE.onValueChange $ HE.input (\s → right ∘ Q.SetAxisLabelAngle s)
        ]
    ]

renderMinSize ∷ ST.State → HTML
renderMinSize state =
  HH.form
    [ HP.classes [ B.colXs6, CSS.axisLabelParam ]
    , Cp.nonSubmit
    ]
    [ HH.label [ HP.classes [ B.controlLabel ] ] [ HH.text "Min size" ]
    , HH.input
        [ HP.classes [ B.formControl ]
        , HP.value $ show $ state.minSize
        , ARIA.label "Min size"
        , HE.onValueChange $ HE.input (\s → right ∘ Q.SetMinSymbolSize s)
        ]
    ]

renderMaxSize ∷ ST.State → HTML
renderMaxSize state =
  HH.form
    [ HP.classes [ B.colXs6, CSS.axisLabelParam ]
    , Cp.nonSubmit
    ]
    [ HH.label [ HP.classes [ B.controlLabel ] ] [ HH.text "Max size" ]
    , HH.input
        [ HP.classes [ B.formControl ]
        , HP.value $ show $ state.maxSize
        , ARIA.label "Max size"
        , HE.onValueChange $ HE.input (\s → right ∘ Q.SetMaxSymbolSize s)
        ]
    ]


eval ∷ Q.QueryC ~> DSL
eval = cardEval ⨁ lineBuilderEval

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
        { dimension: _
        , value: _
        , valueAggregation: _
        , secondValue: st.secondValue ^. _value
        , secondValueAggregation: st.secondValueAgg ^. _value
        , size: st.size ^. _value
        , sizeAggregation: st.sizeAgg ^. _value
        , series: st.series ^. _value
        , maxSize: st.maxSize
        , minSize: st.minSize
        , axisLabelAngle: st.axisLabelAngle
        }
        <$> (st.dimension ^. _value)
        <*> (st.value ^. _value)
        <*> (st.valueAgg ^. _value)
    pure $ k $ Card.BuildLine model
  CC.Load (Card.BuildLine (Just model)) next → do
    loadModel model
    H.modify _
      { maxSize = model.maxSize
      , minSize = model.minSize
      , axisLabelAngle = model.axisLabelAngle
      }
    pure next
  CC.Load card next →
    pure next
  CC.SetDimensions dims next → do
    H.modify _
      { levelOfDetails =
          if dims.width < 576.0 ∨ dims.height < 416.0
            then Low
            else High
      }
    pure next
  CC.ModelUpdated _ next →
    pure next
  CC.ZoomIn next →
    pure next

raiseUpdate ∷ DSL Unit
raiseUpdate = synchronizeChildren *> CC.raiseUpdatedP' CC.EvalModelUpdate

lineBuilderEval ∷ Q.Query ~> DSL
lineBuilderEval = case _ of
  Q.SetAxisLabelAngle str next → do
    let fl = readFloat str
    unless (isNaN fl) do
      H.modify _{axisLabelAngle = fl}
      CC.raiseUpdatedP' CC.EvalModelUpdate
    pure next
  Q.SetMinSymbolSize str next → do
    let fl = readFloat str
    unless (isNaN fl) do
      H.modify _{minSize = fl}
      CC.raiseUpdatedP' CC.EvalModelUpdate
    pure next
  Q.SetMaxSymbolSize str next → do
    let fl = readFloat str
    unless (isNaN fl) do
      H.modify _{maxSize = fl}
      CC.raiseUpdatedP' CC.EvalModelUpdate
    pure next
  Q.Select sel next → do
    case sel of
      Q.Dimension a      → updatePicker ST._dimension Q.Dimension a
      Q.Value a          → updatePicker ST._value Q.Value a
      Q.ValueAgg a       → updateSelect ST._valueAgg a
      Q.SecondValue a    → updatePicker ST._secondValue Q.SecondValue a
      Q.SecondValueAgg a → updateSelect ST._secondValueAgg a
      Q.Size a           → updatePicker ST._size Q.Size a
      Q.SizeAgg a        → updateSelect ST._sizeAgg a
      Q.Series a         → updatePicker ST._series Q.Series a
    pure next
  where
  updatePicker l q = case _ of
    BCI.Open opts → H.modify (ST.showPicker q opts)
    BCI.Choose a  → H.modify (l ∘ _value .~ a) *> raiseUpdate

  updateSelect l = case _ of
    BCI.Open _    → pure unit
    BCI.Choose a  → H.modify (l ∘ _value .~ a) *> raiseUpdate

peek ∷ ∀ a. CS.ChildQuery a → DSL Unit
peek = coproduct peekPicker (const (pure unit))
  where
  peekPicker = case _ of
    DPC.Dismiss _ →
      H.modify _ { picker = Nothing }
    DPC.Confirm value _ → do
      st ← H.get
      let
        value' = flattenJCursors value
      for_ st.picker \{ select } → case select of
        Q.Dimension _   → H.modify (ST._dimension ∘ _value ?~ value')
        Q.Value _       → H.modify (ST._value ∘ _value ?~ value')
        Q.SecondValue _ → H.modify (ST._secondValue ∘ _value ?~ value')
        Q.Size _        → H.modify (ST._size ∘ _value ?~ value')
        Q.Series _      → H.modify (ST._series ∘ _value ?~ value')
        _ → pure unit
      H.modify _ { picker = Nothing }
      raiseUpdate

synchronizeChildren ∷ DSL Unit
synchronizeChildren = do
  st ← H.get
  let
    newDimension =
      setPreviousValueFrom (Just st.dimension)
        $ autoSelect
        $ newSelect
        $ st.axes.category
        ⊕ st.axes.time
        ⊕ st.axes.date
        ⊕ st.axes.datetime
        ⊕ st.axes.value

    newValue =
      setPreviousValueFrom (Just st.value)
        $ autoSelect
        $ newSelect
        $ st.axes.value

    newValueAggregation =
      setPreviousValueFrom (Just st.valueAgg)
        $ nonMaybeAggregationSelect

    newSecondValue =
      setPreviousValueFrom (Just st.secondValue)
        $ newSelect
        $ ifSelected [ newValue ]
        $ st.axes.value
        ⊝ newValue

    newSecondValueAggregation =
      setPreviousValueFrom (Just st.secondValueAgg)
        $ nonMaybeAggregationSelect

    newSize =
      setPreviousValueFrom (Just st.size)
        $ newSelect
        $ ifSelected [ newValue ]
        $ st.axes.value
        ⊝ newValue
        ⊝ newSecondValue

    newSizeAggregation =
      setPreviousValueFrom (Just st.sizeAgg)
        $ nonMaybeAggregationSelect

    newSeries =
      setPreviousValueFrom (Just st.series)
        $ newSelect
        $ ifSelected [ newDimension ]
        $ st.axes.category
        ⊝ newDimension

  H.modify _
    { dimension = newDimension
    , value = newValue
    , valueAgg = newValueAggregation
    , secondValue = newSecondValue
    , secondValueAgg = newSecondValueAggregation
    , size = newSize
    , sizeAgg = newSizeAggregation
    , series = newSeries
    }

loadModel ∷ M.LineR → DSL Unit
loadModel r =
  H.modify _
    { value = fromSelected (Just r.value)
    , valueAgg = fromSelected (Just r.valueAggregation)
    , dimension = fromSelected (Just r.dimension)
    , secondValue = fromSelected r.secondValue
    , secondValueAgg = fromSelected r.secondValueAggregation
    , series = fromSelected r.series
    , size = fromSelected r.size
    , sizeAgg = fromSelected r.sizeAggregation
    }
