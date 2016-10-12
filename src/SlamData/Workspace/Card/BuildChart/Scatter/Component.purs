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

module SlamData.Workspace.Card.BuildChart.Scatter.Component
  ( scatterBuilderComponent
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
  ( newSelect
  , setPreviousValueFrom
  , autoSelect
  , (⊝)
  , _value
  , fromSelected
  )
import SlamData.Workspace.LevelOfDetails (LevelOfDetails(..))
import SlamData.Workspace.Card.Component as CC
import SlamData.Workspace.Card.Common.Render (renderLowLOD)
import SlamData.Workspace.Card.CardType as CT
import SlamData.Workspace.Card.CardType.ChartType as CHT
import SlamData.Workspace.Card.BuildChart.Aggregation (aggregationSelectWithNone)

import SlamData.Workspace.Card.BuildChart.CSS as CSS
import SlamData.Workspace.Card.BuildChart.DimensionPicker.Component as DPC
import SlamData.Workspace.Card.BuildChart.DimensionPicker.JCursor (groupJCursors, flattenJCursors)
import SlamData.Workspace.Card.BuildChart.Inputs as BCI
import SlamData.Workspace.Card.BuildChart.Scatter.Component.ChildSlot as CS
import SlamData.Workspace.Card.BuildChart.Scatter.Component.State as ST
import SlamData.Workspace.Card.BuildChart.Scatter.Component.Query as Q
import SlamData.Workspace.Card.BuildChart.Scatter.Model as M

type DSL =
  H.ParentDSL ST.State CS.ChildState Q.QueryC CS.ChildQuery Slam CS.ChildSlot

type HTML =
  H.ParentHTML CS.ChildState Q.QueryC CS.ChildQuery Slam CS.ChildSlot

scatterBuilderComponent ∷ H.Component CC.CardStateP CC.CardQueryP Slam
scatterBuilderComponent = CC.makeCardComponent
  { cardType: CT.ChartOptions CHT.Scatter
  , component: H.parentComponent { render, eval, peek: Just (peek ∘ H.runChildF) }
  , initialState: H.parentState ST.initialState
  , _State: CC._BuildScatterState
  , _Query: CC.makeQueryPrism' CC._BuildScatterQuery
  }

render ∷ ST.State → HTML
render state =
  HH.div_
    [ renderHighLOD state
    , renderLowLOD (CT.darkCardGlyph $ CT.ChartOptions CHT.Scatter) left state.levelOfDetails
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
    , renderSize state
    , HH.hr_
    , renderSeries state
    , HH.hr_
    , row [ renderMinSize state, renderMaxSize state ]
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
              Q.Size _     → "Choose size"
              Q.Series _   → "Choose series"
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
    [ HP.classes [ CSS.withAggregation, CSS.chartConfigureForm ]
    , Cp.nonSubmit
    ]
    [ HH.label [ HP.classes [ B.controlLabel ] ] [ HH.text "X-Axis" ]
    , BCI.pickerWithSelect
        (BCI.primary (Just "X-Axis") (selecting Q.Abscissa))
        state.abscissa
        (BCI.aggregation (Just "X-Axis Aggregation") (selecting Q.AbscissaAgg))
        state.abscissaAgg
    ]

renderOrdinate ∷ ST.State → HTML
renderOrdinate state =
  HH.form
    [ HP.classes [ CSS.withAggregation, CSS.chartConfigureForm ]
    , Cp.nonSubmit
    ]
    [ HH.label [ HP.classes [ B.controlLabel ] ] [ HH.text "Y-Axis" ]
    , BCI.pickerWithSelect
        (BCI.primary (Just "Y-Axis") (selecting Q.Ordinate))
        state.ordinate
        (BCI.aggregation (Just "Y-Axis Aggregation") (selecting Q.OrdinateAgg))
        state.ordinateAgg
    ]

renderSize ∷ ST.State → HTML
renderSize state =
  HH.form
    [ HP.classes [ CSS.withAggregation, CSS.chartConfigureForm ]
    , Cp.nonSubmit
    ]
    [ HH.label [ HP.classes [ B.controlLabel ] ] [ HH.text "Bubble size" ]
    , BCI.pickerWithSelect
        (BCI.primary (Just "Bubble size") (selecting Q.Size))
        state.size
        (BCI.aggregation (Just "Bubble size Aggregation") (selecting Q.SizeAgg))
        state.sizeAgg
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
eval = cardEval ⨁ scatterBuilderEval

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
        , abscissaAggregation: _
        , ordinate: _
        , ordinateAggregation: _
        , size: st.size ^. _value
        , sizeAggregation: st.sizeAgg ^. _value
        , series: st.series ^. _value
        , minSize: (st.minSize ∷ Number)
        , maxSize: (st.maxSize ∷ Number)
        }
        <$> (st.abscissa ^. _value)
        <*> (st.abscissaAgg ^. _value)
        <*> (st.ordinate ^. _value)
        <*> (st.ordinateAgg ^. _value)
    pure $ k $ Card.BuildScatter model
  CC.Load (Card.BuildScatter (Just model)) next → do
    loadModel model
    H.modify _{ maxSize = model.maxSize
              , minSize = model.minSize
              }
    pure next
  CC.Load card next →
    pure next
  CC.SetDimensions dims next → do
    H.modify
      _ { levelOfDetails =
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

scatterBuilderEval ∷ Q.Query ~> DSL
scatterBuilderEval = case _ of
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
      Q.Abscissa a    → updatePicker ST._abscissa Q.Abscissa a
      Q.AbscissaAgg a → updateSelect ST._abscissaAgg a
      Q.Ordinate a    → updatePicker ST._ordinate Q.Ordinate a
      Q.OrdinateAgg a → updateSelect ST._ordinateAgg a
      Q.Size a        → updatePicker ST._size Q.Size a
      Q.SizeAgg a     → updateSelect ST._sizeAgg a
      Q.Series a      → updatePicker ST._series Q.Series a
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
        Q.Abscissa _ → H.modify (ST._abscissa ∘ _value ?~ value')
        Q.Ordinate _ → H.modify (ST._ordinate ∘ _value ?~ value')
        Q.Size _     → H.modify (ST._size ∘ _value ?~ value')
        Q.Series _   → H.modify (ST._series ∘ _value ?~ value')
        _ → pure unit
      H.modify _ { picker = Nothing }
      raiseUpdate

synchronizeChildren ∷ DSL Unit
synchronizeChildren = do
  st ← H.get
  let
    newAbscissa =
      setPreviousValueFrom (Just st.abscissa)
        $ autoSelect
        $ newSelect
        $ st.axes.value

    newAbscissaAggregation =
      setPreviousValueFrom (Just st.abscissaAgg)
        $ aggregationSelectWithNone

    newOrdinate =
      setPreviousValueFrom (Just st.ordinate)
        $ autoSelect
        $ newSelect
        $ st.axes.value
        ⊝ newAbscissa

    newOrdinateAggregation =
      setPreviousValueFrom (Just st.ordinateAgg)
        $ aggregationSelectWithNone

    newSize =
      setPreviousValueFrom (Just st.size)
        $ newSelect
        $ st.axes.value
        ⊝ newAbscissa
        ⊝ newOrdinate

    newSizeAggregation =
      setPreviousValueFrom (Just st.sizeAgg)
        $ aggregationSelectWithNone

    newSeries =
      setPreviousValueFrom (Just st.series)
        $ autoSelect
        $ newSelect
        $ st.axes.category

  H.modify _
    { abscissa = newAbscissa
    , abscissaAgg = newAbscissaAggregation
    , ordinate = newOrdinate
    , ordinateAgg = newOrdinateAggregation
    , size = newSize
    , sizeAgg = newSizeAggregation
    , series = newSeries
    }

loadModel ∷ M.ScatterR → DSL Unit
loadModel r =
  H.modify _
    { abscissa = fromSelected (Just r.abscissa)
    , abscissaAgg = fromSelected (Just r.abscissaAggregation)
    , ordinate = fromSelected (Just r.ordinate)
    , ordinateAgg = fromSelected (Just r.ordinateAggregation)
    , size = fromSelected r.size
    , sizeAgg = fromSelected r.sizeAggregation
    , series = fromSelected r.series
    }
