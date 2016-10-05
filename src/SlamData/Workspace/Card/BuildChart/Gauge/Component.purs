module SlamData.Workspace.Card.BuildChart.Gauge.Component
  ( gaugeBuilderComponent
  ) where

import SlamData.Prelude

import Data.Lens ((^?), (^.), (?~), (.~))
import Data.Lens as Lens
import Data.List as List

import Halogen as H
import Halogen.HTML.Indexed as HH
import Halogen.CustomProps as Cp
import Halogen.HTML.Properties.Indexed as HP
import Halogen.Themes.Bootstrap3 as B

import SlamData.Monad (Slam)
import SlamData.Workspace.Card.Model as Card
import SlamData.Workspace.Card.Port as Port
import SlamData.Form.Select (newSelect, setPreviousValueFrom, autoSelect, ifSelected, (⊝), _value, fromSelected)
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
import SlamData.Workspace.Card.BuildChart.Gauge.Component.ChildSlot as CS
import SlamData.Workspace.Card.BuildChart.Gauge.Component.State as ST
import SlamData.Workspace.Card.BuildChart.Gauge.Component.Query as Q
import SlamData.Workspace.Card.BuildChart.Gauge.Model as M

type DSL =
  H.ParentDSL ST.State CS.ChildState Q.QueryC CS.ChildQuery Slam CS.ChildSlot

type HTML =
  H.ParentHTML CS.ChildState Q.QueryC CS.ChildQuery Slam CS.ChildSlot

gaugeBuilderComponent ∷ H.Component CC.CardStateP CC.CardQueryP Slam
gaugeBuilderComponent = CC.makeCardComponent
  { cardType: CT.ChartOptions CHT.Gauge
  , component: H.parentComponent { render, eval, peek: Just (peek ∘ H.runChildF) }
  , initialState: H.parentState ST.initialState
  , _State: CC._BuildGaugeState
  , _Query: CC.makeQueryPrism' CC._BuildGaugeQuery
  }

render ∷ ST.State → HTML
render state =
  HH.div_
    [ renderHighLOD state
    , renderLowLOD (CT.darkCardGlyph $ CT.ChartOptions CHT.Gauge) left state.levelOfDetails
    ]

renderHighLOD ∷ ST.State → HTML
renderHighLOD state =
  HH.div
    [ HP.classes
        $ [ CSS.chartEditor ]
        ⊕ (guard (state.levelOfDetails ≠ High) $> B.hidden)
    ]
    [ renderValue state
    , renderParallel state
    , renderMultiple state
    , renderPicker state
    ]

selecting ∷ ∀ a . (a → Q.Selection BCI.SelectAction) → a → H.Action Q.QueryC
selecting f q a = right (Q.Select (f q) a)

renderPicker ∷ ST.State → HTML
renderPicker state = case state.picker of
  Nothing → HH.text ""
  Just { options, select } →
    HH.slot unit \_ →
      { component: DPC.picker
          { title: case select of
              Q.Value _    → "Choose measure"
              Q.Multiple _ → "Choose multiple"
              Q.Parallel _ → "Choose parallel"
              _ → ""
          , label: show
          , render: HH.text ∘ show
          , values: groupJCursors (List.fromFoldable options)
          }
      , initialState: H.parentState DPC.initialState
      }

renderValue ∷ ST.State → HTML
renderValue state =
  HH.form
    [ HP.classes [ CSS.withAggregation, CSS.chartConfigureForm ]
    , Cp.nonSubmit
    ]
    [ HH.label [ HP.classes [ B.controlLabel ] ] [ HH.text "Measure" ]
    , BCI.pickerWithSelect
        (BCI.secondary (Just "Measure") (selecting Q.Value))
        state.value
        (BCI.aggregation (Just "Measure Aggregation") (selecting Q.ValueAgg))
        state.valueAgg
    ]

renderParallel ∷ ST.State → HTML
renderParallel state =
  HH.form
    [ HP.classes [ CSS.chartConfigureForm ]
    , Cp.nonSubmit
    ]
    [ HH.label [ HP.classes [ B.controlLabel ] ] [ HH.text "Parallel series" ]
    , BCI.pickerInput
        (BCI.secondary (Just "Parallel series") (selecting Q.Parallel))
        state.parallel
    ]

renderMultiple ∷ ST.State → HTML
renderMultiple state =
  HH.form
    [ HP.classes [ CSS.chartConfigureForm ]
    , Cp.nonSubmit
    ]
    [ HH.label [ HP.classes [ B.controlLabel ] ] [ HH.text "Multiple cursors" ]
    , BCI.pickerInput
        (BCI.secondary (Just "Multiple cursors") (selecting Q.Multiple))
        state.multiple
    ]

eval ∷ Q.QueryC ~> DSL
eval = cardEval ⨁ chartEval

cardEval ∷ CC.CardEvalQuery ~> DSL
cardEval = case _ of
  CC.EvalCard info output next → do
    for_ (info.input ^? Lens._Just ∘ Port._ResourceAxes) \axes → do
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
        { value: _
        , valueAggregation: _
        , parallel: st.parallel ^. _value
        , multiple: st.multiple ^. _value
        }
        <$> (st.value ^. _value)
        <*> (st.valueAgg ^. _value)
    pure $ k $ Card.BuildGauge model
  CC.Load (Card.BuildGauge model) next → do
    for_ model loadModel
    pure next
  CC.Load card next →
    pure next
  CC.SetDimensions dims next → do
    H.modify
      _{levelOfDetails =
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

chartEval ∷ Q.Query ~> DSL
chartEval (Q.Select sel next) = do
  case sel of
    Q.Value a    → updatePicker ST._value Q.Value a
    Q.ValueAgg a → updateSelect ST._valueAgg a
    Q.Multiple a → updatePicker ST._multiple Q.Multiple a
    Q.Parallel a → updatePicker ST._parallel Q.Parallel a
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
        Q.Value _    → H.modify (ST._value ∘ _value ?~ value')
        Q.Multiple _ → H.modify (ST._multiple ∘ _value ?~ value')
        Q.Parallel _ → H.modify (ST._parallel ∘ _value ?~ value')
        _ → pure unit
      H.modify _ { picker = Nothing }
      raiseUpdate

loadModel ∷ M.GaugeR → DSL Unit
loadModel r =
  H.modify _
    { value = fromSelected (Just r.value)
    , valueAgg = fromSelected (Just r.valueAggregation)
    , multiple = fromSelected r.multiple
    , parallel = fromSelected r.parallel
    }

synchronizeChildren ∷ DSL Unit
synchronizeChildren = do
  st ← H.get
  let
    newValue =
      setPreviousValueFrom (Just st.value)
        $ autoSelect
        $ newSelect
        $ st.axes.value

    newValueAggregation =
      setPreviousValueFrom (Just st.valueAgg)
        $ nonMaybeAggregationSelect

    newParallel =
      setPreviousValueFrom (Just st.parallel)
        $ autoSelect
        $ newSelect
        $ ifSelected [newValue]
        $ st.axes.category
        ⊕ st.axes.time

    newMultiple =
      setPreviousValueFrom (Just st.multiple)
        $ autoSelect
        $ newSelect
        $ ifSelected [newValue]
        $ st.axes.category
        ⊕ st.axes.time
        ⊝ newParallel

  H.modify _
    { value = newValue
    , valueAgg = newValueAggregation
    , multiple = newMultiple
    , parallel = newParallel
    }
