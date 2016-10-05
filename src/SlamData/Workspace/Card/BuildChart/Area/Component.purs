module SlamData.Workspace.Card.BuildChart.Area.Component
  ( areaBuilderComponent
  ) where

import SlamData.Prelude

import Data.Int as Int
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
import SlamData.Workspace.Card.BuildChart.Area.Component.ChildSlot as CS
import SlamData.Workspace.Card.BuildChart.Area.Component.State as ST
import SlamData.Workspace.Card.BuildChart.Area.Component.Query as Q
import SlamData.Workspace.Card.BuildChart.Area.Model as M

type DSL =
  H.ParentDSL ST.State CS.ChildState Q.QueryC CS.ChildQuery Slam CS.ChildSlot

type HTML =
  H.ParentHTML CS.ChildState Q.QueryC CS.ChildQuery Slam CS.ChildSlot

areaBuilderComponent ∷ H.Component CC.CardStateP CC.CardQueryP Slam
areaBuilderComponent = CC.makeCardComponent
  { cardType: CT.ChartOptions CHT.Area
  , component: H.parentComponent { render, eval, peek: Just (peek ∘ H.runChildF) }
  , initialState: H.parentState ST.initialState
  , _State: CC._BuildAreaState
  , _Query: CC.makeQueryPrism' CC._BuildAreaQuery
  }

render ∷ ST.State → HTML
render state =
  HH.div_
    [ renderHighLOD state
    , renderLowLOD (CT.darkCardGlyph $ CT.ChartOptions CHT.Area) left state.levelOfDetails
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
    , HH.hr_
    , renderSeries state
    , HH.hr_
    , row [ renderIsStacked state, renderIsSmooth state ]
    , row [ renderAxisLabelAngle state, renderAxisLabelFontSize state ]
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
              Q.Value _       → "Choose measure"
              Q.Series _      → "Choose series"
              _ → ""
          , label: show
          , render: HH.text ∘ show
          , values: groupJCursors (List.fromFoldable options)
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
    [ HH.label [ HP.classes [ B.controlLabel ] ] [ HH.text "Measure" ]
    , BCI.pickerWithSelect
        (BCI.secondary (Just "Measure") (selecting Q.Value))
        state.value
        (BCI.aggregation (Just "Measure Aggregation") (selecting Q.ValueAgg))
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

renderAxisLabelFontSize ∷ ST.State → HTML
renderAxisLabelFontSize state =
  HH.form
    [ HP.classes [ B.colXs6, CSS.axisLabelParam ]
    , Cp.nonSubmit
    ]
    [ HH.label [ HP.classes [ B.controlLabel ] ] [ HH.text "Label font size" ]
    , HH.input
        [ HP.classes [ B.formControl ]
        , HP.value $ show $ state.axisLabelFontSize
        , ARIA.label "Axis label font size"
        , HE.onValueChange $ HE.input (\s → right ∘ Q.SetAxisLabelFontSize s)
        ]
    ]

renderIsStacked ∷ ST.State → HTML
renderIsStacked state =
  HH.form
    [ HP.classes [ B.colXs6, CSS.axisLabelParam ]
    , Cp.nonSubmit
    ]
    [ HH.label [ HP.classes [ B.controlLabel ] ] [ HH.text "Stacked" ]
    , HH.input
        [ HP.inputType HP.InputCheckbox
        , HP.checked state.isStacked
        , ARIA.label "Stacked"
        , HE.onChecked $ HE.input_ (right ∘ Q.ToggleStacked)
        ]

    ]

renderIsSmooth ∷ ST.State → HTML
renderIsSmooth state =
  HH.form
    [ HP.classes [ B.colXs6, CSS.axisLabelParam ]
    , Cp.nonSubmit
    ]
    [ HH.label [ HP.classes [ B.controlLabel ] ] [ HH.text "Smooth" ]
    , HH.input
        [ HP.inputType HP.InputCheckbox
        , HP.checked state.isStacked
        , ARIA.label "Smooth"
        , HE.onChecked $ HE.input_ (right ∘ Q.ToggleSmooth)
        ]
    ]

eval ∷ Q.QueryC ~> DSL
eval = cardEval ⨁ areaBuilderEval

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
        { dimension: _
        , value: _
        , valueAggregation: _
        , series: st.series ^. _value
        , isStacked: st.isStacked
        , isSmooth: st.isSmooth
        , axisLabelAngle: st.axisLabelAngle
        , axisLabelFontSize: st.axisLabelFontSize
        }
        <$> (st.dimension ^. _value)
        <*> (st.value ^. _value)
        <*> (st.valueAgg ^. _value)
    pure $ k $ Card.BuildArea model
  CC.Load (Card.BuildArea (Just model)) next → do
    loadModel model
    H.modify _{ isStacked = model.isStacked
              , isSmooth = model.isSmooth
              , axisLabelAngle = model.axisLabelAngle
              , axisLabelFontSize = model.axisLabelFontSize
              }
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

areaBuilderEval ∷ Q.Query ~> DSL
areaBuilderEval = case _ of
  Q.SetAxisLabelAngle str next → do
    let fl = readFloat str
    unless (isNaN fl) do
      H.modify _{axisLabelAngle = fl}
      CC.raiseUpdatedP' CC.EvalModelUpdate
    pure next
  Q.SetAxisLabelFontSize str next → do
    let mbFS = Int.fromString str
    for_ mbFS \fs → do
      H.modify _{axisLabelFontSize = fs}
      CC.raiseUpdatedP' CC.EvalModelUpdate
    pure next
  Q.ToggleSmooth next → do
    H.modify \s → s{isSmooth = not s.isSmooth}
    CC.raiseUpdatedP' CC.EvalModelUpdate
    pure next
  Q.ToggleStacked next → do
    H.modify \s → s{isStacked = not s.isStacked}
    CC.raiseUpdatedP' CC.EvalModelUpdate
    pure next
  Q.Select sel next → do
    case sel of
      Q.Dimension a      → updatePicker ST._dimension Q.Dimension a
      Q.Value a          → updatePicker ST._value Q.Value a
      Q.ValueAgg a       → updateSelect ST._valueAgg a
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
        ⊕ st.axes.value

    newValue =
      setPreviousValueFrom (Just st.value)
        $ autoSelect
        $ newSelect
        $ st.axes.value

    newValueAggregation =
      setPreviousValueFrom (Just st.valueAgg)
        $ nonMaybeAggregationSelect

    newSeries =
      setPreviousValueFrom (Just st.series)
        $ autoSelect
        $ newSelect
        $ ifSelected [ newDimension ]
        $ st.axes.category
        ⊝ newDimension

  H.modify _
    { value = newValue
    , valueAgg = newValueAggregation
    , dimension = newDimension
    , series = newSeries
    }

loadModel ∷ M.AreaR → DSL Unit
loadModel r = void do
  H.modify _
    { value = fromSelected (Just r.value)
    , valueAgg = fromSelected (Just r.valueAggregation)
    , dimension = fromSelected (Just r.dimension)
    , series = fromSelected r.series
    }
