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

module SlamData.Workspace.Card.BuildChart.Graph.Component
  ( graphBuilderComponent
  ) where

import SlamData.Prelude

import Data.Lens ((^?), (^.), (?~), (.~))
import Data.Lens as Lens
import Data.List as List

import Halogen as H
import Halogen.HTML.Indexed as HH
import Halogen.CustomProps as Cp
import Halogen.HTML.Events.Indexed as HE
import Halogen.HTML.Properties.Indexed as HP
import Halogen.HTML.Properties.Indexed.ARIA as ARIA
import Halogen.Themes.Bootstrap3 as B

import Global (readFloat, isNaN)

import SlamData.Monad (Slam)
import SlamData.Workspace.Card.Model as Card
import SlamData.Workspace.Card.Port as Port
import SlamData.Render.Common (row)
import SlamData.Form.Select (newSelect,setPreviousValueFrom, autoSelect, ifSelected, (⊝), _value, fromSelected)
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
import SlamData.Workspace.Card.BuildChart.Graph.Component.ChildSlot as CS
import SlamData.Workspace.Card.BuildChart.Graph.Component.State as ST
import SlamData.Workspace.Card.BuildChart.Graph.Component.Query as Q
import SlamData.Workspace.Card.BuildChart.Graph.Model as M

type DSL =
  H.ParentDSL ST.State CS.ChildState Q.QueryC CS.ChildQuery Slam CS.ChildSlot

type HTML =
  H.ParentHTML CS.ChildState Q.QueryC CS.ChildQuery Slam CS.ChildSlot

graphBuilderComponent ∷ H.Component CC.CardStateP CC.CardQueryP Slam
graphBuilderComponent = CC.makeCardComponent
  { cardType: CT.ChartOptions CHT.Graph
  , component: H.parentComponent { render, eval, peek: Just (peek ∘ H.runChildF) }
  , initialState: H.parentState ST.initialState
  , _State: CC._BuildGraphState
  , _Query: CC.makeQueryPrism' CC._BuildGraphQuery
  }

render ∷ ST.State → HTML
render state =
  HH.div_
    [ renderHighLOD state
    , renderLowLOD (CT.darkCardGlyph $ CT.ChartOptions CHT.Graph) left state.levelOfDetails
    ]

renderHighLOD ∷ ST.State → HTML
renderHighLOD state =
  HH.div
    [ HP.classes
      $ [ CSS.chartEditor ]
      ⊕ (guard (state.levelOfDetails ≠ High) $> B.hidden)
    ]
    [ renderSource state
    , renderTarget state
    , HH.hr_
    , renderSize state
    , HH.hr_
    , renderColor state
    , HH.hr_
    , row [ renderMaxSize state, renderMinSize state ]
    , row [ renderCircular state ]
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
              Q.Source _    → "Choose edge source"
              Q.Target _    → "Choose edge target"
              Q.Size _      → "Choose node size"
              Q.Color _     → "Choose node category"
              _ → ""
          , label: DPC.labelNode show
          , render: DPC.renderNode show
          , values: groupJCursors (List.fromFoldable options)
          , isSelectable: DPC.isLeafPath
          }
      , initialState: H.parentState DPC.initialState
      }

renderSource ∷ ST.State → HTML
renderSource state =
  HH.form
    [ HP.classes [ CSS.chartConfigureForm]
    , Cp.nonSubmit
    ]
    [ HH.label [ HP.classes [ B.controlLabel ] ] [ HH.text "Edge source" ]
    , BCI.pickerInput
        (BCI.primary (Just "Edge source") (selecting Q.Source))
        state.source
    ]

renderTarget ∷ ST.State → HTML
renderTarget state =
  HH.form
    [ HP.classes [ CSS.chartConfigureForm ]
    , Cp.nonSubmit
    ]
    [ HH.label [ HP.classes [ B.controlLabel ] ] [ HH.text "Edge target" ]
    , BCI.pickerInput
        (BCI.secondary (Just "Edge target") (selecting Q.Target))
        state.target
    ]

renderSize ∷ ST.State → HTML
renderSize state =
  HH.form
    [ HP.classes [ CSS.withAggregation, CSS.chartConfigureForm ]
    , Cp.nonSubmit
    ]
    [ HH.label [ HP.classes [ B.controlLabel ] ] [ HH.text "Node size" ]
    , BCI.pickerWithSelect
        (BCI.secondary (Just "Node size") (selecting Q.Size))
        state.size
        (BCI.aggregation (Just "Node size aggregation") (selecting Q.SizeAgg))
        state.sizeAgg
    ]

renderColor ∷ ST.State → HTML
renderColor state =
  HH.form
    [ HP.classes [ CSS.chartConfigureForm ]
    , Cp.nonSubmit
    ]
    [ HH.label [ HP.classes [ B.controlLabel ] ] [ HH.text "Node category" ]
    , BCI.pickerInput
        (BCI.secondary (Just "Node category") (selecting Q.Color))
        state.color
    ]

renderMaxSize ∷ ST.State → HTML
renderMaxSize state =
  HH.form
    [ HP.classes [ B.colXs6, CSS.axisLabelParam ]
    , Cp.nonSubmit
    ]
    [ HH.label [ HP.classes [ B.controlLabel ] ] [ HH.text "Max node size" ]
    , HH.input
        [ HP.classes [ B.formControl ]
        , HP.value $ show $ state.maxSize
        , ARIA.label "Max node size"
        , HE.onValueChange $ HE.input (\s → right ∘ Q.SetMaxNodeSize s)
        ]
    ]

renderMinSize ∷ ST.State → HTML
renderMinSize state =
  HH.form
    [ HP.classes [ B.colXs6, CSS.axisLabelParam ]
    , Cp.nonSubmit
    ]
    [ HH.label [ HP.classes [ B.controlLabel ] ] [ HH.text "Min node size" ]
    , HH.input
        [ HP.classes [ B.formControl ]
        , HP.value $ show $ state.minSize
        , ARIA.label "Min node size"
        , HE.onValueChange $ HE.input (\s → right ∘ Q.SetMinNodeSize s)
        ]
    ]

renderCircular ∷ ST.State → HTML
renderCircular state =
  HH.form
    [ HP.classes [ B.colXs6, CSS.chartDetailParam ]
    , Cp.nonSubmit
    ]
    [ HH.label [ HP.classes [ B.controlLabel ] ] [ HH.text "Circular layout" ]
    , HH.input
        [ HP.inputType HP.InputCheckbox
        , HP.checked state.circular
        , ARIA.label "Circular layout"
        , HE.onChecked $ HE.input_ (right ∘ Q.ToggleCircularLayout)
        ]
    ]

eval ∷ Q.QueryC ~> DSL
eval = cardEval ⨁ graphBuilderEval

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
        { source: _
        , target: _
        , size: st.size ^. _value
        , color: st.color ^. _value
        , sizeAggregation: st.sizeAgg ^. _value
        , minSize: st.minSize
        , maxSize: st.maxSize
        , circular: st.circular
        }
        <$> (st.source ^. _value)
        <*> (st.target ^. _value)
    pure $ k $ Card.BuildGraph model
  CC.Load (Card.BuildGraph model) next → do
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

graphBuilderEval ∷ Q.Query ~> DSL
graphBuilderEval = case _ of
  Q.ToggleCircularLayout next → do
    H.modify \x → x{circular = not x.circular}
    CC.raiseUpdatedP' CC.EvalModelUpdate
    pure next
  Q.SetMinNodeSize str next → do
    let fl = readFloat str
    unless (isNaN fl) do
      H.modify _{minSize = fl}
      CC.raiseUpdatedP' CC.EvalModelUpdate
    pure next
  Q.SetMaxNodeSize str next → do
    let fl = readFloat str
    unless (isNaN fl) do
      H.modify _{maxSize = fl}
      CC.raiseUpdatedP' CC.EvalModelUpdate
    pure next
  Q.Select sel next → do
    case sel of
      Q.Source a  → updatePicker ST._source Q.Source a
      Q.Target a  → updatePicker ST._target Q.Target a
      Q.Size a    → updatePicker ST._size Q.Size a
      Q.SizeAgg a → updateSelect ST._sizeAgg a
      Q.Color a   → updatePicker ST._color Q.Color a
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
        Q.Source _ → H.modify (ST._source ∘ _value ?~ value')
        Q.Target _ → H.modify (ST._target ∘ _value ?~ value')
        Q.Size _   → H.modify (ST._size ∘ _value ?~ value')
        Q.Color _  → H.modify (ST._color ∘ _value ?~ value')
        _ → pure unit
      H.modify _ { picker = Nothing }
      raiseUpdate

loadModel ∷ M.GraphR → DSL Unit
loadModel r =
  H.modify _
    { source = fromSelected (Just r.source)
    , target = fromSelected (Just r.target)
    , size = fromSelected r.size
    , sizeAgg = fromSelected r.sizeAggregation
    , color = fromSelected r.color
    }

synchronizeChildren ∷ DSL Unit
synchronizeChildren = do
  st ← H.get
  let
    newSource =
      setPreviousValueFrom (Just st.source)
        $ autoSelect
        $ newSelect
        $ st.axes.category

    newTarget =
      setPreviousValueFrom (Just st.target)
        $ autoSelect
        $ newSelect
        $ ifSelected [newSource]
        $ st.axes.category
        ⊝ newSource

    newSize =
      setPreviousValueFrom (Just st.size)
        $ newSelect
        $ ifSelected [newTarget]
        $ st.axes.value

    newColor =
      setPreviousValueFrom (Just st.color)
        $ newSelect
        $ ifSelected [newTarget]
        $ st.axes.category
        ⊕ st.axes.time
        ⊝ newSource
        ⊝ newTarget

    newSizeAggregation =
      setPreviousValueFrom (Just st.sizeAgg)
        $ nonMaybeAggregationSelect

  H.modify _
    { source = newSource
    , target = newTarget
    , size = newSize
    , sizeAgg = newSizeAggregation
    , color = newColor
    }
