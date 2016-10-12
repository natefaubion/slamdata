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

module SlamData.Workspace.Card.BuildChart.Sankey.Component
  ( sankeyBuilderComponent
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
import SlamData.Workspace.Card.BuildChart.Sankey.Component.ChildSlot as CS
import SlamData.Workspace.Card.BuildChart.Sankey.Component.State as ST
import SlamData.Workspace.Card.BuildChart.Sankey.Component.Query as Q
import SlamData.Workspace.Card.BuildChart.Sankey.Model as M


type DSL =
  H.ParentDSL ST.State CS.ChildState Q.QueryC CS.ChildQuery Slam CS.ChildSlot

type HTML =
  H.ParentHTML CS.ChildState Q.QueryC CS.ChildQuery Slam CS.ChildSlot

sankeyBuilderComponent ∷ H.Component CC.CardStateP CC.CardQueryP Slam
sankeyBuilderComponent = CC.makeCardComponent
  { cardType: CT.ChartOptions CHT.Sankey
  , component: H.parentComponent { render, eval, peek : Just (peek ∘ H.runChildF) }
  , initialState: H.parentState ST.initialState
  , _State: CC._BuildSankeyState
  , _Query: CC.makeQueryPrism' CC._BuildSankeyQuery
  }

render ∷ ST.State → HTML
render state =
  HH.div_
    [ renderHighLOD state
    , renderLowLOD (CT.darkCardGlyph $ CT.ChartOptions CHT.Sankey) left state.levelOfDetails
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
    , renderValue state
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
              Q.Value _  → "Choose weight"
              Q.Source _ → "Choose source"
              Q.Target _ → "Choose target"
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
    [ HP.classes [ CSS.chartConfigureForm ]
    , Cp.nonSubmit
    ]
    [ HH.label [ HP.classes [ B.controlLabel ] ] [ HH.text "Link source" ]
    , BCI.pickerInput
        (BCI.primary (Just "Link source") (selecting Q.Source))
        state.source
    ]

renderTarget ∷ ST.State → HTML
renderTarget state =
  HH.form
    [ HP.classes [ CSS.chartConfigureForm ]
    , Cp.nonSubmit
    ]
    [ HH.label [ HP.classes [ B.controlLabel ] ] [ HH.text "Link target" ]
    , BCI.pickerInput
        (BCI.secondary (Just "Link target") (selecting Q.Target))
        state.target
    ]

renderValue ∷ ST.State → HTML
renderValue state =
  HH.form
    [ HP.classes [ CSS.withAggregation, CSS.chartConfigureForm ]
    , Cp.nonSubmit
    ]
    [ HH.label [ HP.classes [ B.controlLabel ] ] [ HH.text "Weight" ]
    , BCI.pickerWithSelect
        (BCI.secondary (Just "Weight") (selecting Q.Value))
        state.value
        (BCI.aggregation (Just "Weight Aggregation") (selecting Q.ValueAgg))
        state.valueAgg
    ]

eval ∷ Q.QueryC ~> DSL
eval = cardEval ⨁ chartEval

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
        , value: _
        , valueAggregation: _
        }
        <$> (st.source ^. _value)
        <*> (st.target ^. _value)
        <*> (st.value ^.  _value)
        <*> (st.valueAgg ^. _value)
    pure $ k $ Card.BuildSankey model
  CC.Load (Card.BuildSankey model) next → do
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
    Q.Source a   → updatePicker ST._source Q.Source a
    Q.Target a   → updatePicker ST._target Q.Target a
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
        Q.Value _  → H.modify (ST._value ∘ _value ?~ value')
        Q.Source _ → H.modify (ST._source ∘ _value ?~ value')
        Q.Target _ → H.modify (ST._target ∘ _value ?~ value')
        _ → pure unit
      H.modify _ { picker = Nothing }
      raiseUpdate

synchronizeChildren ∷ DSL Unit
synchronizeChildren = void do
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
        $ ifSelected [ newSource ]
        $ st.axes.category
        ⊝ newSource

    newValue =
      setPreviousValueFrom (Just st.value)
        $ autoSelect
        $ newSelect
        $ ifSelected [newTarget]
        $ st.axes.value

    newValueAggregation =
      setPreviousValueFrom (Just st.valueAgg)
        $ nonMaybeAggregationSelect

  H.modify _
    { source = newSource
    , target = newTarget
    , value = newValue
    , valueAgg = newValueAggregation
    }

loadModel ∷ M.SankeyR → DSL Unit
loadModel r =
  H.modify _
    { source = fromSelected (Just r.source)
    , target = fromSelected (Just r.target)
    , value = fromSelected (Just r.value)
    , valueAgg = fromSelected (Just r.valueAggregation)
    }
