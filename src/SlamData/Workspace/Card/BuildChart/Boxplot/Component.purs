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

module SlamData.Workspace.Card.BuildChart.Boxplot.Component
  ( boxplotBuilderComponent
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

import SlamData.Workspace.Card.BuildChart.CSS as CSS
import SlamData.Workspace.Card.BuildChart.DimensionPicker.Component as DPC
import SlamData.Workspace.Card.BuildChart.DimensionPicker.JCursor (groupJCursors, flattenJCursors)
import SlamData.Workspace.Card.BuildChart.Inputs as BCI
import SlamData.Workspace.Card.BuildChart.Boxplot.Component.ChildSlot as CS
import SlamData.Workspace.Card.BuildChart.Boxplot.Component.State as ST
import SlamData.Workspace.Card.BuildChart.Boxplot.Component.Query as Q
import SlamData.Workspace.Card.BuildChart.Boxplot.Model as M

type DSL =
  H.ParentDSL ST.State CS.ChildState Q.QueryC CS.ChildQuery Slam CS.ChildSlot

type HTML =
  H.ParentHTML CS.ChildState Q.QueryC CS.ChildQuery Slam CS.ChildSlot

boxplotBuilderComponent ∷ H.Component CC.CardStateP CC.CardQueryP Slam
boxplotBuilderComponent = CC.makeCardComponent
  { cardType: CT.ChartOptions CHT.Boxplot
  , component: H.parentComponent { render, eval, peek: Just (peek ∘ H.runChildF) }
  , initialState: H.parentState ST.initialState
  , _State: CC._BuildBoxplotState
  , _Query: CC.makeQueryPrism' CC._BuildBoxplotQuery
  }

render ∷ ST.State → HTML
render state =
  HH.div_
    [ renderHighLOD state
    , renderLowLOD (CT.darkCardGlyph $ CT.ChartOptions CHT.Boxplot) left state.levelOfDetails
    ]

renderHighLOD ∷ ST.State → HTML
renderHighLOD state =
  HH.div
    [ HP.classes
        $ [ CSS.chartEditor ]
        ⊕ (guard (state.levelOfDetails ≠ High) $> B.hidden)
    ]
    [ renderDimension state
    , renderValue state
    , HH.hr_
    , renderSeries state
    , renderParallel state
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
              Q.Parallel _    → "Choose parallel"
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
    [ HP.classes [ CSS.chartConfigureForm ]
    , Cp.nonSubmit
    ]
    [ HH.label [ HP.classes [ B.controlLabel ] ] [ HH.text "Measure" ]
    , BCI.pickerInput
        (BCI.primary (Just "Measure") (selecting Q.Value))
        state.value
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

renderParallel ∷ ST.State → HTML
renderParallel state =
  HH.form
    [ HP.classes [ CSS.chartConfigureForm ]
    , Cp.nonSubmit
    ]
    [ HH.label [ HP.classes [ B.controlLabel ] ] [ HH.text "Parallel" ]
    , BCI.pickerInput
        (BCI.secondary (Just "Parallel") (selecting Q.Parallel))
        state.parallel
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
        { dimension: _
        , value: _
        , series: st.series ^. _value
        , parallel: st.parallel ^. _value
        }
        <$> (st.dimension ^. _value)
        <*> (st.value ^. _value)
    pure $ k $ Card.BuildBoxplot model
  CC.Load (Card.BuildBoxplot (Just model)) next → do
    loadModel model
    pure next
  CC.Load card next →
    pure next
  CC.SetDimensions dims next → do
    H.modify
      _ {levelOfDetails =
            if dims.height < 516.0 ∨ dims.height < 416.0
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
    Q.Value a     → updatePicker ST._value Q.Value a
    Q.Dimension a → updatePicker ST._dimension Q.Dimension a
    Q.Series a    → updatePicker ST._series Q.Series a
    Q.Parallel a  → updatePicker ST._parallel Q.Parallel a
  pure next
  where
  updatePicker l q = case _ of
    BCI.Open opts → H.modify (ST.showPicker q opts)
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
        Q.Value _     → H.modify (ST._value ∘ _value ?~ value')
        Q.Dimension _ → H.modify (ST._dimension ∘ _value ?~ value')
        Q.Series _    → H.modify (ST._series ∘ _value ?~ value')
        Q.Parallel _  → H.modify (ST._parallel ∘ _value ?~ value')
      H.modify _ { picker = Nothing }
      raiseUpdate

synchronizeChildren ∷ DSL Unit
synchronizeChildren = void do
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

    newValue =
      setPreviousValueFrom (Just st.value)
        $ autoSelect
        $ newSelect
        $ st.axes.value
        ⊝ newDimension

    newSeries =
      setPreviousValueFrom (Just st.series)
        $ newSelect
        $ ifSelected [newDimension]
        $ st.axes.category
        ⊕ st.axes.time
        ⊝ newDimension

    newParallel =
      setPreviousValueFrom (Just st.parallel)
        $ newSelect
        $ ifSelected [newDimension]
        $ st.axes.category
        ⊕ st.axes.time
        ⊝ newDimension
        ⊝ newSeries

  H.modify _
    { value = newValue
    , dimension = newDimension
    , series = newSeries
    , parallel = newParallel
    }

loadModel ∷ M.BoxplotR → DSL Unit
loadModel r =
  H.modify _
    { value = fromSelected (Just r.value)
    , dimension = fromSelected (Just r.dimension)
    , series = fromSelected r.series
    , parallel = fromSelected r.parallel
    }
