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

module SlamData.Workspace.Card.Setups.Chart.Graph.Component
  ( graphBuilderComponent
  ) where

import SlamData.Prelude

import Data.Lens ((^?), _Just)

import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.HTML.Properties.ARIA as ARIA

import Global (readFloat, isNaN)

import SlamData.Render.ClassName as CN
import SlamData.Render.Common (row)
import SlamData.Workspace.Card.CardType as CT
import SlamData.Workspace.Card.CardType.ChartType as CHT
import SlamData.Workspace.Card.Component as CC
import SlamData.Workspace.Card.Eval.State as ES
import SlamData.Workspace.Card.Model as M
import SlamData.Workspace.Card.Setups.CSS as CSS
import SlamData.Workspace.Card.Setups.Chart.Graph.Component.ChildSlot as CS
import SlamData.Workspace.Card.Setups.Chart.Graph.Component.Query as Q
import SlamData.Workspace.Card.Setups.Chart.Graph.Component.State as ST
import SlamData.Workspace.Card.Setups.Dimension as D
import SlamData.Workspace.Card.Setups.DimensionMap.Component as DM
import SlamData.Workspace.Card.Setups.DimensionMap.Component.Query as DQ
import SlamData.Workspace.Card.Setups.DimensionMap.Component.State as DS
import SlamData.Workspace.Card.Setups.Package.DSL as P
import SlamData.Workspace.Card.Setups.Package.Lenses as PL
import SlamData.Workspace.Card.Setups.Package.Projection as PP
import SlamData.Workspace.LevelOfDetails (LevelOfDetails(..))

type DSL = CC.InnerCardParentDSL ST.State Q.Query CS.ChildQuery CS.ChildSlot
type HTML = CC.InnerCardParentHTML Q.Query CS.ChildQuery CS.ChildSlot

package ∷ DS.Package
package = P.onPrism (M._BuildGraph ∘ _Just) $ DS.interpret do
  source ←
    P.field PL._source PP._source
      >>= P.addSource _.category

  target ←
    P.field PL._target PP._target
      >>= P.addSource _.category
      >>= P.isFilteredBy source

  size ←
    P.optional PL._size PP._size
      >>= P.addSource _.value

  color ←
    P.optional PL._color PP._color
      >>= P.addAll
      >>= P.isFilteredBy size
      >>= P.isFilteredBy source
      >>= P.isFilteredBy target

  pure unit

graphBuilderComponent ∷ CC.CardOptions → CC.CardComponent
graphBuilderComponent =
  CC.makeCardComponent (CT.ChartOptions CHT.Graph) $ H.parentComponent
    { render
    , eval: cardEval ⨁ setupEval
    , receiver: const Nothing
    , initialState: const ST.initialState
    }


render ∷ ST.State → HTML
render state =
  HH.div
    [ HP.classes [ CSS.chartEditor ]
    ]
    [ HH.slot' CS.cpDims unit (DM.component package) unit
        $ HE.input \l → right ∘ Q.HandleDims l
    , HH.hr_
    , row [ renderCircular state ]
    , HH.hr_
    , row [ renderMaxSize state, renderMinSize state ]
    ]

renderMaxSize ∷ ST.State → HTML
renderMaxSize state =
  HH.div
    [ HP.classes [ CSS.axisLabelParam ]
    ]
    [ HH.label [ HP.classes [ CN.controlLabel ] ] [ HH.text "Max node size" ]
    , HH.input
        [ HP.classes [ CN.formControl ]
        , HP.value $ show $ state.maxSize
        , ARIA.label "Max node size"
        , HE.onValueChange $ HE.input (\s → right ∘ Q.SetMaxNodeSize s)
        ]
    ]

renderMinSize ∷ ST.State → HTML
renderMinSize state =
  HH.div
    [ HP.classes [ CSS.axisLabelParam ]
    ]
    [ HH.label [ HP.classes [ CN.controlLabel ] ] [ HH.text "Min node size" ]
    , HH.input
        [ HP.classes [ CN.formControl ]
        , HP.value $ show $ state.minSize
        , ARIA.label "Min node size"
        , HE.onValueChange $ HE.input (\s → right ∘ Q.SetMinNodeSize s)
        ]
    ]

renderCircular ∷ ST.State → HTML
renderCircular state =
  HH.div
    [ HP.classes [ CSS.axisLabelParam ]
    ]
    [ HH.label [ HP.classes [ CN.controlLabel ] ]
        [ HH.input
            [ HP.type_ HP.InputCheckbox
            , HP.checked state.circular
            , ARIA.label "Circular layout"
            , HE.onChecked $ HE.input_ (right ∘ Q.ToggleCircularLayout)
            ]
        , HH.text "Circular layout"
        ]
    ]


cardEval ∷ CC.CardEvalQuery ~> DSL
cardEval = case _ of
  CC.Activate next →
    pure next
  CC.Deactivate next →
    pure next
  CC.Save k → do
    st ← H.get
    let
      inp = M.BuildGraph $ Just
        { source: D.topDimension
        , target: D.topDimension
        , size: Nothing
        , color: Nothing
        , minSize: st.minSize
        , maxSize: st.maxSize
        , circular: st.circular
        }
    out ← H.query' CS.cpDims unit $ H.request $ DQ.Save inp
    pure $ k case join out of
      Nothing → M.BuildGraph Nothing
      Just a → a
  CC.Load m next → do
    _ ← H.query' CS.cpDims unit $ H.action $ DQ.Load $ Just m
    for_ (m ^? M._BuildGraph ∘ _Just) \r →
      H.modify _{ minSize = r.minSize, maxSize = r.maxSize, circular = r.circular }
    pure next
  CC.ReceiveInput _ _ next →
    pure next
  CC.ReceiveOutput _ _ next →
    pure next
  CC.ReceiveState evalState next → do
    for_ (evalState ^? ES._Axes) \axes → do
      H.query' CS.cpDims unit $ H.action $ DQ.SetAxes axes
    pure next
  CC.ReceiveDimensions dims reply → do
    pure $ reply
      if dims.width < 576.0 ∨ dims.height < 416.0
      then Low
      else High

raiseUpdate ∷ DSL Unit
raiseUpdate =
  H.raise CC.modelUpdate

setupEval ∷ Q.Query ~> DSL
setupEval = case _ of
  Q.ToggleCircularLayout next → do
    H.modify \x → x{circular = not x.circular}
    raiseUpdate
    pure next
  Q.SetMinNodeSize str next → do
    let fl = readFloat str
    unless (isNaN fl) do
      H.modify _{minSize = fl}
      raiseUpdate
    pure next
  Q.SetMaxNodeSize str next → do
    let fl = readFloat str
    unless (isNaN fl) do
      H.modify _{maxSize = fl}
      raiseUpdate
    pure next
  Q.HandleDims q next → do
    case q of
      DQ.Update _ → raiseUpdate
    pure next
