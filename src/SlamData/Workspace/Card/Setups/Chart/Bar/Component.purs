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

module SlamData.Workspace.Card.Setups.Chart.Bar.Component
  ( barBuilderComponent
  ) where

import SlamData.Prelude

import Data.Lens ((^?), (.~), _Just)

import Global (readFloat, isNaN)

import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.HTML.Properties.ARIA as ARIA

import SlamData.Render.ClassName as CN
import SlamData.Render.Common (row)
import SlamData.Workspace.Card.CardType as CT
import SlamData.Workspace.Card.CardType.ChartType as CHT
import SlamData.Workspace.Card.Component as CC
import SlamData.Workspace.Card.Eval.State as ES
import SlamData.Workspace.Card.Model as M
import SlamData.Workspace.Card.Setups.CSS as CSS
import SlamData.Workspace.Card.Setups.Chart.Bar.Component.ChildSlot as CS
import SlamData.Workspace.Card.Setups.Chart.Bar.Component.Query as Q
import SlamData.Workspace.Card.Setups.Chart.Bar.Component.State as ST
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
package = P.onPrism (M._BuildBar ∘ _Just) $ DS.interpret do
  category ←
    P.field PL._category PP._category
      >>= P.addAll
  value ←
    P.field PL._value PP._value
      >>= P.addSource _.value
      >>= P.isFilteredBy category

  stack ←
    P.optional PL._stack PP._stack
      >>= P.addAll
      >>= P.isFilteredBy category
      >>= P.isFilteredBy value
      >>= P.isActiveWhen category

  parallel ←
    P.optional PL._parallel PP._parallel
      >>= P.addAll
      >>= P.isFilteredBy category
      >>= P.isFilteredBy stack
      >>= P.isFilteredBy value
      >>= P.isActiveWhen category

  pure unit

barBuilderComponent ∷ CC.CardOptions → CC.CardComponent
barBuilderComponent =
  CC.makeCardComponent (CT.ChartOptions CHT.Bar) $ H.parentComponent
    { render
    , eval: cardEval ⨁ setupEval
    , receiver: const Nothing
    , initialState: const ST.initialState
    }

render ∷ ST.State → HTML
render state =
  HH.div
    [ HP.classes [ CSS.chartEditor ]  ]
    [ HH.slot' CS.cpDims unit (DM.component package) unit
        $ HE.input \l → right ∘ Q.HandleDims l
    ,  HH.hr_
    , row [ renderAxisLabelAngle state ]
    ]

renderAxisLabelAngle ∷ ST.State → HTML
renderAxisLabelAngle state =
  HH.div
    [ HP.classes [ CSS.axisLabelParam ]
    ]
    [ HH.label [ HP.classes [ CN.controlLabel ] ] [ HH.text "Label angle" ]
    , HH.input
        [ HP.classes [ CN.formControl ]
        , HP.value $ show $ state.axisLabelAngle
        , ARIA.label "Axis label angle"
        , HE.onValueChange $ HE.input (\s → right ∘ Q.SetAxisLabelAngle s)
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
      inp = M.BuildBar $ Just
        { axisLabelAngle: st.axisLabelAngle
        , category: D.topDimension
        , value: D.topDimension
        , stack: Nothing
        , parallel: Nothing
        }
    out ← H.query' CS.cpDims unit $ H.request $ DQ.Save inp
    pure $ k case join out of
      Nothing → M.BuildBar Nothing
      Just a → a
  CC.Load m next → do
    _ ← H.query' CS.cpDims unit $ H.action $ DQ.Load $ Just m
    for_ (m ^? M._BuildBar ∘ _Just) \r →
      H.modify _{ axisLabelAngle = r.axisLabelAngle }
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
  Q.SetAxisLabelAngle str next → do
    let fl = readFloat str
    unless (isNaN fl) do
      H.modify $ ST._axisLabelAngle .~ fl
      raiseUpdate
    pure next
  Q.HandleDims q next → do
    case q of
      DQ.Update _ → raiseUpdate
    pure next
