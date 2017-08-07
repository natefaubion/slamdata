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

module SlamData.Workspace.Card.Setups.Chart.Funnel.Component
  ( funnelBuilderComponent
  ) where

import SlamData.Prelude

import Data.Lens ((^?), (.~), _Just)

import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Halogen.HTML.Events as HE

import SlamData.Common.Align (alignSelect)
import SlamData.Common.Sort (sortSelect)
import SlamData.Form.Select as S
import SlamData.Render.ClassName as CN
import SlamData.Render.Common (row)
import SlamData.Workspace.Card.CardType as CT
import SlamData.Workspace.Card.CardType.ChartType as CHT
import SlamData.Workspace.Card.Component as CC
import SlamData.Workspace.Card.Eval.State as ES
import SlamData.Workspace.Card.Model as M
import SlamData.Workspace.Card.Setups.CSS as CSS
import SlamData.Workspace.Card.Setups.Chart.Funnel.Component.ChildSlot as CS
import SlamData.Workspace.Card.Setups.Chart.Funnel.Component.Query as Q
import SlamData.Workspace.Card.Setups.Chart.Funnel.Component.State as ST
import SlamData.Workspace.Card.Setups.Dimension as D
import SlamData.Workspace.Card.Setups.DimensionMap.Component as DM
import SlamData.Workspace.Card.Setups.DimensionMap.Component.Query as DQ
import SlamData.Workspace.Card.Setups.DimensionMap.Component.State as DS
import SlamData.Workspace.Card.Setups.Inputs as BCI
import SlamData.Workspace.Card.Setups.Package.DSL as P
import SlamData.Workspace.Card.Setups.Package.Lenses as PL
import SlamData.Workspace.Card.Setups.Package.Projection as PP
import SlamData.Workspace.LevelOfDetails (LevelOfDetails(..))

type DSL = CC.InnerCardParentDSL ST.State Q.Query CS.ChildQuery CS.ChildSlot
type HTML = CC.InnerCardParentHTML Q.Query CS.ChildQuery CS.ChildSlot

package ∷ DS.Package
package = P.onPrism (M._BuildFunnel ∘ _Just) $ DS.interpret do
  category ←
    P.field PL._category PP._category
      >>= P.addAll

  value ←
    P.field PL._value PP._value
      >>= P.addSource _.value
      >>= P.isFilteredBy category

  series ←
    P.optional PL._series PP._series
      >>= P.addAll
      >>= P.isFilteredBy category
      >>= P.isFilteredBy value
      >>= P.isActiveWhen category


  pure unit

funnelBuilderComponent ∷ CC.CardOptions → CC.CardComponent
funnelBuilderComponent =
  CC.makeCardComponent (CT.ChartOptions CHT.Funnel) $ H.parentComponent
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
    , row [ renderOrder state, renderAlign state ]
    ]

renderOrder ∷ ST.State → HTML
renderOrder state =
  HH.div
    [ HP.classes [ CSS.axisLabelParam ]
    ]
    [ HH.label [ HP.classes [ CN.controlLabel ] ] [ HH.text "Order" ]
    , BCI.selectInput
        (BCI.dropdown Nothing (\l → right ∘ Q.SelectOrder l))
        (S.trySelect' state.order sortSelect)
    ]

renderAlign ∷ ST.State → HTML
renderAlign state =
  HH.div
    [ HP.classes [ CSS.axisLabelParam ]
    ]
    [ HH.label [ HP.classes [ CN.controlLabel ] ] [ HH.text "Alignment" ]
    , BCI.selectInput
        (BCI.dropdown Nothing (\l → right ∘ Q.SelectAlign l))
        (S.trySelect' state.align alignSelect)
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
      inp = M.BuildFunnel $ Just
        { category: D.topDimension
        , value: D.topDimension
        , series: Nothing
        , order: st.order
        , align: st.align
        }
    out ← H.query' CS.cpDims unit $ H.request $ DQ.Save inp
    pure $ k case join out of
      Nothing → M.BuildFunnel Nothing
      Just a → a
  CC.Load m next → do
    _ ← H.query' CS.cpDims unit $ H.action $ DQ.Load $ Just m
    for_ (m ^? M._BuildFunnel ∘ _Just) \r →
      H.modify _{ align = r.align, order = r.order }
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
  Q.SelectAlign m next → do
    case m of
      BCI.Choose (Just a) →
        H.modify $ ST._align .~ a
      _ →
        pure unit
    raiseUpdate
    pure next
  Q.SelectOrder m next → do
    case m of
      BCI.Choose (Just o) →
        H.modify $ ST._order .~ o
      _ →
        pure unit
    raiseUpdate
    pure next
  Q.HandleDims q next → do
    case q of
      DQ.Update _ → raiseUpdate
    pure next
