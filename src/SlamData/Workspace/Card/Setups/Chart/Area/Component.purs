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

module SlamData.Workspace.Card.Setups.Chart.Area.Component
  ( areaBuilderComponent
  ) where

import SlamData.Prelude

import Data.Lens (_Just, (.~), (^?), (%~))

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
import SlamData.Workspace.Card.Setups.Chart.Area.Component.Query as Q
import SlamData.Workspace.Card.Setups.Chart.Area.Component.State as ST
import SlamData.Workspace.Card.Setups.Chart.Area.Component.ChildSlot as CS
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
package = P.onPrism (M._BuildArea ∘ _Just) $ DS.interpret do
  dimension ←
    P.field PL._dimension PP._dimension
      >>= P.addAll
  value ←
    P.field PL._value PP._value
      >>= P.isFilteredBy dimension
      >>= P.addSource _.value

  series ←
    P.optional PL._series PP._series
      >>= P.addAll
      >>= P.isFilteredBy value
      >>= P.isFilteredBy dimension
      >>= P.isActiveWhen dimension

  pure unit

areaBuilderComponent ∷ CC.CardOptions → CC.CardComponent
areaBuilderComponent =
  CC.makeCardComponent (CT.ChartOptions CHT.Area) $ H.parentComponent
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
    , row [ renderIsStacked state, renderIsSmooth state ]
    , HH.hr_
    , row [ renderAxisLabelAngle state, renderSize state ]
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
        , HE.onValueChange $ HE.input \l → right ∘ Q.SetAxisLabelAngle l
        ]
    ]

renderIsStacked ∷ ST.State → HTML
renderIsStacked state =
  HH.div
    [ HP.classes [ CSS.axisLabelParam ]
    ]
    [ HH.label [ HP.classes [ CN.controlLabel ] ]
        [ HH.input
            [ HP.type_ HP.InputCheckbox
            , HP.checked state.isStacked
            , ARIA.label "Stacked"
            , HE.onChecked $ HE.input_ $ right ∘ Q.ToggleStacked
            ]
        , HH.text "Stacked"
        ]
    ]

renderIsSmooth ∷ ST.State → HTML
renderIsSmooth state =
  HH.div
    [ HP.classes [ CSS.axisLabelParam ]
    ]
    [ HH.label [ HP.classes [ CN.controlLabel ] ]
        [ HH.input
            [ HP.type_ HP.InputCheckbox
            , HP.checked state.isSmooth
            , ARIA.label "Smooth"
            , HE.onChecked $ HE.input_ $ right ∘ Q.ToggleSmooth
            ]
        , HH.text "Smooth"
        ]
    ]

renderSize ∷ ST.State → HTML
renderSize state =
  HH.div
    [ HP.classes [ CSS.axisLabelParam ]
    ]
    [ HH.label
        [ HP.classes [ CN.controlLabel ] ]
        [ HH.text "Size" ]
    , HH.input
        [ HP.classes [ CN.formControl ]
        , HP.value $ show state.size
        , ARIA.label "Size"
        , HE.onValueChange $ HE.input (\s → right ∘ Q.SetSize s)
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
      inp = M.BuildArea $ Just
        { axisLabelAngle: st.axisLabelAngle
        , isStacked: st.isStacked
        , isSmooth: st.isSmooth
        , size: st.size
        , dimension: D.topDimension
        , value: D.topDimension
        , series: Nothing
        }
    out ← H.query' CS.cpDims unit $ H.request $ DQ.Save inp
    pure $ k case join out of
      Nothing → M.BuildArea Nothing
      Just a → a
  CC.Load m next → do
    _ ← H.query' CS.cpDims unit $ H.action $ DQ.Load $ Just m
    for_ (m ^? M._BuildArea ∘ _Just) \r →
      H.modify _{ isStacked = r.isStacked, isSmooth = r.isSmooth, size = r.size }
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
  Q.SetSize str next → do
    let fl = readFloat str
    unless (isNaN fl)
      $ H.modify _{ size = fl }
    raiseUpdate
    pure next

  Q.SetAxisLabelAngle str next → do
    let fl = readFloat str
    unless (isNaN fl) do
      H.modify $ ST._axisLabelAngle .~ fl
      raiseUpdate
    pure next
  Q.ToggleSmooth next → do
    H.modify $ ST._isSmooth %~ not
    raiseUpdate
    pure next
  Q.ToggleStacked next → do
    H.modify $ ST._isStacked %~ not
    raiseUpdate
    pure next
  Q.HandleDims q next → do
    case q of
      DQ.Update _ → raiseUpdate
    pure next
