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

module SlamData.Workspace.Card.Setups.Chart.Heatmap.Component
  ( heatmapBuilderComponent
  ) where

import SlamData.Prelude

import Data.Lens ((^?), _Just)

import Global (readFloat, isNaN)

import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.HTML.Properties.ARIA as ARIA

import SlamData.Form.Select as S
import SlamData.Render.ClassName as CN
import SlamData.Render.Common (row)
import SlamData.Workspace.Card.CardType as CT
import SlamData.Workspace.Card.CardType.ChartType as CHT
import SlamData.Workspace.Card.Component as CC
import SlamData.Workspace.Card.Eval.State as ES
import SlamData.Workspace.Card.Model as M
import SlamData.Workspace.Card.Setups.CSS as CSS
import SlamData.Workspace.Card.Setups.Chart.ColorScheme (colorSchemeSelect)
import SlamData.Workspace.Card.Setups.Chart.Heatmap.Component.ChildSlot as CS
import SlamData.Workspace.Card.Setups.Chart.Heatmap.Component.Query as Q
import SlamData.Workspace.Card.Setups.Chart.Heatmap.Component.State as ST
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
package = P.onPrism (M._BuildHeatmap ∘ _Just) $ DS.interpret do
  abscissa ←
    P.field PL._abscissa PP._abscissa
      >>= P.addAll

  ordinate ←
    P.field PL._ordinate PP._ordinate
      >>= P.addAll
      >>= P.isFilteredBy abscissa

  value ←
    P.field PL._value PP._value
      >>= P.addSource _.value
      >>= P.isFilteredBy abscissa
      >>= P.isFilteredBy ordinate

  series ←
    P.optional PL._series PP._series
      >>= P.addAll
      >>= P.isFilteredBy value
      >>= P.isActiveWhen value
      >>= P.isFilteredBy abscissa
      >>= P.isFilteredBy ordinate
      >>= P.isActiveWhen abscissa
      >>= P.isActiveWhen ordinate

  pure unit

heatmapBuilderComponent ∷ CC.CardOptions → CC.CardComponent
heatmapBuilderComponent =
  CC.makeCardComponent (CT.ChartOptions CHT.Heatmap) $ H.parentComponent
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
    , row [ renderColorScheme state, renderIsReversedScheme state ]
    , HH.hr_
    , row [ renderMinVal state, renderMaxVal state ]
    ]

renderMinVal ∷ ST.State → HTML
renderMinVal state =
  HH.div
    [ HP.classes [ CSS.axisLabelParam ]
    ]
    [ HH.label [ HP.classes [ CN.controlLabel ] ] [ HH.text "Minimum value" ]
    , HH.input
        [ HP.classes [ CN.formControl ]
        , HP.value $ show $ state.minValue
        , ARIA.label "Minimum value"
        , HE.onValueChange $ HE.input (\s → right ∘ Q.SetMinValue s)
        ]
    ]

renderMaxVal ∷ ST.State → HTML
renderMaxVal state =
  HH.div
    [ HP.classes [ CSS.axisLabelParam ]
    ]
    [ HH.label [ HP.classes [ CN.controlLabel ] ] [ HH.text "Maximum value" ]
    , HH.input
        [ HP.classes [ CN.formControl ]
        , HP.value $ show $ state.maxValue
        , ARIA.label "Maximum value"
        , HE.onValueChange $ HE.input (\s → right ∘ Q.SetMaxValue s)
        ]
    ]


renderIsReversedScheme ∷ ST.State → HTML
renderIsReversedScheme state =
  HH.div
    [ HP.classes [ CSS.axisLabelParam ]
    ]
    [ HH.label [ HP.classes [ CN.controlLabel ] ]
        [ HH.input
            [ HP.type_ HP.InputCheckbox
            , HP.checked state.isColorSchemeReversed
            , ARIA.label "Reverse color scheme"
            , HE.onChecked $ HE.input_ (right ∘ Q.ToggleReversedScheme)
            ]
        , HH.text "Reversed"
        ]
    ]

renderColorScheme ∷ ST.State → HTML
renderColorScheme state =
  HH.div
    [ HP.classes [ CSS.axisLabelParam ]
    ]
    [ HH.label [ HP.classes [ CN.controlLabel ] ] [ HH.text "Color scheme" ]
    , BCI.selectInput
        (BCI.dropdown (Just "Color scheme") (\l → right ∘ Q.SelectColorScheme l))
        (S.trySelect' state.colorScheme colorSchemeSelect)
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
      inp = M.BuildHeatmap $ Just
        { abscissa: D.topDimension
        , ordinate: D.topDimension
        , value: D.topDimension
        , series: Nothing
        , colorScheme: st.colorScheme
        , isColorSchemeReversed: st.isColorSchemeReversed
        , minValue: st.minValue
        , maxValue: st.maxValue
        }
    out ← H.query' CS.cpDims unit $ H.request $ DQ.Save inp
    pure $ k case join out of
      Nothing → M.BuildHeatmap Nothing
      Just a → a
  CC.Load m next → do
    _ ← H.query' CS.cpDims unit $ H.action $ DQ.Load $ Just m
    for_ (m ^? M._BuildHeatmap ∘ _Just) \r →
      H.modify _{ colorScheme = r.colorScheme
                , isColorSchemeReversed = r.isColorSchemeReversed
                , minValue = r.minValue
                , maxValue = r.maxValue
                }
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
  Q.SelectColorScheme m next → do
    case m of
      BCI.Choose (Just a) →
        H.modify _{ colorScheme = a }
      _ →
        pure unit
    raiseUpdate
    pure next
  Q.SetMinValue str next → do
    let fl = readFloat str
    unless (isNaN fl) do
      H.modify _{minValue = fl}
      raiseUpdate
    pure next
  Q.SetMaxValue str next → do
    let fl = readFloat str
    unless (isNaN fl) do
      H.modify _{maxValue = fl}
      raiseUpdate
    pure next
  Q.ToggleReversedScheme next → do
    H.modify \x → x{isColorSchemeReversed = not x.isColorSchemeReversed}
    raiseUpdate
    pure next
  Q.HandleDims q next → do
    case q of
      DQ.Update _ → raiseUpdate
    pure next
