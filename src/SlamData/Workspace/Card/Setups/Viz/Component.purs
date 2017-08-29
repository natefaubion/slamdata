{-
Copyright 2017 SlamData, Inc.

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

module SlamData.Workspace.Card.Setups.Viz.Component
  ( component
  ) where

import SlamData.Prelude

import CSS as CSS

import Data.Lens ((^?))
import Data.ListMap as LM
import Data.Variant as V
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.CSS as HCSS
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.HTML.Properties.ARIA as ARIA
import SlamData.Render.Icon as I
import SlamData.Wiring as W
import SlamData.Workspace.Card.CardType as CT
import SlamData.Workspace.Card.CardType.VizType as VCT
import SlamData.Workspace.Card.Component as CC
import SlamData.Workspace.Card.Eval.State as ES
import SlamData.Workspace.Card.Model as M
import SlamData.Workspace.Card.Setups.Auxiliary as Aux
import SlamData.Workspace.Card.Setups.DimensionMap.Component as DM
import SlamData.Workspace.Dialog.Component as Dialog
import SlamData.Workspace.Card.Setups.DimensionMap.Component.Query as DQ
import SlamData.Workspace.Card.Setups.DimensionMap.Package as DP
import SlamData.Workspace.Card.Setups.PivotTable.Component as PT
import SlamData.Workspace.Card.Setups.PivotTable.Component.Query as PQ
import SlamData.Workspace.Card.Setups.Viz.Component.ChildSlot as CS
import SlamData.Workspace.Card.Setups.Viz.Component.Query as Q
import SlamData.Workspace.Card.Setups.Viz.Component.State as ST
import SlamData.Workspace.Card.Setups.VizPicker.Component as VT
import SlamData.Workspace.LevelOfDetails (LevelOfDetails(..))

type DSL = CC.InnerCardParentDSL ST.State Q.Query CS.ChildQuery CS.ChildSlot
type HTML = CC.InnerCardParentHTML Q.Query CS.ChildQuery CS.ChildSlot

lm ∷ ∀ a. LM.Module VCT.VizType a
lm = LM.openModule eq

component ∷ CC.CardOptions → CC.CardComponent
component options =
  CC.makeCardComponent CT.setupViz (H.parentComponent
    { render: render options
    , eval: cardEval ⨁ setupEval
    , receiver: const Nothing
    , initialState: const ST.initialState
    }) options

render ∷ CC.CardOptions → ST.State → HTML
render options state =
  HH.div
  [ HCSS.style $ CSS.width (CSS.pct 100.0) *> CSS.height (CSS.pct 100.0) ]
  $ ( if state.vizTypePickerExpanded
      then [ picker ]
      else [ button ] <> dims )
  ⊕ aux
  ⊕ pivotOptions
  where

  pivotOptions ∷ Array HTML
  pivotOptions =
    guard (not state.vizTypePickerExpanded && state.vizType == CT.pivot) $>
      HH.slot'
        CS.cpPivot
        unit
        (PT.component options)
        unit
        (HE.input \e → right ∘ Q.HandlePivotTable e)


  button =
    HH.button
      [ HE.onClick $ HE.input_ (right ∘ Q.ToggleVizPicker)
      , HP.class_ (HH.ClassName "sd-viztype-button")
      , ARIA.label "Select visualization type"
      ]
      [ I.unIconHTML (VCT.icon state.vizType)
      , HH.p_ [ HH.text (VCT.name state.vizType) ]
      ]

  picker =
    HH.slot'
      CS.cpPicker
      unit
      VT.component
      unit
      (HE.input \e → right ∘ Q.HandlePicker e)

  dims = foldMap pure do
    package ← lm.lookup state.vizType DP.packages
    pure $
      HH.slot'
        CS.cpDims
        unit
        DM.component
        package
        (HE.input \e → right ∘ Q.HandleDims e)

  aux = foldMap pure do
    auxState ← lm.lookup state.vizType state.auxes
    comp ← Aux.vizTypeAux state.vizType
    pure $
      HH.div_
        [ HH.slot'
            CS.cpAux
            unit
            comp
            auxState
            (HE.input \e → right ∘ Q.HandleAux e)
        ]

cardEval ∷ CC.CardEvalQuery ~> DSL
cardEval = case _ of
  CC.Activate next →
    pure next
  CC.Deactivate next →
    pure next
  CC.Save k → do
    st ← H.get
    pure $ k $ M.SetupViz
      { dimMaps: st.dimMaps
      , vizType: st.vizType
      , auxes: st.auxes
      }
  CC.Load m next → do
    case m of
      M.SetupViz r → do
        H.modify _
          { dimMaps = r.dimMaps
          , vizType = r.vizType
          , auxes = r.auxes
          }
        for_ (lm.lookup r.vizType r.dimMaps) \dimMap → do
          void $ H.query' CS.cpDims unit $ H.action $ DQ.Load dimMap
          H.raise CC.modelUpdate
        for_ (lm.lookup CT.pivot r.auxes >>= V.prj CT._pivot) \aux → do
          void $ H.query' CS.cpPivot unit $ H.action $ PQ.Load aux
          H.raise CC.modelUpdate
      _ → pure unit
    pure next
  CC.ReceiveInput _ _ next →
    pure next
  CC.ReceiveOutput _ _ next →
    pure next
  CC.ReceiveState evalState next → do
    for_ (evalState ^? ES._Axes) \axes → do
      H.modify _{ axes = Just axes }
      _ ← H.query' CS.cpPicker unit $ H.action $ VT.UpdateAxes axes
      _ ← H.query' CS.cpDims unit $ H.action $ DQ.SetAxes axes
      _ ← H.query' CS.cpPivot unit $ H.action $ PQ.SetAxes axes
      pure unit
    pure next
  CC.ReceiveDimensions _ reply →
    pure $ reply High

setupEval ∷ Q.Query ~> DSL
setupEval = case _ of
  Q.HandlePicker vt next → do
    st ← H.get
    case vt of
      VT.SetVizType v → do
        for_ (lm.lookup v st.dimMaps) \dimMap →
          void $ H.query' CS.cpDims unit $ H.action $ DQ.Load dimMap
        H.modify _{ vizType = v }
        H.modify _{ vizTypePickerExpanded = false }
        H.raise CC.modelUpdate
      VT.ExplainNotWorking vizType → do
        for_ st.axes \axes →
          W.showDialog $ Dialog.VizUnavailable {vizType, axes}
      _ → pure unit
    pure next
  Q.HandleDims msg next → do
    case msg of
      DQ.Update dimMap →
        H.modify \st → st{ dimMaps = lm.insert st.vizType dimMap st.dimMaps }
    H.raise CC.modelUpdate
    pure next
  Q.ToggleVizPicker next → do
    state ← H.get
    H.modify _{ vizTypePickerExpanded = true
              , vizType = (CT.pivot ∷ VCT.VizType)
              }
    for_ state.axes \axes →
      void $ H.query' CS.cpPicker unit $ H.action $ VT.UpdateAxes axes
    pure next
  Q.HandleAux auxState next → do
    H.modify \st → st { auxes = lm.insert st.vizType auxState st.auxes }
    H.raise CC.modelUpdate
    pure next
  Q.HandlePivotTable m next → do
    H.modify \st → st { auxes = lm.insert CT.pivot (V.inj CT._pivot m) st.auxes }
    H.raise CC.modelUpdate
    pure next
