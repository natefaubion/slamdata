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

module SlamData.Workspace.Card.Setups.DimensionMap.Component where

import SlamData.Prelude

import Data.Lens ((^.), (.~), view)

import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.Component.Proxy as HCP

import SlamData.Monad (Slam)
import SlamData.Workspace.Card.Setups.ActionSelect.Component as AS
import SlamData.Workspace.Card.Setups.DimensionMap.Component.ChildSlot as CS
import SlamData.Workspace.Card.Setups.DimensionMap.Component.Query as Q
import SlamData.Workspace.Card.Setups.DimensionMap.Component.State as ST
import SlamData.Workspace.Card.Setups.DimensionMap.Package as DP
import SlamData.Workspace.Card.Setups.DimensionMap.Projection as Pr
import SlamData.Workspace.Card.Setups.DimensionPicker.Component as DPC
import SlamData.Workspace.Card.Setups.DimensionPicker.JCursor as DJ
import SlamData.Workspace.Card.Setups.Inputs as I
import SlamData.Workspace.Card.Setups.Transform as T
import SlamData.Workspace.Card.Setups.Transform.Numeric as N
import SlamData.Workspace.Card.Setups.Transform.Place.Component as TPC
import Utils (showJCursorTip)

type HTML = H.ParentHTML Q.Query CS.ChildQuery CS.ChildSlot Slam
type DSL = H.ParentDSL ST.State Q.Query CS.ChildQuery CS.ChildSlot Q.Message Slam

component ∷ H.Component HH.HTML Q.Query DP.Package Q.Message Slam
component =
  H.parentComponent
    { initialState: ST.initialState
    , render
    , eval
    , receiver: HE.input $ Q.SetPackage
    }

render ∷ ST.State → HTML
render state =
  HH.div [ HP.classes [ HH.ClassName "sd-axes-selector" ] ]
  $ [ HH.div [ HP.classes [ HH.ClassName "sd-axes-gap" ] ] [ ] ]
  ⊕ ( foldMap (pure ∘ renderButton state) $ state.package.allFields state.dimMap state.axes )
  ⊕ [ renderSelection state ]

renderSelection ∷ ST.State → HTML
renderSelection state = case state ^. ST._selected of
  Nothing → HH.text ""
  Just (Right tp) →
    HH.slot' CS.cpTransform unit AS.component
      { options: ST.transforms tp state
      , selection: (\a → a × a) <$> ST.getTransform tp state
      , title: "Choose transformation"
      , toLabel: \t -> { text: T.prettyPrintTransform t, icon: Nothing }
      , deselectable: Pr.isDeselectable tp
      , toSelection: case _ of
          T.Numeric (N.Floor _) → Just $ HCP.proxy TPC.transformFloor
          T.Numeric (N.Round _) → Just $ HCP.proxy TPC.transformRound
          T.Numeric (N.Ceil _) → Just $ HCP.proxy TPC.transformCeil
          _ → Nothing
      }
      (HE.input \m → Q.OnField tp ∘ Q.HandleTransformPicker m)
  Just (Left pf) →
    let
      conf =
        { title: Pr.getSelect pf
        , label: DPC.labelNode showJCursorTip
        , render: DPC.renderNode showJCursorTip
        , values: DJ.groupJCursors $ ST.selectedCursors state
        , isSelectable: DPC.isLeafPath
        }
    in
      HH.slot'
        CS.cpPicker
        unit
        (DPC.picker conf)
        unit
        (HE.input \m → Q.OnField pf ∘ Q.HandleDPMessage m)


renderButton ∷ ST.State → Pr.Projection → HTML
renderButton state fld =
  HH.div [ HP.classes [ HH.ClassName "chart-configure-form" ] ]
  [ I.dimensionButton
    { configurable: true
    , formattable: false
    , dimension: sequence $ ST.getSelected fld state
    , showLabel: absurd
    , showDefaultLabel: const $ Pr.getLabel fld
    , showValue: ST.showValue fld
    , onLabelChange: HE.input \l → Q.OnField fld ∘ Q.LabelChanged l
    , onDismiss: HE.input_ $ Q.OnField fld ∘ Q.Dismiss
    , onConfigure: HE.input_ $ Q.OnField fld ∘ Q.Configure
    , onSetupFormatting: const Nothing
    , onClick: HE.input_ $ Q.OnField fld ∘ Q.Select
    , onMouseDown: const Nothing
    , onLabelClick: const Nothing
    , disabled: ST.isDisabled fld state
    , dismissable: isJust $ ST.getSelected fld state
    , labelless: Pr.isLabelless fld
    } ]

eval ∷ Q.Query ~> DSL
eval = case _ of
  Q.SetPackage p next → do
    H.modify _{ package = p, selected = Nothing }
    pure next
  Q.Save k → do
    H.gets $ k ∘ view ST._dimMap
  Q.Load m next → do
    H.modify $ ST._dimMap .~ m
    pure next
  Q.SetAxes ax next → do
    H.modify $ ST._axes .~ ax
    pure next
  Q.OnField fld fldQuery → case fldQuery of
    Q.Select next → do
      H.modify $ ST.select fld
      pure next
    Q.Configure next → do
      H.modify $ ST.configure fld
      pure next
    Q.Dismiss next → do
      H.modify $ ST.clear fld
      sendUpdatedDimensionMap
      pure next
    Q.LabelChanged str next → do
      H.modify $ ST.setLabel fld str
      sendUpdatedDimensionMap
      pure next
    Q.HandleDPMessage m next → case m of
      DPC.Dismiss → do
        H.modify ST.deselect
        pure next
      DPC.Confirm value → do
        H.modify
          $ ( ST.setValue fld $ DJ.flattenJCursors value )
          ∘ ( ST.deselect )
        sendUpdatedDimensionMap
        pure next
    Q.HandleTransformPicker msg next → do
      case msg of
        AS.Dismiss →
          H.modify ST.deselect
        AS.Confirm mbt → do
          H.modify
            $ ST.deselect
            ∘ ST.setTransform fld mbt
          sendUpdatedDimensionMap
      pure next

sendUpdatedDimensionMap ∷ DSL Unit
sendUpdatedDimensionMap = do
  st ← H.get
  H.raise $ Q.Update st.dimMap
