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

import Data.Lens ((^.), (.~), (%~))

import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP

import SlamData.Monad (Slam)
import SlamData.Workspace.Card.Setups.ActionSelect.Component as AS
import SlamData.Workspace.Card.Setups.DimensionMap.Component.ChildSlot as CS
import SlamData.Workspace.Card.Setups.DimensionMap.Component.Query as Q
import SlamData.Workspace.Card.Setups.DimensionMap.Component.State as ST
import SlamData.Workspace.Card.Setups.DimensionMap.Defaults as DMD
import SlamData.Workspace.Card.Setups.DimensionPicker.Component as DPC
import SlamData.Workspace.Card.Setups.DimensionPicker.JCursor as DJ
import SlamData.Workspace.Card.Setups.Package.DSL as T
import SlamData.Workspace.Card.Setups.Inputs as I
import SlamData.Workspace.Card.Setups.Transform as Tr
import Utils (showJCursorTip)

type HTML = H.ParentHTML Q.Query CS.ChildQuery CS.ChildSlot Slam
type DSL = H.ParentDSL ST.State Q.Query CS.ChildQuery CS.ChildSlot Q.Message Slam

component ∷ ST.Package → H.Component HH.HTML Q.Query Unit Q.Message Slam
component package =
  H.parentComponent
    { initialState: ST.initialState
    , render: render package
    , eval: eval package
    , receiver: const Nothing
    }

render ∷ ST.Package → ST.State → HTML
render pack state =
  HH.div [ HP.classes [ HH.ClassName "sd-axes-selector" ] ]
  $ ( foldMap (pure ∘ renderButton pack state) $ pack.allFields state.dimMap state.axes )
  ⊕ [ renderSelection pack state ]

renderSelection ∷ ST.Package → ST.State → HTML
renderSelection pack state = case state ^. ST._selected of
  Nothing → HH.text ""
  Just (Right tp) →
    HH.slot' CS.cpTransform unit AS.component
      { options: ST.transforms state
      , selection: (\a → a × a) <$> ST.getTransform tp state
      , title: "Choose transformation"
      , toLabel: \t -> { text: Tr.prettyPrintTransform t, icon: Nothing }
      , deselectable: (DMD.getDefaults tp).deselectable
      , toSelection: const Nothing
      }
      (HE.input \m → Q.OnField tp ∘ Q.HandleTransformPicker m)
  Just (Left pf) →
    let
      conf =
        { title: ST.chooseLabel pf
        , label: DPC.labelNode showJCursorTip
        , render: DPC.renderNode showJCursorTip
        , values: DJ.groupJCursors $ ST.selectedCursors pack state
        , isSelectable: DPC.isLeafPath
        }
    in
      HH.slot'
        CS.cpPicker
        unit
        (DPC.picker conf)
        unit
        (HE.input \m → Q.OnField pf ∘ Q.HandleDPMessage m)

renderButton ∷ ST.Package → ST.State → T.Projection → HTML
renderButton pack state fld =
  HH.div [ HP.classes [ HH.ClassName "chart-configure-form" ] ]
  [ I.dimensionButton
    { configurable: ST.isConfigurable fld pack state
    , formattable: false
    , dimension: sequence $ ST.getSelected fld state
    , showLabel: absurd
    , showDefaultLabel: ST.showDefaultLabel fld
    , showValue: ST.showValue fld
    , onLabelChange: HE.input \l → Q.OnField fld ∘ Q.LabelChanged l
    , onDismiss: HE.input_ $ Q.OnField fld ∘ Q.Dismiss
    , onConfigure: HE.input_ $ Q.OnField fld ∘ Q.Configure
    , onSetupFormatting: const Nothing
    , onClick: HE.input_ $ Q.OnField fld ∘ Q.Select
    , onMouseDown: const Nothing
    , onLabelClick: const Nothing
    , disabled: ST.isDisabled fld pack state
    , dismissable: isJust $ ST.getSelected fld state
    } ]

eval ∷ ST.Package → Q.Query ~> DSL
eval package = case _ of
  Q.Save m k → do
    st ← H.get
    pure $ k $ package.save st.dimMap m
  Q.Load m next → do
    st ← H.get
    H.modify $ ST._dimMap %~ package.load m
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
      H.raise $ Q.Update Nothing
      pure next
    Q.LabelChanged str next → do
      H.modify $ ST.setLabel fld str
      H.raise $ Q.Update Nothing
      pure next
    Q.HandleDPMessage m next → case m of
      DPC.Dismiss → do
        H.modify ST.deselect
        pure next
      DPC.Confirm value → do
        H.modify
          $ ( ST.setValue fld $ DJ.flattenJCursors value )
          ∘ ( ST.deselect )
        H.raise $ Q.Update $ Just fld
        pure next
    Q.HandleTransformPicker msg next → do
      case msg of
        AS.Dismiss →
          H.modify ST.deselect
        AS.Confirm mbt → do
          H.modify
            $ ST.deselect
            ∘ ST.setTransform fld mbt
          H.raise $ Q.Update $ Just fld
      pure next
