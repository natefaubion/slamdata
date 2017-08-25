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

module SlamData.Workspace.Card.Setups.VizPicker.Component where

import SlamData.Prelude

import CSS as CSS

import Data.Equivalence (Equivalence(..))
import Halogen as H
import Halogen.HTML as HH
import Halogen.Component.ChildPath as CP
import Halogen.HTML.CSS as HCSS
import Halogen.HTML.Events as HE
import SlamData.ActionList.Component as AL
import SlamData.ActionList.Action as AA
import SlamData.ActionList.Filter.Component as ALF
import SlamData.Monad (Slam)
import SlamData.Workspace.Card.CardType.VizType as VT
import SlamData.Workspace.Card.Setups.Axis as Ax
import SlamData.Workspace.Card.Setups.DimensionMap.Package as DP
import SlamData.Workspace.Card.Setups.DimensionMap.DSL as DSL

type ChildSlot = Unit ⊹ Unit ⊹ Void

type ChildQuery = AL.Query Action ⨁ ALF.Query ⨁ Const Void

data Action
  = Set VT.VizType
  | Explain VT.VizType

derive instance eqVizPickerAction ∷ Eq Action

fitsRequirement ∷ Ax.Axes → VT.VizType → Boolean
fitsRequirement axes vt =
  sizes.value >= conf.value
  ∧ sizes.category >= conf.category
  ∧ sizes.date >= conf.date
  ∧ sizes.time >= conf.time
  ∧ sizes.datetime >= conf.datetime
  ∧ total >= conf.total
  ∧ nonMeasure >= conf.nonMeasure
  where
  conf = maybe DSL.noneRequirements _.axesRequirements $ DP.lookup vt
  sizes = Ax.axesSizes axes
  nonMeasure = sizes.category + sizes.date + sizes.time + sizes.datetime
  total = nonMeasure + sizes.value

axesActions ∷ Ax.Axes → Array Action
axesActions axes =
  VT.all <#> \vt →
  if fitsRequirement axes vt
  then Set vt
  else Explain vt

isSet ∷ Action → Boolean
isSet = case _ of
  Set _ → true
  _ → false

vizType ∷ Action → VT.VizType
vizType = case _ of
  Set vt → vt
  Explain vt → vt

cpList
  ∷ CP.ChildPath
      (AL.Query Action) ChildQuery
      Unit ChildSlot
cpList = CP.cp1

cpFilter
  ∷ CP.ChildPath
      ALF.Query ChildQuery
      Unit ChildSlot
cpFilter = CP.cp2

data Query a
  = HandleFilter ALF.Message a
  | HandleAction (AL.Message Action) a
  | UpdateAxes Ax.Axes a

data Message
  = SetVizType VT.VizType
  | ExplainNotWorking VT.VizType
  | ActionListUpdated

type State =
  { }

type HTML =
  H.ParentHTML Query ChildQuery ChildSlot Slam
type DSL =
  H.ParentDSL State Query ChildQuery ChildSlot Message Slam

component ∷ H.Component HH.HTML Query Unit Message Slam
component = H.parentComponent
  { initialState: const { }
  , render
  , eval
  , receiver: const Nothing
  }

render ∷ State → HTML
render state =
  HH.div
  [ HCSS.style do
       CSS.width $ CSS.pct 100.0
       CSS.height $ CSS.pct 100.0
  ]
  [ HH.slot' cpFilter unit ALF.component "Filter visualizations"
      $ HE.input HandleFilter
  , HH.slot' cpList unit (AL.actionListComp' (Equivalence eq)  AL.defaultConf [ ]) unit
      $ HE.input HandleAction
  ]

description ∷ Action → String
description = case _ of
  Set vt → "Set visualization type to " ⊕ (VT.name $ expand vt)
  Explain vt → "Find why " ⊕ (VT.name $ expand vt) ⊕ " is unavailable for this data set"


toAction ∷ Action → AA.Action Action
toAction vt = AA.mkDo
  { name: VT.name $ expand $ vizType vt
  , icon: Just $ VT.icon $ expand $ vizType vt
  , highlighted: isSet vt
  , disabled: false
  , description: description vt
  , action: vt
  }

eval ∷ Query ~> DSL
eval = case _ of
  HandleFilter m next → case m of
    ALF.FilterChanged str → do
      _ ← H.query' cpList unit
        $ H.action
        $ AL.UpdateFilter str
      pure next
  HandleAction m next → case m of
    AL.Selected a → do
      H.raise $ case a of
        Set vt → SetVizType vt
        Explain vt → ExplainNotWorking vt
      pure next
  UpdateAxes axes next → do
    _ ← H.query' cpList unit
      $ H.action
      $ AL.UpdateActions
      $ map toAction
      $ axesActions axes
    pure next
