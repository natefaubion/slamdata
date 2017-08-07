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

module SlamData.Workspace.Card.Setups.Chart.Candlestick.Component
  ( candlestickBuilderComponent
  ) where

import SlamData.Prelude

import Data.Lens ((^?), _Just)

import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Halogen.HTML.Events as HE

import SlamData.Workspace.Card.CardType as CT
import SlamData.Workspace.Card.CardType.ChartType as CHT
import SlamData.Workspace.Card.Component as CC
import SlamData.Workspace.Card.Eval.State as ES
import SlamData.Workspace.Card.Model as M
import SlamData.Workspace.Card.Setups.CSS as CSS
import SlamData.Workspace.Card.Setups.Chart.Candlestick.Component.ChildSlot as CS
import SlamData.Workspace.Card.Setups.Chart.Candlestick.Component.Query as Q
import SlamData.Workspace.Card.Setups.Chart.Candlestick.Component.State as ST
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
package = P.onPrism (M._BuildCandlestick ∘ _Just) $ DS.interpret do
  dimension ←
    P.field PL._dimension PP._dimension
      >>= P.addAll

  open ←
    P.field PL._open PP._open
      >>= P.addSource _.value

  close ←
    P.field PL._close PP._close
      >>= P.addSource _.value
      >>= P.isFilteredBy open

  high ←
    P.field PL._high PP._high
      >>= P.addSource _.value
      >>= P.isFilteredBy open
      >>= P.isFilteredBy close

  low ←
    P.field PL._low PP._low
      >>= P.addSource _.value
      >>= P.isFilteredBy open
      >>= P.isFilteredBy close
      >>= P.isFilteredBy high

  parallel ←
    P.optional PL._parallel PP._parallel
      >>= P.addAll
      >>= P.isFilteredBy dimension
      >>= P.isActiveWhen dimension
      >>= P.isFilteredBy open
      >>= P.isFilteredBy close
      >>= P.isFilteredBy high
      >>= P.isFilteredBy low

  pure unit

candlestickBuilderComponent ∷ CC.CardOptions → CC.CardComponent
candlestickBuilderComponent =
  CC.makeCardComponent (CT.ChartOptions CHT.Candlestick) $ H.parentComponent
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
    ]

cardEval ∷ CC.CardEvalQuery ~> DSL
cardEval = case _ of
  CC.Activate next →
    pure next
  CC.Deactivate next →
    pure next
  CC.Save k → do
    let
      inp = M.BuildCandlestick $ Just
        { dimension: D.topDimension
        , open: D.topDimension
        , close: D.topDimension
        , high: D.topDimension
        , low: D.topDimension
        , parallel: Nothing
        }
    out ← H.query' CS.cpDims unit $ H.request $ DQ.Save inp
    pure $ k case join out of
      Nothing → M.BuildCandlestick Nothing
      Just a → a
  CC.Load m next → do
    _ ← H.query' CS.cpDims unit $ H.action $ DQ.Load $ Just m
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
  Q.HandleDims q next → do
    case q of
      DQ.Update _ → raiseUpdate
    pure next
