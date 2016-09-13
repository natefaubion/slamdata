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

module SlamData.Workspace.Card.PivotTable.Component
  ( pivotTableComponent
  ) where

import SlamData.Prelude

import Halogen as H
import Halogen.HTML.Indexed as HH

import SlamData.Monad (Slam)
import SlamData.Workspace.Card.CardType as CT
import SlamData.Workspace.Card.Component as CC
import SlamData.Workspace.Card.Model as Card
import SlamData.Workspace.Card.PivotTable.Component.Query (Query(..), QueryC)
import SlamData.Workspace.Card.PivotTable.Component.State (State, initialState, modelFromState)

type PivotTableDSL = H.ParentDSL State Void QueryC (Const Void) Slam Void

type PivotTableHTML = H.ParentHTML Void QueryC (Const Void) Slam Void

pivotTableComponent ∷ CC.CardComponent
pivotTableComponent = CC.makeCardComponent
  { cardType: CT.PivotTable
  , component: H.parentComponent
      { render
      , eval: coproduct evalCard evalTable
      , peek: Nothing
      }
  , initialState: H.parentState initialState
  , _State: CC._PivotTableState
  , _Query: CC.makeQueryPrism' CC._PivotTableQuery
  }

render ∷ State → PivotTableHTML
render st =
  HH.text "Pivot Table"


evalCard ∷ CC.CardEvalQuery ~> PivotTableDSL
evalCard = case _ of
  CC.EvalCard _ _ next →
    pure next
  CC.Activate next →
    pure next
  CC.Deactivate next →
    pure next
  CC.SetDimensions _ next → do
    pure next
  CC.Save k →
    map (k ∘ Card.PivotTable ∘ modelFromState) H.get
  CC.Load card next → do
    case card of
      Card.PivotTable model → do
        H.set model
      _ → pure unit
    pure next
  CC.ModelUpdated _ next →
    pure next
  CC.ZoomIn next →
    pure next

evalTable ∷ Query ~> PivotTableDSL
evalTable = case _ of
  Noop next →
    pure next
