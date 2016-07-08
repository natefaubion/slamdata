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

module SlamData.Workspace.Card.Variables.Component (variablesComponent) where

import SlamData.Prelude

import Data.List as L

import Halogen as H
import Halogen.HTML.Indexed as HH

import SlamData.Effects (Slam)
import SlamData.Workspace.Card.Variables.Component.State (State, initialState)
import SlamData.Workspace.Card.CardType as CT
import SlamData.Workspace.Card.Component as CC
import SlamData.Workspace.Card.Model as Card
import SlamData.Workspace.FormBuilder.Component as FB
import SlamData.Workspace.FormBuilder.Item.Component as Item

type HTML = H.ParentHTML (FB.StateP Slam) CC.CardEvalQuery FB.QueryP Slam Unit
type DSL = H.ParentDSL State (FB.StateP Slam) CC.CardEvalQuery FB.QueryP Slam Unit

variablesComponent ∷ H.Component CC.CardStateP CC.CardQueryP Slam
variablesComponent =
  CC.makeCardComponent
    { cardType: CT.Variables
    , component: H.parentComponent { render, eval, peek: Just (peek ∘ H.runChildF) }
    , initialState: H.parentState initialState
    , _State: CC._VariablesState
    , _Query: CC.makeQueryPrism CC._VariablesQuery
    }

render
  ∷ State
  → HTML
render _ =
  HH.slot unit \_ →
   { component : FB.formBuilderComponent
   , initialState : H.parentState FB.initialState
   }

eval ∷ CC.CardEvalQuery ~> DSL
eval = case _ of
  CC.EvalCard info output next ->
    pure next
  CC.Activate next →
    pure next
  CC.Save k →
    H.query unit (H.request (FB.GetItems ⋙ left)) <#>
      maybe [] L.fromList
        ⋙ { items: _ }
        ⋙ Card.Variables
        ⋙ k
  CC.Load card next → do
    case card of
      Card.Variables { items } →
        void ∘ H.query unit $ H.action (FB.SetItems (L.toList items) ⋙ left)
      _ → pure unit
    pure next
  CC.SetDimensions _ next →
    pure next
  CC.ModelUpdated _ next →
    pure next
  CC.ZoomIn next →
    pure next

peek ∷ ∀ x. FB.QueryP x → DSL Unit
peek = const (pure unit) ⨁ peekItemQuery ∘ H.runChildF

peekItemQuery ∷ ∀ x. Item.Query x → DSL Unit
peekItemQuery = case _ of
  Item.Update _ → CC.raiseUpdatedP CC.EvalModelUpdate
  _ → pure unit
