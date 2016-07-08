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

module SlamData.Workspace.Card.Error.Component where

import SlamData.Prelude
import SlamData.Effects (Slam)

import Data.Lens ((^?))
import Data.Lens as Lens

import SlamData.Workspace.Card.Model as Card
import SlamData.Workspace.Card.CardType as CT
import SlamData.Workspace.Card.Component as CC
import SlamData.Workspace.Card.Error.Component.State as ECS
import SlamData.Workspace.Card.Error.Component.Query as ECQ
import SlamData.Workspace.Card.Port as Port
import SlamData.Render.CSS as CSS

import Halogen as H
import Halogen.HTML.Indexed as HH
import Halogen.HTML.Properties.Indexed as HP

type DSL = H.ComponentDSL ECS.State ECQ.QueryP Slam
type HTML = H.ComponentHTML ECQ.QueryP

comp ∷ CC.CardComponent
comp =
  CC.makeCardComponent
    { cardType: CT.ErrorCard
    , component: H.component { render, eval }
    , initialState: ECS.initialState
    , _State: CC._ErrorState
    , _Query: CC.makeQueryPrism CC._ErrorQuery
    }

render
  ∷ ECS.State
  → HTML
render st =
  case st.message of
    Just msg →
      HH.div
        [ HP.classes [ CSS.cardFailures ] ]
        [ HH.text msg ]
    Nothing →
      HH.text ""

eval ∷ ECQ.QueryP ~> DSL
eval = coproduct cardEval ECQ.initiality

cardEval ∷ CC.CardEvalQuery ~> DSL
cardEval = case _ of
  CC.EvalCard {input} output next → do
    H.modify ∘ Lens.set ECS._message $ input ^? Lens._Just ∘ Port._CardError
    pure next
  CC.Activate next →
    pure next
  CC.SetDimensions _ next →
    pure next
  CC.Save k →
    pure $ k Card.ErrorCard
  CC.Load _ next →
    pure next
  CC.ModelUpdated _ next →
    pure next
  CC.ZoomIn next →
    pure next
