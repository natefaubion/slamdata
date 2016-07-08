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

module SlamData.Workspace.Card.Pending.Component where

import SlamData.Prelude
import SlamData.Effects (Slam)

import SlamData.Workspace.Card.CardType as CT
import SlamData.Workspace.Card.Component as CC
import SlamData.Workspace.Card.Model as Card
import SlamData.Workspace.Card.Pending.Component.Query as PCQ
import SlamData.Workspace.Card.Pending.Component.State as PCS

import Halogen as H
import Halogen.Themes.Bootstrap3 as B
import Halogen.HTML.Indexed as HH
import Halogen.HTML.Properties.Indexed as HP

type DSL = H.ComponentDSL PCS.State PCQ.QueryP Slam
type HTML = H.ComponentHTML PCQ.QueryP

comp ∷ CC.CardComponent
comp =
  CC.makeCardComponent
    { cardType: CT.PendingCard
    , component: H.component { render, eval }
    , initialState: PCS.initialState
    , _State: CC._PendingState
    , _Query: CC.makeQueryPrism CC._PendingQuery
    }

render
  ∷ PCS.State
  → HTML
render st =
  HH.div [ HP.classes [ B.alert, B.alertInfo ] ]
    [ HH.img [ HP.src "img/blue-spin.svg" ]
    , HH.text st.message
    ]

eval ∷ PCQ.QueryP ~> DSL
eval = coproduct cardEval PCQ.initiality

cardEval ∷ CC.CardEvalQuery ~> DSL
cardEval q =
  case q of
    CC.EvalCard _ _ next → do
      pure next
    CC.Activate next →
      pure next
    CC.Save k →
      pure $ k Card.PendingCard
    CC.Load _ next →
      pure next
    CC.SetDimensions _ next →
      pure next
    CC.ModelUpdated _ next →
      pure next
    CC.ZoomIn next →
      pure next
