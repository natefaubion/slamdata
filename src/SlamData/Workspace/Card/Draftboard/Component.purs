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

module SlamData.Workspace.Card.Draftboard.Component
  ( draftboardComponent
  , module SlamData.Workspace.Card.Draftboard.Component.Query
  , module SlamData.Workspace.Card.Draftboard.Component.State
  ) where

import SlamData.Prelude

import Halogen as H
import Halogen.HTML.Indexed as HH

import SlamData.Effects (Slam)
import SlamData.Workspace.Card.Draftboard.Component.Query (Query(..), QueryP)
import SlamData.Workspace.Card.Draftboard.Component.State (State, DeckPosition, initialState, _decks, _zoomed, encode, decode)
import SlamData.Workspace.Card.CardType as Ct
import SlamData.Workspace.Card.Common.EvalQuery as Ceq
import SlamData.Workspace.Card.Component as Cp

type DraftboardDSL = H.ComponentDSL State QueryP Slam

draftboardComponent ∷ Cp.CardComponent
draftboardComponent = Cp.makeCardComponent
  { cardType: Ct.Draftboard
  , component: H.component { render, eval: coproduct evalCard evalBoard }
  , initialState
  , _State: Cp._DraftboardState
  , _Query: Cp.makeQueryPrism Cp._DraftboardQuery
  }

render ∷ State → H.ComponentHTML QueryP
render _ = HH.text ""

evalCard ∷ Natural Ceq.CardEvalQuery (H.ComponentDSL State QueryP Slam)
evalCard (Ceq.EvalCard input k) = pure $ k { output: Nothing, messages: [] }
evalCard (Ceq.SetupCard info next) = pure next
evalCard (Ceq.NotifyRunCard next) = pure next
evalCard (Ceq.NotifyStopCard next) = pure next
evalCard (Ceq.SetCanceler canceler next) = pure next
evalCard (Ceq.Save k) = map (k ∘ encode) H.get
evalCard (Ceq.Load json next) = for_ (decode json) H.set $> next

evalBoard ∷ Natural Query DraftboardDSL
evalBoard (StartDragging next) = pure next
evalBoard (StopDragging next) = pure next
evalBoard (AddDeck next) = pure next
