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

import Data.Array as Array
import Data.Map as Map

import CSS as CSS

import Halogen as H
import Halogen.Component.Opaque.Unsafe (opaqueState, opaqueQuery)
import Halogen.HTML.CSS.Indexed as HC
import Halogen.HTML.Events.Indexed as HE
import Halogen.HTML.Indexed as HH
import Halogen.HTML.Properties.Indexed as HP

import SlamData.Config as Config
import SlamData.Effects (Slam)
import SlamData.Render.CSS as RC
import SlamData.Workspace.Card.Draftboard.Component.Query (Query(..), QueryP, QueryC)
import SlamData.Workspace.Card.Draftboard.Component.State (State, DeckPosition, initialState, _decks, _zoomed, encode, decode)
import SlamData.Workspace.Card.CardType as Ct
import SlamData.Workspace.Card.Common (CardOptions)
import SlamData.Workspace.Card.Common.EvalQuery as Ceq
import SlamData.Workspace.Card.Component as Cp
import SlamData.Workspace.Deck.Component.Query as DCQ
import SlamData.Workspace.Deck.Component.State as DCS
import SlamData.Workspace.Deck.DeckId (DeckId, deckIdToString)

type DraftboardDSL = H.ParentDSL State DCS.StateP QueryC DCQ.QueryP Slam DeckId

type DraftboardHTML = H.ParentHTML DCS.StateP QueryC DCQ.QueryP Slam DeckId

draftboardComponent ∷ CardOptions → Cp.CardComponent
draftboardComponent opts = Cp.makeCardComponent
  { cardType: Ct.Draftboard
  , component: H.parentComponent
      { render: render opts
      , eval: coproduct evalCard evalBoard
      , peek: Nothing
      }
  , initialState: H.parentState $ initialState { path = opts.path }
  , _State: Cp._DraftboardState
  , _Query: Cp.makeQueryPrism' Cp._DraftboardQuery
  }

render ∷ CardOptions → State → DraftboardHTML
render opts state =
  HH.div [ HP.classes [ RC.board ] ]
    $ map renderDeck (foldl Array.snoc [] $ Map.toList state.decks)

  where

  renderDeck (Tuple deckId rect) =
    HH.div
      [ HP.key $ deckIdToString deckId
      , HC.style do
          CSS.position CSS.absolute
          CSS.top $ CSS.px $ rect.x * Config.gridPx
          CSS.left $ CSS.px $ rect.y * Config.gridPx
          CSS.width $ CSS.px $ rect.width * Config.gridPx
          CSS.height $ CSS.px $ rect.height * Config.gridPx
      ]
      [ HH.slot deckId $ mkDeckComponent deckId ]

  mkDeckComponent id _ =
    { component: opts.deckComponent
    , initialState: opaqueState $ DCS.initialDeck
    }

evalCard ∷ Natural Ceq.CardEvalQuery DraftboardDSL
evalCard (Ceq.EvalCard input k) = pure $ k { output: Nothing, messages: [] }
evalCard (Ceq.SetupCard info next) = pure next
evalCard (Ceq.NotifyRunCard next) = pure next
evalCard (Ceq.NotifyStopCard next) = pure next
evalCard (Ceq.SetCanceler canceler next) = pure next
evalCard (Ceq.Save k) = map (k ∘ encode) H.get
evalCard (Ceq.Load json next) = do
  for_ (decode json) \model → do
    H.modify _ { decks = model.decks }
    loadDecks
  pure next

evalBoard ∷ Natural Query DraftboardDSL
evalBoard (StartDragging next) = pure next
evalBoard (StopDragging next) = pure next
evalBoard (AddDeck next) = pure next

loadDecks ∷ DraftboardDSL Unit
loadDecks = void $
  H.gets _.path >>= traverse \path →
    H.gets (Map.keys ∘ _.decks) >>= traverse \deckId →
      H.query deckId
        $ opaqueQuery
        $ H.action
        $ DCQ.Load path deckId
