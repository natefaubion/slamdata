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
import Utils.CSS as CSSUtils

import Halogen as H
import Halogen.Component.Opaque.Unsafe (opaqueState, opaqueQuery, peekOpaqueQuery, OpaqueQuery)
import Halogen.Component.Utils.Drag as Drag
import Halogen.HTML.CSS.Indexed as HC
import Halogen.HTML.Events.Indexed as HE
import Halogen.HTML.Indexed as HH
import Halogen.HTML.Properties.Indexed as HP

import Math (round)

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
      , peek: Just peek
      }
  , initialState: H.parentState $ initialState { path = opts.path }
  , _State: Cp._DraftboardState
  , _Query: Cp.makeQueryPrism' Cp._DraftboardQuery
  }

render ∷ CardOptions → State → DraftboardHTML
render opts state =
  HH.div [ HP.classes [ RC.board ] ] $
    map renderDeck (foldl Array.snoc [] $ Map.toList state.decks)

  where

  renderDeck (Tuple deckId rect) =
    HH.div
      [ HP.key $ deckIdToString deckId
      , HC.style $ cssPos $ case state.moving of
          Just (Tuple deckId' rect') | deckId == deckId' → rect'
          _ → rect
      ]
      [ HH.slot deckId $ mkDeckComponent deckId ]

  mkDeckComponent id _ =
    { component: opts.deckComponent
    , initialState: opaqueState $ DCS.initialDeck
    }

  cssPos rect = do
    CSS.position CSS.absolute
    CSS.top $ CSS.px $ rect.y * Config.gridPx
    CSS.left $ CSS.px $ rect.x * Config.gridPx
    CSS.width $ CSS.px $ rect.width * Config.gridPx
    CSS.height $ CSS.px $ rect.height * Config.gridPx

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
evalBoard (Grabbing deckId ev next) = do
  case ev of
    Drag.Move _ d → do
      H.gets (Map.lookup deckId ∘ _.decks) >>= traverse_ \rect → do
        let newRect = clampDeck rect
              { x = rect.x + (d.offsetX / Config.gridPx)
              , y = rect.y + (d.offsetY / Config.gridPx)
              }
        H.modify _ { moving = Just (Tuple deckId newRect) }
    Drag.Done _ →
      stopDragging
  pure next
evalBoard (Resizing deckId ev next) = do
  case ev of
    Drag.Move _ d → do
      H.gets (Map.lookup deckId ∘ _.decks) >>= traverse_ \rect → do
        let newRect = clampDeck rect
              { width = rect.width + (d.offsetX / Config.gridPx)
              , height = rect.height + (d.offsetY / Config.gridPx)
              }
        H.modify _ { moving = Just (Tuple deckId newRect) }
    Drag.Done _ →
      stopDragging
  pure next
evalBoard (AddDeck next) = pure next

peek ∷ ∀ a. H.ChildF DeckId (OpaqueQuery DCQ.Query) a → DraftboardDSL Unit
peek (H.ChildF deckId q) = flip peekOpaqueQuery q
  case _ of
    DCQ.GrabDeck ev _ → startDragging deckId ev Grabbing
    DCQ.ResizeDeck ev _ → startDragging deckId ev Resizing
    _ → pure unit

  where
  startDragging deckId ev tag =
    H.gets (Map.lookup deckId ∘ _.decks) >>= traverse_ \rect → do
      H.modify _ { moving = Just (Tuple deckId rect) }
      void
        $ Drag.subscribe' ev
        $ right ∘ H.action ∘ tag deckId

stopDragging ∷ DraftboardDSL Unit
stopDragging = do
  H.gets _.moving >>= traverse_ \(Tuple deckId rect) →
    H.modify \s → s { decks = Map.insert deckId (roundDeck rect) s.decks }
  H.modify _ { moving = Nothing }

clampDeck ∷ DeckPosition → DeckPosition
clampDeck rect =
  { x: if rect.x < 0.0 then 0.0 else rect.x
  , y: if rect.y < 0.0 then 0.0 else rect.y
  , width: if rect.width < 10.0 then 10.0 else rect.width
  , height: if rect.height < 10.0 then 10.0 else rect.height
  }

roundDeck ∷ DeckPosition → DeckPosition
roundDeck rect =
  { x: round rect.x
  , y: round rect.y
  , width: round rect.width
  , height: round rect.height
  }

loadDecks ∷ DraftboardDSL Unit
loadDecks = void $
  H.gets _.path >>= traverse \path →
    H.gets (Map.keys ∘ _.decks) >>= traverse \deckId →
      H.query deckId
        $ opaqueQuery
        $ H.action
        $ DCQ.Load path deckId
