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

module SlamData.Workspace.Deck.BackSide where

import SlamData.Prelude

import Data.List (List)

import SlamData.ActionList.Action as Action
import SlamData.Workspace.Card.CardType as CT
import SlamData.Workspace.Deck.Component.State (CardDef)
import SlamData.Workspace.Deck.DeckId (DeckId)

data BackAction
  = Trash
  | Rename
  | Embed
  | Publish
  | DeleteDeck
  | Mirror
  | Wrap
  | WrapChoice CT.CardType
  | Unwrap
  | Share
  | Unshare

derive instance eqBackAction ∷ Eq BackAction

type BackSideOptions =
  { deckId ∷ DeckId
  , displayCursor ∷ List DeckId
  }

allBackActions ∷ Boolean → Array BackAction
allBackActions isAdvanced =
  [ Trash
  , Rename
  ]
  ⊕ (if isAdvanced
      then [ Share
           , Unshare
           ]
      else [ ])
  ⊕ [ Embed
    , Publish
    , DeleteDeck
    , Mirror
    , Wrap
    , Unwrap
    ]

toActionListAction
  ∷ Boolean
  → Maybe CardDef
  → Array CardDef
  → BackAction
  → Action.Action BackAction
toActionListAction unwrappable activeCard cardDefs action =
  case action of
    Wrap → Action.mkDrill
      { name
      , iconSrc
      , description
      , children: toActionListAction unwrappable activeCard cardDefs <$>
          [ WrapChoice CT.Draftboard
          , WrapChoice CT.Tabs
          ]
      }
    _ → Action.mkDo
      { name
      , iconSrc
      , description
      , highlighted
      , disabled
      , action
      }
  where
  iconSrc = case action of
    Trash → "img/cardAndDeckActions/deleteCard.svg"
    Rename → "img/cardAndDeckActions/renameDeck.svg"
    Share → "img/cardAndDeckActions/shareDeck.svg"
    Unshare → "img/cardAndDeckActions/unshareDeck.svg"
    Embed → "img/cardAndDeckActions/embedDeck.svg"
    Publish → "img/cardAndDeckActions/publishDeck.svg"
    Mirror → "img/cardAndDeckActions/mirrorDeck.svg"
    Wrap → "img/cardAndDeckActions/wrapDeck.svg"
    Unwrap → "img/cardAndDeckActions/unwrapDeck.svg"
    DeleteDeck → "img/cardAndDeckActions/deleteDeck.svg"
    WrapChoice cty → CT.cardIconLightSrc cty

  name = case action of
    Trash → "Delete card"
    Rename → "Rename deck"
    Share → "Share deck"
    Embed → "Embed deck"
    Publish → "Publish deck"
    DeleteDeck → "Delete deck"
    Mirror → "Mirror"
    Wrap → "Wrap"
    Unwrap → "Collapse"
    Unshare → "Unshare deck"
    WrapChoice cty → CT.cardName cty

  description = name

  highlighted = case activeCard, action of
    Nothing, Trash → false
    _, Unwrap → unwrappable
    _, _ → true

  disabled = not highlighted
