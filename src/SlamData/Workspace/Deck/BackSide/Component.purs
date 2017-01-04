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

module SlamData.Workspace.Deck.BackSide.Component where

import SlamData.Prelude

import Data.Array as A
import Data.List (List(..), (:))
import Data.String as Str

import Halogen as H
import Halogen.HTML.Events.Indexed as HE
import Halogen.HTML.Indexed as HH
import Halogen.HTML.Properties.Indexed as HP
import Halogen.HTML.Properties.Indexed.ARIA as ARIA

import SlamData.Monad (Slam)
import SlamData.Quasar.Auth (getIdToken)
import SlamData.Render.Common as RC
import SlamData.Render.CSS as CSS
import SlamData.Workspace.Card.Model as CM
import SlamData.Workspace.Card.Component.CSS as CCSS
import SlamData.Workspace.Deck.Component.State (CardDef)
import SlamData.Workspace.Deck.DeckId (DeckId)
import SlamData.Workspace.Eval.Persistence as P

data Query a
  = UpdateFilter String a
  | DoAction BackAction a
  | UpdateCard (Maybe CardDef) (Array CardDef) a
  | Init a

data BackAction
  = Trash
  | Rename
  | Share
  | Embed
  | Publish
  | DeleteDeck
  | Mirror
  | Wrap
  | Unwrap
  | Share
  | Unshare

allBackActions ∷ State → Array BackAction
allBackActions state =
  [ Trash
  , Rename
  ]
  ⊕ (if state.isLogged
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

type BackSideOptions =
  { deckId ∷ DeckId
  , displayCursor ∷ List DeckId
  }

type State =
  { filterString ∷ String
  , activeCard ∷ Maybe CardDef
  , cardDefs ∷ Array CardDef
  , saved ∷ Boolean
  , isLogged ∷ Boolean
  , unwrappable ∷ Boolean
  }

initialState ∷ State
initialState =
  { filterString: ""
  , activeCard: Nothing
  , cardDefs: mempty
  , saved: false
  , isLogged: false
  , unwrappable: false
  }

labelAction ∷ BackAction → String
labelAction = case _ of
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

actionEnabled ∷ State → BackAction → Boolean
actionEnabled st a =
  case st.activeCard, a of
    Nothing, Trash → false
    _, Unwrap → st.unwrappable
    _, _ → true

actionGlyph ∷ BackAction → HTML
actionGlyph = case _ of
  Trash → HH.img [ HP.src "img/cardAndDeckActions/deleteCard.svg" ]
  Rename → HH.img [ HP.src "img/cardAndDeckActions/renameDeck.svg" ]
  Share → HH.img [ HP.src "img/cardAndDeckActions/shareDeck.svg" ]
  Unshare → HH.img [ HP.src "img/cardAndDeckActions/unshareDeck.svg" ]
  Embed → HH.img [ HP.src "img/cardAndDeckActions/embedDeck.svg" ]
  Publish → HH.img [ HP.src "img/cardAndDeckActions/publishDeck.svg" ]
  Mirror → HH.img [ HP.src "img/cardAndDeckActions/mirrorDeck.svg" ]
  Wrap → HH.img [ HP.src "img/cardAndDeckActions/wrapDeck.svg" ]
  Unwrap → HH.img [ HP.src "img/cardAndDeckActions/unwrapDeck.svg" ]
  DeleteDeck → HH.img [ HP.src "img/cardAndDeckActions/deleteDeck.svg" ]

type HTML = H.ComponentHTML Query
type DSL = H.ComponentDSL State Query Slam

comp ∷ BackSideOptions → H.Component State Query Slam
comp opts =
  H.lifecycleComponent
    { render: render opts
    , eval: eval opts
    , finalizer: Nothing
    , initializer: Just (H.action Init)
    }

render ∷ BackSideOptions → State → HTML
render opts state =
  -- Extra div for consistent targetting with next action card styles
  HH.div_
    [ HH.div
        [ HP.class_ CCSS.deckCard ]
        [ HH.div
            [ HP.class_ CSS.deckBackSide ]
            [ HH.div_
                [ HH.form_
                    [ HH.div_
                        [ HH.div
                            [ HP.class_ (HH.className "sd-action-filter-icon") ]
                            [ RC.searchFieldIcon ]
                        , HH.input
                            [ HP.value state.filterString
                            , HE.onValueInput (HE.input UpdateFilter)
                            , ARIA.label "Filter deck and card actions"
                            , HP.placeholder "Filter actions"
                            ]
                        , HH.button
                            [ HP.buttonType HP.ButtonButton
                            , HE.onClick (HE.input_ (UpdateFilter ""))
                            , HP.enabled (state.filterString /= "")
                            ]
                            [ RC.clearFieldIcon "Clear filter" ]
                        ]
                    ]
                , HH.ul_ $ map backsideAction (allBackActions state)
                ]
            ]
        ]
    ]
  where

  filterString ∷ Str.Pattern
  filterString = Str.Pattern (Str.toLower state.filterString)

  backsideAction ∷ BackAction → HTML
  backsideAction action =
    HH.li_
      [ HH.button attrs
          [ icon
          , HH.p_ [ HH.text $ labelAction action ]
          ]
      ]
    where
      attrs =
        [ HP.title lbl
        , ARIA.label lbl
        , HP.disabled (not enabled)
        , HP.buttonType HP.ButtonButton
        ] ⊕ if enabled then [ HE.onClick (HE.input_ (DoAction action)) ] else [ ]

      enabled = Str.contains filterString (Str.toLower $ labelAction action) && actionEnabled state action
      lbl = labelAction action ⊕ if enabled then "" else " disabled"
      icon = actionGlyph action

eval ∷ BackSideOptions → Query ~> DSL
eval opts = case _ of
  DoAction _ next →
    pure next
  UpdateFilter str next → do
    H.modify _ { filterString = str }
    pure next
  UpdateCard card defs next → do
    uw ← fromMaybe false <$> traverse (unwrappable opts) card
    H.modify _
      { activeCard = card
      , cardDefs = defs
      , unwrappable = uw
      }
    pure next
  Init next → do
    isLogged ← isJust <$> H.liftH getIdToken
    H.modify _ { isLogged = isLogged }
    pure next

unwrappable ∷ BackSideOptions → CardDef → DSL Boolean
unwrappable { displayCursor, deckId } { cardId } = do
  deck ← map _.model <$> H.liftH (P.getDeck deckId)
  card ← map _.model <$> H.liftH (P.getCard cardId)
  let
    cardLen = A.length ∘ _.cards <$> deck
    deckIds = CM.childDeckIds <$> card
  pure case displayCursor, card, cardLen, deckIds of
    Nil    , Just (CM.Draftboard _), Just 1, Just (_ : Nil) → true
    _ : Nil, Just (CM.Draftboard _), Just 1, _ → true
    _ , _, _, _ → false
