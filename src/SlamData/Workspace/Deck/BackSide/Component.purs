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

import Data.Array as Arr
import Data.Foldable as F
import Data.String as Str
import Data.Map as Map

import Halogen as H
import Halogen.HTML.Events.Indexed as HE
import Halogen.HTML.Indexed as HH
import Halogen.HTML.Properties.Indexed as HP
import Halogen.HTML.Properties.Indexed.ARIA as ARIA
import Halogen.Themes.Bootstrap3 as B

import SlamData.Effects (Slam)
import SlamData.Render.Common (glyph)
import SlamData.Render.CSS as Rc
import SlamData.Workspace.Card.CardType as CT
import SlamData.Workspace.Card.Component.CSS as CCSS
import SlamData.Quasar.Auth (retrieveIdToken)

import SlamData.Workspace.Card.Draftboard.Model (DeckPosition)
import SlamData.Workspace.Deck.DeckId (DeckId)
import SlamData.Workspace.Deck.Model (Deck)

data Query a
  = UpdateFilter String a
  | DoAction BackAction a
  | UpdateCardType (Maybe CT.CardType) (Array CT.CardType) a
  | Init a
  -- TODO: this is a bit of a hack, we probably instead want a way of
  -- customising the available and enabled actions based on card type & state
  -- in the long run -gb
  | SetUnwrappable DeckMap a

data BackAction
  = Trash
  | Rename
  | Share
  | Embed
  | Publish
  | DeleteDeck
  | Mirror
  | Wrap
  | Unwrap DeckMap
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
    ]
  ⊕ (guard (state.activeCardType == Just CT.Draftboard) $> Unwrap state.unwrappableDecks)

type DeckMap = Map.Map DeckId (DeckPosition × Deck)

type State =
  { filterString ∷ String
  , activeCardType ∷ Maybe CT.CardType
  , cardTypes ∷ Array CT.CardType
  , saved ∷ Boolean
  , isLogged ∷ Boolean
  , unwrappableDecks ∷ DeckMap
  }

initialState ∷ State
initialState =
  { filterString: ""
  , activeCardType: Nothing
  , cardTypes: mempty
  , saved: false
  , isLogged: false
  , unwrappableDecks: Map.empty
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
  Unwrap _ → "Collapse board"
  Unshare → "Unshare deck"

keywordsAction ∷ BackAction → Array String
keywordsAction = case _ of
  Trash → ["remove", "delete", "trash"]
  Rename → ["rename", "title"]
  Share → ["share"]
  Embed → ["embed"]
  Publish → ["publish", "presentation", "view"]
  DeleteDeck → ["remove", "delete", "trash"]
  Mirror → ["mirror", "copy", "duplicate", "shallow"]
  Wrap → ["wrap", "pin", "card"]
  Unwrap _ → ["collapse", "unwrap", "breakout", "remove", "merge"]
  Unshare → ["unshare", "manage"]

actionEnabled ∷ State → BackAction → Boolean
actionEnabled st a =
  case st.activeCardType, a of
    Just CT.ErrorCard, Trash → false
    Just CT.NextAction, Trash → false
    Nothing, Trash → false
    _, Unwrap _ → not Map.isEmpty st.unwrappableDecks
    _, Mirror | F.elem CT.Draftboard st.cardTypes → false
    _, _ → true

actionGlyph ∷ BackAction → HTML
actionGlyph = case _ of
  Trash → glyph B.glyphiconTrash
  Rename → glyph B.glyphiconPencil
  Share → glyph B.glyphiconShare
  Unshare → glyph B.glyphiconWrench
  Embed → glyph B.glyphiconShareAlt
  Publish → glyph B.glyphiconBlackboard
  Mirror → glyph B.glyphiconDuplicate
  Wrap → glyph B.glyphiconLogIn
  Unwrap _ → glyph B.glyphiconLogOut
  DeleteDeck → HH.i [ HP.classes [ Rc.actionIcon, Rc.deleteDeckIcon ] ] [ ]

type HTML = H.ComponentHTML Query
type DSL = H.ComponentDSL State Query Slam

comp ∷ H.Component State Query Slam
comp =
  H.lifecycleComponent
    { render
    , eval
    , finalizer: Nothing
    , initializer: Just (H.action Init)
    }

render ∷ State → HTML
render state =
  HH.div
    [ HP.class_ CCSS.deckCard ]
    [ HH.div
        [ HP.class_ Rc.deckBackSide ]
        [ HH.div_
            [ HH.form_
                [ HH.div_
                    [ HH.input
                        [ HP.value state.filterString
                        , HE.onValueInput (HE.input UpdateFilter)
                        , ARIA.label "Filter actions"
                        , HP.placeholder "Filter actions"
                        ]
                    , HH.button
                          [ HP.buttonType HP.ButtonButton ]
                          [ glyph B.glyphiconRemove ]
                    ]
                ]
            , HH.ul_
                $ map (backsideAction true) actions.enabledActions
                ⊕ map (backsideAction false) actions.disabledActions
            ]
        ]
    ]
  where

  actions ∷ {enabledActions ∷ Array BackAction, disabledActions ∷ Array BackAction}
  actions =
    foldl
      (\{enabledActions, disabledActions} action →
         if backActionConforms action
           then { enabledActions: Arr.snoc enabledActions action, disabledActions }
           else { enabledActions, disabledActions: Arr.snoc disabledActions action }
      )
      {enabledActions: [], disabledActions: []}
      (allBackActions state)

  backActionConforms ∷ BackAction → Boolean
  backActionConforms ba =
    actionEnabled state ba &&
      F.any
        (isJust ∘ Str.stripPrefix (Str.trim $ Str.toLower state.filterString))
        (keywordsAction ba)

  backsideAction ∷ Boolean → BackAction → HTML
  backsideAction enabled action =
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

      lbl = labelAction action ⊕ if enabled then "" else " disabled"
      icon = actionGlyph action

eval ∷ Query ~> DSL
eval (DoAction _ next) = pure next
eval (UpdateFilter str next) =
  H.modify (_ { filterString = str }) $> next
eval (UpdateCardType cty ctys next) =
  H.modify (_ { activeCardType = cty, cardTypes = ctys, unwrappableDecks = Map.empty :: DeckMap }) $> next
eval (Init next) = next <$ do
  isLogged ← map isJust $ H.fromEff retrieveIdToken
  H.modify (_ { isLogged = isLogged })
eval (SetUnwrappable decks next) =
  H.modify (_ { unwrappableDecks = decks }) $> next
