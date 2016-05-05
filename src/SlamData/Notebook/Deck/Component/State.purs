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

module SlamData.Notebook.Deck.Component.State
  ( StateP
  , State
  , StateMode(..)
  , CardDef
  , CardConstructor
  , initialDeck
  , _id
  , _fresh
  , _accessType
  , _cards
  , _dependencies
  , _activeCardId
  , _name
  , _browserFeatures
  , _viewingCard
  , _path
  , _saveTrigger
  , _runTrigger
  , _globalVarMap
  , _pendingCards
  , _stateMode
  , _backsided
  , addCard
  , addCard'
  , removeCards
  , findRoot
  , findParent
  , findChildren
  , findDescendants
  , findLast
  , findLastCardType
  , addPendingCard
  , getCardType
  , cardsOfType
  , fromModel
  , deckPath
  ) where

import SlamData.Prelude

import Data.BrowserFeatures (BrowserFeatures)
import Data.Foldable (maximum, any)
import Data.Lens (LensP, lens)
import Data.List (List)
import Data.List as L
import Data.Map as M
import Data.Path.Pathy ((</>))
import Data.Path.Pathy as P
import Data.Set as S
import Data.StrMap as SM

import Halogen as H
import Halogen.Component.Utils.Debounced (DebounceTrigger)

import SlamData.Effects (Slam)
import SlamData.Notebook.AccessType (AccessType(..))
import SlamData.Notebook.Card.Ace.Component (AceEvaluator, AceSetup, aceComponent)
import SlamData.Notebook.Card.API.Component (apiComponent)
import SlamData.Notebook.Card.APIResults.Component (apiResultsComponent)
import SlamData.Notebook.Card.CardId (CardId(..), runCardId)
import SlamData.Notebook.Card.CardType (CardType(..), AceMode(..))
import SlamData.Notebook.Card.Chart.Component (chartComponent)
import SlamData.Notebook.Card.Component (CardComponent, CardStateP, CardQueryP, initialCardState)
import SlamData.Notebook.Card.Download.Component (downloadComponent)
import SlamData.Notebook.Card.JTable.Component (jtableComponent)
import SlamData.Notebook.Card.Markdown.Component (markdownComponent)
import SlamData.Notebook.Card.Next.Component (nextCardComponent)
import SlamData.Notebook.Card.Save.Component (saveCardComponent)
import SlamData.Notebook.Card.DownloadOptions.Component as DOpts
import SlamData.Notebook.Card.Markdown.Eval (markdownEval, markdownSetup)
import SlamData.Notebook.Card.Model as Card
import SlamData.Notebook.Card.Port.VarMap as Port
import SlamData.Notebook.Card.Query.Eval (queryEval, querySetup)
import SlamData.Notebook.Card.Search.Component (searchComponent)
import SlamData.Notebook.Card.Viz.Component (vizComponent)
import SlamData.Notebook.Card.OpenResource.Component (openResourceComponent)
import SlamData.Notebook.Deck.Component.ChildSlot (CardSlot(..), ChildSlot, ChildState, ChildQuery)
import SlamData.Notebook.Deck.Component.Query (Query)
import SlamData.Notebook.Deck.DeckId (DeckId, deckIdToString)
import SlamData.Notebook.Deck.Model as Model

import Utils.Path (DirPath)

type StateP = H.ParentState State ChildState Query ChildQuery Slam ChildSlot

data StateMode
  = Loading
  | Ready
  | Error String

-- | The notebook state. See the corresponding lenses for descriptions of
-- | the fields.
type State =
  { id ∷ Maybe DeckId
  , fresh ∷ Int
  , accessType ∷ AccessType
  , cards ∷ List CardDef
  , dependencies ∷ M.Map CardId CardId
  , cardTypes ∷ M.Map CardId CardType
  , activeCardId ∷ Maybe CardId
  , name ∷ Maybe String
  , path ∷ Maybe DirPath
  , browserFeatures ∷ BrowserFeatures
  , viewingCard ∷ Maybe CardId
  , saveTrigger ∷ Maybe (DebounceTrigger Query Slam)
  , runTrigger ∷ Maybe (DebounceTrigger Query Slam)
  , pendingCards ∷ S.Set CardId
  , globalVarMap ∷ Port.VarMap
  , stateMode ∷ StateMode
  , backsided ∷ Boolean
  }

-- | A record used to represent card definitions in the notebook.
type CardDef = { id ∷ CardId, ty ∷ CardType, ctor ∷ CardConstructor }

-- | The specific `SlotConstructor` type for cards in the notebook.
type CardConstructor = H.SlotConstructor CardStateP CardQueryP Slam CardSlot

-- | Constructs a default `State` value.
initialDeck ∷ BrowserFeatures → State
initialDeck browserFeatures =
  { id: Nothing
  , fresh: 0
  , accessType: Editable
  , cards: mempty
  , cardTypes: M.empty
  , dependencies: M.empty
  , activeCardId: Nothing
  , name: Nothing
  , browserFeatures
  , viewingCard: Nothing
  , path: Nothing
  , saveTrigger: Nothing
  , globalVarMap: SM.empty
  , runTrigger: Nothing
  , pendingCards: S.empty
  , stateMode: Ready
  , backsided: false
  }

-- | The unique identifier of the deck. If it's a fresh, unsaved deck, the id
-- | will be Nothing.
_id ∷ LensP State (Maybe DeckId)
_id = lens _.id _{id = _}

-- | A counter used to generate `CardId` values.
_fresh ∷ LensP State Int
_fresh = lens _.fresh _{fresh = _}

-- | Determines whether the notebook is editable.
_accessType ∷ LensP State AccessType
_accessType = lens _.accessType _{accessType = _}

-- | The list of cards currently in the notebook.
_cards ∷ LensP State (List CardDef)
_cards = lens _.cards _{cards = _}

-- | A map of the edges in the dependency tree, where each key/value pair
-- | represents a child/parent relation.
_dependencies ∷ LensP State (M.Map CardId CardId)
_dependencies = lens _.dependencies _{dependencies = _}

-- | The `CardId` for the currently focused card.
_activeCardId ∷ LensP State (Maybe CardId)
_activeCardId = lens _.activeCardId _{activeCardId = _}

-- | The display name of the deck.
_name ∷ LensP State (Maybe String)
_name = lens _.name _{name = _}

-- | The path to the deck in the filesystem
_path ∷ LensP State (Maybe DirPath)
_path = lens _.path _{path = _}

-- | The available browser features - passed through to markdown results cards
-- | as they need this information to render the output HTML.
_browserFeatures ∷ LensP State BrowserFeatures
_browserFeatures = lens _.browserFeatures _{browserFeatures = _}

-- | The currently focused card when viewing an individual card within a
-- | notebook.
_viewingCard ∷ LensP State (Maybe CardId)
_viewingCard = lens _.viewingCard _{viewingCard = _}

-- | The debounced trigger for notebook save actions.
_saveTrigger ∷ LensP State (Maybe (DebounceTrigger Query Slam))
_saveTrigger = lens _.saveTrigger _{saveTrigger = _}

-- | The debounced trigger for running all cards that are pending.
_runTrigger ∷ LensP State (Maybe (DebounceTrigger Query Slam))
_runTrigger = lens _.runTrigger _{runTrigger = _}

-- | The global `VarMap`, passed through to the notebook via the URL.
_globalVarMap ∷ LensP State Port.VarMap
_globalVarMap = lens _.globalVarMap _{globalVarMap = _}

-- | The cards that have been enqueued to run.
_pendingCards ∷ LensP State (S.Set CardId)
_pendingCards = lens _.pendingCards _{pendingCards = _}

-- | The "state mode" used to track whether the notebook is ready, loading, or
-- | if an error has occurred while loading.
_stateMode ∷ LensP State StateMode
_stateMode = lens _.stateMode _{stateMode = _}

-- | Is `true` if backside of deck is displayed
_backsided ∷ ∀ a r. LensP {backsided ∷ a |r} a
_backsided = lens _.backsided _{backsided = _}

-- | Adds a new card to the notebook.
-- |
-- | Takes the current notebook state, the type of card to add, and an optional
-- | parent card ID.
addCard ∷ CardType → Maybe CardId → State → State
addCard cardType parent st = fst $ addCard' cardType parent st

-- | Adds a new card to the notebook.
-- |
-- | Takes the current notebook state, the type of card to add, and an optional
-- | parent card ID and returns the modified notebook state and the new card ID.
addCard' ∷ CardType → Maybe CardId → State → Tuple State CardId
addCard' cardType parent st =
  let
    cardId = CardId st.fresh
    newState = st
      { fresh = st.fresh + 1
      , cards = st.cards `L.snoc` mkCardDef cardType cardId
      , activeCardId = Just cardId
      , cardTypes = M.insert cardId cardType st.cardTypes
      , dependencies =
          maybe st.dependencies (flip (M.insert cardId) st.dependencies) parent
      }
  in
    Tuple newState cardId
  where
  mkCardDef ∷ CardType → CardId → CardDef
  mkCardDef cardType cardId =
    let component = cardTypeComponent cardType cardId st.browserFeatures
        initialState =
          H.parentState initialCardState
            { accessType = st.accessType }
    in { id: cardId
       , ty: cardType
       , ctor: H.SlotConstructor (CardSlot cardId) \_ → { component, initialState }
       }

cardTypeComponent ∷ CardType → CardId → BrowserFeatures → CardComponent
cardTypeComponent (Ace mode) _ _ = aceComponent { mode, evaluator, setup }
  where
  evaluator = aceEvalMode mode
  setup = aceSetupMode mode
cardTypeComponent Search _ _ = searchComponent
cardTypeComponent Viz _ _ = vizComponent
cardTypeComponent Chart _ _ = chartComponent
cardTypeComponent Markdown cardId bf = markdownComponent cardId bf
cardTypeComponent JTable _ _ = jtableComponent
cardTypeComponent Download _ _ = downloadComponent
cardTypeComponent API _ _ = apiComponent
cardTypeComponent APIResults _ _ = apiResultsComponent
cardTypeComponent NextAction _ _ = nextCardComponent
cardTypeComponent Save _ _ = saveCardComponent
cardTypeComponent OpenResource _ _ = openResourceComponent
cardTypeComponent DownloadOptions _ _ = DOpts.comp

aceEvalMode ∷ AceMode → AceEvaluator
aceEvalMode MarkdownMode = markdownEval
aceEvalMode SQLMode = queryEval

aceSetupMode ∷ AceMode → AceSetup
aceSetupMode MarkdownMode = markdownSetup
aceSetupMode SQLMode = querySetup

-- | Removes a set of cards from the notebook. Any cards that depend on a card
-- | in the set of provided cards will also be removed.
-- |
-- | Takes the set of IDs for the cards to remove and the current notebook
-- | state.
removeCards ∷ S.Set CardId → State → State
removeCards cardIds st = st
    { cards = L.filter f st.cards
    , cardTypes = foldl (flip M.delete) st.cardTypes cardIds'
    , dependencies = M.fromList $ L.filter g $ M.toList st.dependencies
    , pendingCards = S.difference st.pendingCards cardIds
    }
  where
  cardIds' ∷ S.Set CardId
  cardIds' = cardIds ⊕ foldMap (flip findDescendants st) cardIds

  f ∷ CardDef → Boolean
  f = not ∘ flip S.member cardIds' ∘ _.id

  g ∷ Tuple CardId CardId → Boolean
  g (Tuple kId vId) = not $ S.member kId cardIds' ∨ S.member vId cardIds'


-- | Finds the last card/card
findLast ∷ State → Maybe CardId
findLast state =
  maximum $ M.keys state.cardTypes

findLastCardType ∷ State → Maybe CardType
findLastCardType state =
  join $ flip M.lookup state.cardTypes <$> findLast state

-- | Finds the root in a chain of dependencies starting at the specified card.
-- | A card can be its own root if it depends on no other cards.
-- |
-- | Takes the ID of the card to start searching from and the current notebook
-- | state.
findRoot ∷ CardId → State → CardId
findRoot cardId st = case findParent cardId st of
  Nothing → cardId
  Just parentId → findRoot parentId st

-- | Finds the parent of a card. If the card is a root it has no parent, and
-- | the result will be `Nothing`.
-- |
-- | Takes the ID of the card to find the parent of and the current notebook
-- | state.
findParent ∷ CardId → State → Maybe CardId
findParent cardId st = M.lookup cardId st.dependencies

-- | Finds the immediate dependencies of a card.
-- |
-- | Takes the ID of the card to find the children of and the current notebook
-- | state.
findChildren ∷ CardId → State → S.Set CardId
findChildren parentId st =
  S.fromList $ map fst $ L.filter f $ M.toList st.dependencies
  where
  f ∷ Tuple CardId CardId → Boolean
  f (Tuple _ cardId) = cardId ≡ parentId

-- | Finds all the dependencies of a card: the children, children's children,
-- | and so on until the leaves of the tree are reached.
-- |
-- | Takes the ID of the card to find the descendants of and the current
-- | notebook state.
findDescendants ∷ CardId → State → S.Set CardId
findDescendants cardId st =
  let children = findChildren cardId st
  in children ⊕ foldMap (flip findDescendants st) children

-- | Determine's the `CardType` of a card; returns `Just` if the card is
-- | in the notebook, and `Nothing` if it is not.
getCardType ∷ CardId → State → Maybe CardType
getCardType cardId st = M.lookup cardId st.cardTypes

cardsOfType ∷ CardType → State → List CardId
cardsOfType cardType =
  _.cardTypes ⋙ M.toList ⋙ L.mapMaybe \(Tuple cid ty) →
    if ty ≡ cardType
       then Just cid
       else Nothing

-- | Adds a card to the set of cards that are enqueued to run.
-- |
-- | If the card is a descendant of an card that has already been enqueued this
-- | will have no effect, as in this case the card is already pending by
-- | implication: all cards under the queued ancestor will be evaluated as
-- | changes propagate through the subgraph.
-- |
-- | If the card is an ancestor of cards that have already been enqueued they
-- | will be removed when this card is added, for the same reasoning as above.
addPendingCard ∷ CardId → State → State
addPendingCard cardId st@{ pendingCards } =
  if cardId `S.member` pendingCards ∨ any isAncestor pendingCards
  then st
  else st { pendingCards = S.insert cardId (removeDescendants pendingCards) }
  where
  isAncestor ∷ CardId → Boolean
  isAncestor otherId = cardId `S.member` findDescendants otherId st
  removeDescendants ∷ S.Set CardId → S.Set CardId
  removeDescendants = flip S.difference (findDescendants cardId st)

-- | Finds the current deck path
deckPath ∷ State → Maybe DirPath
deckPath state = do
  path ← state.path
  deckId ← deckIdToString <$> state.id
  pure $ path </> P.dir deckId

-- | Reconstructs a notebook state from a notebook model.
fromModel
  ∷ BrowserFeatures
  → Maybe DirPath
  → Maybe DeckId
  → Model.Deck
  → Tuple (Array Card.Model) State
fromModel browserFeatures path deckId { cards, dependencies, name } =
  Tuple
    cards
    ({ id: deckId
    , fresh: maybe 0 (_ + 1) $ maximum $ map (runCardId ∘ _.cardId) cards
    , accessType: ReadOnly
    , cards: foldMap cardDefFromModel cards
    , cardTypes: foldl addCardIdTypePair M.empty cards
    , dependencies
    , activeCardId: Nothing
    , name
    , browserFeatures
    , viewingCard: Nothing
    , path
    , saveTrigger: Nothing
    , globalVarMap: SM.empty
    , runTrigger: Nothing
    , pendingCards: S.empty
    , stateMode: Loading
    , backsided: false
    } ∷ State)
  where
  addCardIdTypePair mp {cardId, cardType} = M.insert cardId cardType mp

  cardDefFromModel ∷ Card.Model → List CardDef
  cardDefFromModel { cardId, cardType} =
    let component = cardTypeComponent cardType cardId browserFeatures
        initialState = H.parentState initialCardState
    in
      pure
        { id: cardId
        , ty: cardType
        , ctor: H.SlotConstructor (CardSlot cardId) \_ → { component, initialState }
        }
