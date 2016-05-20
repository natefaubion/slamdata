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

module SlamData.Workspace.Deck.Component.State
  ( StateP
  , State
  , DisplayMode(..)
  , StateMode(..)
  , CardDef
  , initialDeck
  , _id
  , _accessType
  , _cards
  , _activeCardIndex
  , _name
  , _browserFeatures
  , _path
  , _saveTrigger
  , _runTrigger
  , _globalVarMap
  , _pendingCard
  , _failingCards
  , _stateMode
  , _displayMode
  , _initialSliderX
  , _initialSliderCardWidth
  , _sliderTransition
  , _sliderTranslateX
  , _nextActionCardElement
  , addCard
  , addCard'
  , removeCard
  , findFirst
  , findLast
  , findLastCardType
  , addPendingCard
  , removePendingCard
  , cardsOfType
  , fromModel
  , deckPath
  , cardIndexFromId
  , cardIdFromIndex
  , VirtualState
  , _VirtualState
  , runVirtualState
  , virtualState

  , VirtualIndex(..)
  , runVirtualIndex
  , activeCardId
  , activeCardType
  ) where

import SlamData.Prelude

import Data.Array as A
import Data.BrowserFeatures (BrowserFeatures)
import Data.Foldable (maximum, elem)
import Data.Lens (LensP, lens, (^.))
import Data.Path.Pathy ((</>))
import Data.Path.Pathy as P
import Data.Set as S
import Data.StrMap as SM

import DOM.HTML.Types (HTMLElement)

import Halogen.Component.Opaque.Unsafe (OpaqueState)
import Halogen.Component.Utils.Debounced (DebounceTrigger)

import SlamData.Effects (Slam)
import SlamData.Workspace.AccessType (AccessType(..))

import SlamData.Workspace.Card.CardId (CardId(..), runCardId)
import SlamData.Workspace.Card.CardType (CardType(..))
import SlamData.Workspace.Card.Model as Card
import SlamData.Workspace.Card.Port.VarMap as Port

import SlamData.Workspace.Deck.Component.Query (Query)
import SlamData.Workspace.Deck.DeckId (DeckId, deckIdToString)
import SlamData.Workspace.Deck.Model as Model

import Utils.Path (DirPath)

type StateP = OpaqueState State

data StateMode
  = Loading
  | Ready
  | Error String

newtype VirtualIndex = VirtualIndex Int

runVirtualIndex ∷ VirtualIndex → Int
runVirtualIndex (VirtualIndex i) = i

derive instance eqVirtualIndex ∷ Eq VirtualIndex
derive instance ordVirtualIndex ∷ Ord VirtualIndex

data DisplayMode
  = Normal
  | Backside
  | Dialog

derive instance eqDisplayMode ∷ Eq DisplayMode

-- | The deck state. See the corresponding lenses for descriptions of
-- | the fields.
type State =
  { id ∷ Maybe DeckId
  , fresh ∷ Int
  , accessType ∷ AccessType
  , cards ∷ Array CardDef
  , activeCardIndex ∷ VirtualIndex
  , name ∷ Maybe String
  , path ∷ Maybe DirPath
  , browserFeatures ∷ BrowserFeatures
  , saveTrigger ∷ Maybe (DebounceTrigger Query Slam)
  , runTrigger ∷ Maybe (DebounceTrigger Query Slam)
  , pendingCard ∷ Maybe CardId
  , failingCards ∷ S.Set CardId
  , globalVarMap ∷ Port.VarMap
  , stateMode ∷ StateMode
  , displayMode ∷ DisplayMode
  , initialSliderX ∷ Maybe Number
  , initialSliderCardWidth ∷ Maybe Number
  , sliderTransition ∷ Boolean
  , sliderTranslateX ∷ Number
  , nextActionCardElement ∷ Maybe HTMLElement
  }

-- | A record used to represent card definitions in the deck.
type CardDef = { id ∷ CardId, ty ∷ CardType }

-- | Constructs a default `State` value.
initialDeck ∷ BrowserFeatures → State
initialDeck browserFeatures =
  { id: Nothing
  , fresh: 0
  , accessType: Editable
  , cards: mempty
  , activeCardIndex: VirtualIndex 0
  , name: Nothing
  , browserFeatures
  , path: Nothing
  , saveTrigger: Nothing
  , globalVarMap: SM.empty
  , runTrigger: Nothing
  , pendingCard: Nothing
  , failingCards: S.empty
  , stateMode: Ready
  , displayMode: Normal
  , initialSliderX: Nothing
  , initialSliderCardWidth: Nothing
  , sliderTransition: false
  , sliderTranslateX: 0.0
  , nextActionCardElement: Nothing
  }

-- | The unique identifier of the deck. If it's a fresh, unsaved deck, the id
-- | will be Nothing.
_id ∷ LensP State (Maybe DeckId)
_id = lens _.id _{id = _}

-- | A counter used to generate `CardId` values. This should be a monotonically increasing value
_fresh ∷ LensP State Int
_fresh = lens _.fresh _{fresh = _}

-- | Determines whether the deck is editable.
_accessType ∷ LensP State AccessType
_accessType = lens _.accessType _{accessType = _}

-- | The list of cards currently in the deck.
_cards ∷ LensP State (Array CardDef)
_cards = lens _.cards _{cards = _}

-- | The `CardId` for the currently focused card. `Nothing` indicates the next
-- | action card.
_activeCardIndex ∷ LensP State VirtualIndex
_activeCardIndex = lens _.activeCardIndex _{activeCardIndex = _}

-- | The display name of the deck.
_name ∷ LensP State (Maybe String)
_name = lens _.name _{name = _}

_browserFeatures ∷ LensP State BrowserFeatures
_browserFeatures = lens _.browserFeatures _{browserFeatures = _}

-- | The path to the deck in the filesystem
_path ∷ LensP State (Maybe DirPath)
_path = lens _.path _{path = _}

-- | The debounced trigger for deck save actions.
_saveTrigger ∷ LensP State (Maybe (DebounceTrigger Query Slam))
_saveTrigger = lens _.saveTrigger _{saveTrigger = _}

-- | The debounced trigger for running all cards that are pending.
_runTrigger ∷ LensP State (Maybe (DebounceTrigger Query Slam))
_runTrigger = lens _.runTrigger _{runTrigger = _}

-- | The global `VarMap`, passed through to the deck via the URL.
_globalVarMap ∷ LensP State Port.VarMap
_globalVarMap = lens _.globalVarMap _{globalVarMap = _}

-- | The earliest card in the deck that needs to evaluate.
_pendingCard ∷ LensP State (Maybe CardId)
_pendingCard = lens _.pendingCard _{pendingCard = _}

-- | The cards which currently have errors.
_failingCards ∷ LensP State (S.Set CardId)
_failingCards = lens _.failingCards _{failingCards = _}

-- | The "state mode" used to track whether the deck is ready, loading, or
-- | if an error has occurred while loading.
_stateMode ∷ LensP State StateMode
_stateMode = lens _.stateMode _{stateMode = _}

-- | backsided, dialog or normal (card)
_displayMode ∷ ∀ a r. LensP {displayMode ∷ a |r} a
_displayMode = lens (_.displayMode) (_{displayMode = _})

-- | The x position of the card slider at the start of the slide interaction in
-- | pixels. If `Nothing` slide interaction is not in progress.
_initialSliderX ∷ LensP State (Maybe Number)
_initialSliderX = lens _.initialSliderX _{initialSliderX = _}

-- | The width of the next action card at the start of the slide interaction in
-- | pixels. If `Nothing` either the slide interaction is not in progress or the
-- | next action card element reference is broken.
_initialSliderCardWidth ∷ LensP State (Maybe Number)
_initialSliderCardWidth = lens _.initialSliderCardWidth _{initialSliderCardWidth = _}

-- | Whether the translation of the card slider should be animated or not.
-- | Should be true between the end of the slide interaction and the end of the
-- | transition.
_sliderTransition ∷ LensP State Boolean
_sliderTransition = lens _.sliderTransition _{sliderTransition = _}

-- | The current x translation of the card slider during the slide interaction.
_sliderTranslateX ∷ LensP State Number
_sliderTranslateX = lens _.sliderTranslateX _{sliderTranslateX = _}

-- | The next action card HTML element
_nextActionCardElement ∷ LensP State (Maybe HTMLElement)
_nextActionCardElement = lens _.nextActionCardElement _{nextActionCardElement = _}

-- | Adds a new card to the deck.
-- |
-- | Takes the current deck state, the type of card to add, and an optional
-- | parent card ID.
addCard ∷ CardType → Maybe CardId → State → State
addCard cardType parent st = fst $ addCard' cardType parent st

-- | Adds a new card to the deck.
-- |
-- | Takes the current deck state, the type of card to add, and an optional
-- | parent card ID and returns the modified deck state and the new card ID.
addCard' ∷ CardType → Maybe CardId → State → Tuple State CardId
addCard' cardType parent st =
  let
    cardId = CardId st.fresh
    newState = st
      { fresh = st.fresh + 1
      , cards = st.cards `A.snoc` mkCardDef cardType cardId
      }
  in
    Tuple newState cardId

-- | Insert an error card as a child to a specified card, and reassociate all
-- | its children as children of the error card.
insertErrorCard ∷ CardId → State → State
insertErrorCard parentId st =
  st
    { cards =
        fromMaybe st.cards do
          parentAddr ← A.findIndex (\c → c.id ≡ parentId) st.cards
          let errorCard = mkCardDef ErrorCard cardId
          A.insertAt (parentAddr + 1) errorCard st.cards
    }
  where
  -- The -1 index is reserved for the error card.
  cardId = CardId (-1)

mkCardDef ∷ CardType → CardId → CardDef
mkCardDef cardType cardId = { id: cardId, ty: cardType }

-- | Removes a set of cards from the deck. Any cards that depend on a card
-- | in the set of provided cards will also be removed.
-- |
-- | Takes the set of IDs for the cards to remove and the current deck
-- | state.
removeCard ∷ CardId → State → State
removeCard cardId st =
  removePendingCard cardId $
    st
      { cards = newCards
      , activeCardIndex = VirtualIndex $ A.length virtualCards - 1
      }
  where
  virtualCards = A.filter f (runVirtualState (virtualState st)).cards
  cards = A.filter f st.cards

  newCards ∷ Array CardDef
  newCards = A.filter (\c → c.id < cardId) st.cards

  oldCards ∷ Array CardId
  oldCards =
    A.mapMaybe (\c → if c.id >= cardId then Just c.id else Nothing) st.cards

  f ∷ CardDef → Boolean
  f = not ∘ flip elem oldCards ∘ _.id

-- | Finds the first card in the deck.
findFirst ∷ State → Maybe CardId
findFirst { cards } = _.id <$> A.head cards

-- | Finds the last card in the deck.
findLast ∷ State → Maybe CardId
findLast { cards } = _.id <$> A.last cards

-- | Finds the type of the last card.
findLastCardType ∷ State → Maybe CardType
findLastCardType { cards } = _.ty <$> A.last cards

cardsOfType ∷ CardType → State → Array CardId
cardsOfType cardType =
  _.cards ⋙ A.mapMaybe cardTypeMatches ⋙ foldMap pure
  where
  cardTypeMatches { id: cid, ty } =
    if ty ≡ cardType
       then Just cid
       else Nothing

newtype VirtualState = VirtualState State

runVirtualState ∷ VirtualState → State
runVirtualState (VirtualState st) = st

_VirtualState ∷ LensP VirtualState State
_VirtualState = lens runVirtualState \_ → VirtualState

-- | Equip the state for presentation by inserting Error cards
-- | in the appropriate places.
virtualState ∷ State → VirtualState
virtualState st =
  VirtualState
    case find' hasError st.cards of
      Just c → insertErrorCard c.id st
      Nothing → st
  where

  -- in case you're wondering, Data.Foldable.find does not find the *first*
  -- satisfying element in the list! This took me a long time to figure out.
  -- TODO: this is unnecessary after updating to foldable-traversable v1.0.0
  find' ∷ ∀ a. (a → Boolean) → Array a → Maybe a
  find' p xs =
    A.findIndex p xs
      >>= A.index xs

  hasError ∷ CardDef → Boolean
  hasError c = S.member c.id st.failingCards

-- | Updates the stored card that is pending to run. This handles the logic of
-- | changing the pending card when a provided card appears earlier in the deck
-- | than the currently enqueued card, and if the provided card appears after
-- | the currently enqueued card the function is a no-op.
addPendingCard ∷ CardId → State → State
addPendingCard cardId st@{ pendingCard } =
  case pendingCard of
    Nothing → st { pendingCard = Just cardId }
    Just oldCardId | cardId < oldCardId → st { pendingCard = Just cardId }
    _ -> st

removePendingCard ∷ CardId → State → State
removePendingCard cardId st@{ pendingCard } =
  case pendingCard of
    Just oldCardId | cardId <= oldCardId → st { pendingCard = Nothing }
    _ → st

-- | Finds the current deck path
deckPath ∷ State → Maybe DirPath
deckPath state = do
  path ← state.path
  deckId ← deckIdToString <$> state.id
  pure $ path </> P.dir deckId

-- | Reconstructs a deck state from a deck model.
fromModel
  ∷ BrowserFeatures
  → Maybe DirPath
  → Maybe DeckId
  → Model.Deck
  → State
  → Tuple (Array Card.Model) State
fromModel browserFeatures path deckId { cards, name } state =
  Tuple
    cards
    ((state
        { accessType = ReadOnly
        , activeCardIndex = VirtualIndex $ A.length cardDefs - 1 -- fishy!
        , displayMode = Normal
        , browserFeatures = browserFeatures
        , cards = cardDefs
        , failingCards = S.empty
        , fresh = maybe 0 (_ + 1) $ maximum $ map (runCardId ∘ _.cardId) cards
        , globalVarMap = SM.empty
        , id = deckId
        , initialSliderX = Nothing
        , name = name
        , path = path
        , runTrigger = Nothing
        , pendingCard = Nothing
        }) ∷ State)
  where
  cardDefs = foldMap cardDefFromModel cards

  cardDefFromModel ∷ Card.Model → Array CardDef
  cardDefFromModel { cardId, cardType } = pure { id: cardId, ty: cardType }

cardIndexFromId ∷ VirtualState → CardId → VirtualIndex
cardIndexFromId st =
  -- TODO: for performance, use A.findIndex instead
  VirtualIndex ∘ fromMaybe (A.length cards) ∘ flip A.elemIndex (_.id <$> cards)
  where
    cards = st ^. _VirtualState ∘ _cards

cardFromIndex ∷ VirtualState → VirtualIndex → Maybe CardDef
cardFromIndex st (VirtualIndex i) = A.index (st ^. _VirtualState ∘ _cards) i

cardIdFromIndex ∷ VirtualState → VirtualIndex → Maybe CardId
cardIdFromIndex st vi = _.id <$> cardFromIndex st vi

activeCardId ∷ VirtualState → Maybe CardId
activeCardId st =
  cardIdFromIndex st $
    st ^. _VirtualState ∘ _activeCardIndex

activeCardType ∷ VirtualState → Maybe CardType
activeCardType st =
  _.ty <$> cardFromIndex st (st ^. _VirtualState ∘ _activeCardIndex)
