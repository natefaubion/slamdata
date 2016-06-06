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
  , CardDef
  , initialDeck
  , _id
  , _parent
  , _accessType
  , _cards
  , _activeCardIndex
  , _name
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
  , _cardElementWidth
  , _topLevel
  , addCard
  , addCard'
  , removeCard
  , findFirst
  , findLast
  , findLastCardType
  , findLastRealCard
  , addPendingCard
  , removePendingCard
  , cardsOfType
  , fromModel
  , deckPath
  , deckPath'
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
import Data.Foldable (maximum, elem)
import Data.Lens (LensP, lens, (^.))
import Data.Ord (max)
import Data.Path.Pathy ((</>))
import Data.Path.Pathy as P
import Data.Set as S
import Data.StrMap as SM

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
import SlamData.Workspace.StateMode (StateMode(..))

import Utils.Path (DirPath)

type StateP = OpaqueState State

newtype VirtualIndex = VirtualIndex Int

runVirtualIndex ∷ VirtualIndex → Int
runVirtualIndex (VirtualIndex i) = i

derive instance eqVirtualIndex ∷ Eq VirtualIndex
derive instance ordVirtualIndex ∷ Ord VirtualIndex

instance semiringVirtualIndex ∷ Semiring VirtualIndex where
  one = VirtualIndex one
  zero = VirtualIndex zero
  add (VirtualIndex a) (VirtualIndex b) = VirtualIndex $ a + b
  mul (VirtualIndex a) (VirtualIndex b) = VirtualIndex $ a * b

instance ringVirtualIndex ∷ Ring VirtualIndex where
  sub (VirtualIndex a) (VirtualIndex b) = VirtualIndex $ a - b

data DisplayMode
  = Normal
  | Backside
  | Dialog

derive instance eqDisplayMode ∷ Eq DisplayMode

-- | The deck state. See the corresponding lenses for descriptions of
-- | the fields.
type State =
  { id ∷ Maybe DeckId
  , parent ∷ Maybe (Tuple DeckId CardId)
  , fresh ∷ Int
  , accessType ∷ AccessType
  , cards ∷ Array CardDef
  , activeCardIndex ∷ VirtualIndex
  , name ∷ Maybe String
  , path ∷ Maybe DirPath
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
  , cardElementWidth ∷ Maybe Number
  , topLevel ∷ Boolean
  }

-- | A record used to represent card definitions in the deck.
type CardDef = { id ∷ CardId, ty ∷ CardType }

-- | Constructs a default `State` value.
initialDeck ∷ State
initialDeck =
  { id: Nothing
  , parent: Nothing
  , fresh: 0
  , accessType: Editable
  , cards: mempty
  , activeCardIndex: VirtualIndex 0
  , name: Nothing
  , path: Nothing
  , saveTrigger: Nothing
  , globalVarMap: SM.empty
  , runTrigger: Nothing
  , pendingCard: Nothing
  , failingCards: S.empty
  , stateMode: Loading
  , displayMode: Normal
  , initialSliderX: Nothing
  , initialSliderCardWidth: Nothing
  , sliderTransition: false
  , sliderTranslateX: 0.0
  , cardElementWidth: Nothing
  , topLevel: true
  }

-- | The unique identifier of the deck. If it's a fresh, unsaved deck, the id
-- | will be Nothing.
_id ∷ ∀ a r. LensP {id ∷ a|r} a
_id = lens _.id _{id = _}

-- | A pointer to the parent deck/card. If `Nothing`, the deck is assumed to be
-- | the root deck.
_parent ∷ ∀ a r. LensP {parent ∷ a|r} a
_parent = lens _.parent _{parent = _}

-- | A counter used to generate `CardId` values. This should be a monotonically increasing value
_fresh ∷ ∀ a r. LensP {fresh ∷ a|r} a
_fresh = lens _.fresh _{fresh = _}

-- | Determines whether the deck is editable.
_accessType ∷ ∀ a r. LensP {accessType ∷ a|r} a
_accessType = lens _.accessType _{accessType = _}

-- | The list of cards currently in the deck.
_cards ∷ ∀ a r. LensP {cards ∷ a |r} a
_cards = lens _.cards _{cards = _}

-- | The `CardId` for the currently focused card. `Nothing` indicates the next
-- | action card.
_activeCardIndex ∷ ∀ a r. LensP {activeCardIndex ∷ a |r} a
_activeCardIndex = lens _.activeCardIndex _{activeCardIndex = _}

-- | The display name of the deck.
_name ∷ ∀ a r. LensP {name ∷ a |r} a
_name = lens _.name _{name = _}

-- | The path to the deck in the filesystem
_path ∷ ∀ a r. LensP {path ∷ a |r} a
_path = lens _.path _{path = _}

-- | The debounced trigger for deck save actions.
_saveTrigger ∷ ∀ a r. LensP {saveTrigger ∷ a|r} a
_saveTrigger = lens _.saveTrigger _{saveTrigger = _}

-- | The debounced trigger for running all cards that are pending.
_runTrigger ∷ ∀ a r. LensP {runTrigger ∷ a|r} a
_runTrigger = lens _.runTrigger _{runTrigger = _}

-- | The global `VarMap`, passed through to the deck via the URL.
_globalVarMap ∷ ∀ a r. LensP {globalVarMap ∷ a |r} a
_globalVarMap = lens _.globalVarMap _{globalVarMap = _}

-- | The earliest card in the deck that needs to evaluate.
_pendingCard ∷ ∀ a r. LensP {pendingCard ∷ a|r} a
_pendingCard = lens _.pendingCard _{pendingCard = _}

-- | The cards which currently have errors.
_failingCards ∷ ∀ a r. LensP {failingCards ∷ a|r} a
_failingCards = lens _.failingCards _{failingCards = _}

-- | The "state mode" used to track whether the deck is ready, loading, or
-- | if an error has occurred while loading.
_stateMode ∷ ∀ a r. LensP {stateMode ∷ a|r} a
_stateMode = lens _.stateMode _{stateMode = _}

-- | backsided, dialog or normal (card)
_displayMode ∷ ∀ a r. LensP {displayMode ∷ a |r} a
_displayMode = lens (_.displayMode) (_{displayMode = _})

-- | The x position of the card slider at the start of the slide interaction in
-- | pixels. If `Nothing` slide interaction is not in progress.
_initialSliderX ∷ ∀ a r. LensP {initialSliderX ∷ a|r} a
_initialSliderX = lens _.initialSliderX _{initialSliderX = _}

-- | The width of the next action card at the start of the slide interaction in
-- | pixels. If `Nothing` either the slide interaction is not in progress or the
-- | next action card element reference is broken.
_initialSliderCardWidth ∷ ∀ a r. LensP {initialSliderCardWidth ∷ a|r} a
_initialSliderCardWidth = lens _.initialSliderCardWidth _{initialSliderCardWidth = _}

-- | Whether the translation of the card slider should be animated or not.
-- | Should be true between the end of the slide interaction and the end of the
-- | transition.
_sliderTransition ∷ ∀ a r. LensP {sliderTransition ∷ a |r} a
_sliderTransition = lens _.sliderTransition _{sliderTransition = _}

-- | The current x translation of the card slider during the slide interaction.
_sliderTranslateX ∷ ∀ a r. LensP {sliderTranslateX ∷ a |r} a
_sliderTranslateX = lens _.sliderTranslateX _{sliderTranslateX = _}

-- | The width of card
_cardElementWidth ∷ ∀ a r. LensP {cardElementWidth ∷ a|r} a
_cardElementWidth = lens _.cardElementWidth _{cardElementWidth = _}

-- | Whether the deck is at the top-level of the deck component hierarchy
_topLevel ∷ ∀ a r. LensP {topLevel ∷ a|r} a
_topLevel = lens _.topLevel _{topLevel = _}

addCard ∷ CardType → State → State
addCard cardType st = fst $ addCard' cardType st

addCard' ∷ CardType → State → State × CardId
addCard' cardType st =
  let
    cardId = CardId st.fresh
    newState = st
      { fresh = st.fresh + one
      , cards =
          let
            def = mkCardDef cardType cardId
          in case A.uncons $ A.reverse st.cards of
            Nothing → st.cards `A.snoc` def
            Just {head, tail} →
              if head.id ≡ top
                then A.reverse $ def A.: tail
                else st.cards `A.snoc` def
      }
  in newState × cardId

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

insertNextActionCard ∷ State → State
insertNextActionCard st =
  let
    lastId = findLast st
  in
    st
      { cards =
           case lastId of
             Nothing → [ mkCardDef NextAction top ]
             Just lid →
               if lid ≡ top
                 then st.cards
                 else st.cards `A.snoc` mkCardDef NextAction top
      , activeCardIndex =
          case lastId of
            Nothing → zero
            Just lid → st.activeCardIndex
    }

mkCardDef ∷ CardType → CardId → CardDef
mkCardDef cardType cardId = { id: cardId, ty: cardType }


removeCard ∷ CardId → State → State
removeCard cardId st =
  removePendingCard cardId $
    st
      { cards = newCards
      , activeCardIndex = VirtualIndex $ max zero $ A.length newCards - one
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

findLastRealCard ∷ State → Maybe CardId
findLastRealCard { cards } =
  A.findLastIndex (\x → x.id ≠ top) cards
    >>= A.index cards
    <#> _.id

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
    $ insertNextActionCard
    $ case find' hasError st.cards of
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
deckPath state = deckPath' <$> state.path <*> state.id

deckPath' ∷ DirPath → DeckId → DirPath
deckPath' path deckId = path </> P.dir (deckIdToString deckId)

-- | Reconstructs a deck state from a deck model.
fromModel
  ∷ Maybe DirPath
  → Maybe DeckId
  → Model.Deck
  → State
  → Tuple (Array Card.Model) State
fromModel path deckId { cards, name, parent } state =
  Tuple
    cards
    ((state
        { accessType = Editable -- why was it ReadOnly?
        , activeCardIndex = VirtualIndex $ A.length cardDefs - 1 -- fishy!
        , displayMode = Normal
        , cards = cardDefs
        , failingCards = S.empty
        , fresh = maybe 0 (_ + 1) $ maximum $ map (runCardId ∘ _.cardId) cards
        , globalVarMap = SM.empty
        , id = deckId
        , parent = parent
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
