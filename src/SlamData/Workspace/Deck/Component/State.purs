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
  , _modelCards
  , _displayCards
  , _cardsToLoad
  , _activeCardIndex
  , _path
  , _saveTrigger
  , _runTrigger
  , _globalVarMap
  , _pendingCard
  , _stateMode
  , _displayMode
  , _initialSliderX
  , _initialSliderCardWidth
  , _sliderTransition
  , _sliderTranslateX
  , _cardElementWidth
  , _level
  , addCard
  , addCard'
  , removeCard
  , findLastCardType
  , findLastRealCardIndex
  , findLastRealCard
  , addPendingCard
  , removePendingCard
  , apiCards
  , fromModel
  , deckPath
  , deckPath'
  , cardIndexFromCoord
  , cardCoordFromIndex
  , activeCardCoord
  , activeCardType
  , eqCoordModel
  , compareCoordCards
  , coordModelToCoord
  ) where

import SlamData.Prelude

import Data.Array as A
import Data.Foldable (maximum)
import Data.Lens (LensP, lens)
import Data.Lens as Lens
import Data.Ord (max)
import Data.Path.Pathy ((</>))
import Data.Path.Pathy as P
import Data.StrMap as SM
import Data.Set as Set

import Halogen.Component.Opaque.Unsafe (OpaqueState)
import Halogen.Component.Utils.Debounced (DebounceTrigger)

import SlamData.Effects (Slam)
import SlamData.Workspace.AccessType (AccessType(..))

import SlamData.Workspace.Card.CardId (CardId(..))
import SlamData.Workspace.Card.CardId as CID
import SlamData.Workspace.Card.CardType as CT
import SlamData.Workspace.Card.Model as Card
import SlamData.Workspace.Card.Port.VarMap as Port

import SlamData.Workspace.Deck.Component.Query (Query)
import SlamData.Workspace.Deck.DeckId (DeckId, deckIdToString)
import SlamData.Workspace.Deck.DeckLevel as DL
import SlamData.Workspace.Deck.Model as Model
import SlamData.Workspace.StateMode (StateMode(..))

import Utils.Path (DirPath)

type StateP = OpaqueState State

data DisplayMode
  = Normal
  | Backside
  | Dialog

derive instance eqDisplayMode ∷ Eq DisplayMode

-- | The deck state. See the corresponding lenses for descriptions of
-- | the fields.
type State =
  { id ∷ DeckId
  , parent ∷ Maybe (DeckId × CardId)
  , mirror ∷ Maybe (DeckId × Int)
  , fresh ∷ Int
  , accessType ∷ AccessType
  , modelCards ∷ Array (DeckId × Card.Model)
  , displayCards ∷ Array (DeckId × Card.Model)
  , cardsToLoad ∷ Set.Set (DeckId × CardId)
  , activeCardIndex ∷ Maybe Int
  , path ∷ DirPath
  , saveTrigger ∷ Maybe (DebounceTrigger Query Slam)
  , runTrigger ∷ Maybe (DebounceTrigger Query Slam)
  , pendingCard ∷ Maybe (DeckId × CardId)
  , globalVarMap ∷ Port.VarMap
  , stateMode ∷ StateMode
  , displayMode ∷ DisplayMode
  , initialSliderX ∷ Maybe Number
  , initialSliderCardWidth ∷ Maybe Number
  , sliderTransition ∷ Boolean
  , sliderTranslateX ∷ Number
  , cardElementWidth ∷ Maybe Number
  , level ∷ DL.DeckLevel
  }

-- | A record used to represent card definitions in the deck.
type CardDef = { id ∷ CardId, ty ∷ CT.CardType }

-- | Constructs a default `State` value.
initialDeck ∷ DirPath → DeckId → State
initialDeck path deckId =
  { id: deckId
  , parent: Nothing
  , mirror: Nothing
  , fresh: 0
  , accessType: Editable
  , modelCards: mempty
  , displayCards: mempty
  , cardsToLoad: mempty
  , activeCardIndex: Nothing
  , path
  , saveTrigger: Nothing
  , globalVarMap: SM.empty
  , runTrigger: Nothing
  , pendingCard: Nothing
  , stateMode: Loading
  , displayMode: Normal
  , initialSliderX: Nothing
  , initialSliderCardWidth: Nothing
  , sliderTransition: false
  , sliderTranslateX: 0.0
  , cardElementWidth: Nothing
  , level: DL.root
  }

-- | The unique identifier of the deck. If it's a fresh, unsaved deck, the id
-- | will be Nothing.
_id ∷ ∀ a r. LensP {id ∷ a|r} a
_id = lens _.id _{id = _}

-- | A pointer to the parent deck/card. If `Nothing`, the deck is assumed to be
-- | the root deck.
_parent ∷ ∀ a r. LensP {parent ∷ a|r} a
_parent = lens _.parent _{parent = _}

-- | A pointer to the mirrored base of the deck. A mirror is a slice of some
-- | other deck, represented by a DeckId and a card offset.
_mirror ∷ ∀ a r. LensP {mirror ∷ a|r} a
_mirror = lens _.mirror _{mirror = _}

-- | A counter used to generate `CardId` values. This should be a monotonically increasing value
_fresh ∷ ∀ a r. LensP {fresh ∷ a|r} a
_fresh = lens _.fresh _{fresh = _}

-- | Determines whether the deck is editable.
_accessType ∷ ∀ a r. LensP {accessType ∷ a|r} a
_accessType = lens _.accessType _{accessType = _}

-- | The list of cards currently in the deck.
_modelCards ∷ ∀ a r. LensP {modelCards ∷ a |r} a
_modelCards = lens _.modelCards _{modelCards = _}

-- | The list of cards to be displayed in the deck
_displayCards ∷ ∀ a r. LensP {displayCards ∷ a |r} a
_displayCards = lens _.displayCards _{displayCards = _}

_cardsToLoad ∷ ∀ a r. LensP {cardsToLoad ∷ a |r} a
_cardsToLoad = lens _.cardsToLoad _{cardsToLoad = _}

-- | The `CardId` for the currently focused card. `Nothing` indicates the next
-- | action card.
_activeCardIndex ∷ ∀ a r. LensP {activeCardIndex ∷ a |r} a
_activeCardIndex = lens _.activeCardIndex _{activeCardIndex = _}

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
_level ∷ ∀ a r. LensP {level ∷ a|r} a
_level = lens _.level _{level = _}

addCard ∷ Card.AnyCardModel → State → State
addCard card st = fst $ addCard' card st

addCard' ∷ Card.AnyCardModel → State → State × CardId
addCard' model st =
  let
    cardId = CardId st.fresh
    newState = st
      { fresh = st.fresh + one
      , modelCards =
          A.snoc st.modelCards (st.id × { cardId, model })
      }
  in newState × cardId

removeCard ∷ DeckId × CardId → State → (Array (DeckId × Card.Model)) × State
removeCard coord st =
  Tuple displayCards.rest
    $ removePendingCard coord
    $ st
      { modelCards = modelCards.init
      , displayCards = displayCards.init
      , activeCardIndex = Just $ max zero $ A.length displayCards.init - 1
      }
  where
  modelCards = A.span (not ∘ eqCoordModel coord) st.modelCards
  displayCards = A.span (not ∘ eqCoordModel coord) st.displayCards

findLastRealCardIndex ∷ State → Maybe Int
findLastRealCardIndex =
  A.findLastIndex (Lens.has CID._CardId ∘ _.cardId ∘ snd)
    ∘ _.displayCards

findLastRealCard ∷ State → Maybe (DeckId × CardId)
findLastRealCard state =
  findLastRealCardIndex state
    >>= A.index state.displayCards
    <#> coordModelToCoord

-- | Finds the type of the last card.
findLastCardType ∷ State → Maybe CT.CardType
findLastCardType { displayCards } = Card.modelCardType ∘ _.model ∘ snd <$> A.last displayCards

apiCards ∷ State → Array (DeckId × CardId)
apiCards =
  _.modelCards ⋙ A.mapMaybe cardTypeMatches ⋙ foldMap pure
  where
  cardTypeMatches (deckId × { cardId, model }) =
    if Card.modelCardType model ≡ CT.API
       then Just (deckId × cardId)
       else Nothing

-- | Updates the stored card that is pending to run. This handles the logic of
-- | changing the pending card when a provided card appears earlier in the deck
-- | than the currently enqueued card, and if the provided card appears after
-- | the currently enqueued card the function is a no-op.
addPendingCard ∷ (DeckId × CardId) → State → State
addPendingCard coord st =
  case st.pendingCard of
    Nothing → st { pendingCard = Just coord }
    Just oldCoord → fromMaybe st $
      compareCoordCards coord oldCoord st.modelCards <#> eq LT <#>
        if _ then st { pendingCard = Just coord } else st

removePendingCard ∷ DeckId × CardId → State → State
removePendingCard coord st =
  fromMaybe st do
    oldCoord ← st.pendingCard
    comp ← (eq LT || eq EQ) <$> compareCoordCards coord oldCoord st.modelCards
    pure $ st { pendingCard = Nothing }

-- | Finds the current deck path
deckPath ∷ State → DirPath
deckPath state = deckPath' state.path state.id

deckPath' ∷ DirPath → DeckId → DirPath
deckPath' path deckId = path </> P.dir (deckIdToString deckId)

-- | Reconstructs a deck state from a deck model.
fromModel
  ∷ DirPath
  → DeckId
  → Model.Deck
  → State
  → State
fromModel path deckId { cards, parent } state =
  state
    { activeCardIndex = Nothing
    , displayMode = Normal
    , modelCards = Tuple deckId <$> cards
    , displayCards = mempty
    , fresh = fresh
    , globalVarMap = SM.empty
    , id = deckId
    , parent = parent
    , initialSliderX = Nothing
    , path = path
    , runTrigger = Nothing
    , pendingCard = Nothing
    }

  where
    fresh ∷ Int
    fresh = maybe 0 (_ + 1) $ maximum $ A.mapMaybe (Lens.preview CID._CardId ∘ _.cardId) cards

cardIndexFromCoord ∷ DeckId × CardId → State → Maybe Int
cardIndexFromCoord coord = A.findIndex (eqCoordModel coord) ∘ _.displayCards

cardFromIndex ∷ Int → State → Maybe (DeckId × Card.Model)
cardFromIndex i st = A.index st.displayCards i

cardCoordFromIndex ∷ Int → State → Maybe (DeckId × CardId)
cardCoordFromIndex vi = map (map _.cardId) ∘ cardFromIndex vi

activeCardCoord ∷ State → Maybe (DeckId × CardId)
activeCardCoord st = cardCoordFromIndex (fromMaybe 0 st.activeCardIndex) st

activeCardType ∷ State → Maybe CT.CardType
activeCardType st =
  Card.modelCardType ∘ _.model ∘ snd <$>
    cardFromIndex (fromMaybe 0 st.activeCardIndex) st

eqCoordModel ∷ DeckId × CardId → DeckId × Card.Model → Boolean
eqCoordModel (deckId × cardId) (deckId' × model) =
  deckId ≡ deckId' && cardId ≡ model.cardId

compareCoordCards
  ∷ DeckId × CardId
  → DeckId × CardId
  → Array (DeckId × Card.Model)
  → Maybe Ordering
compareCoordCards coordA coordB cards = 
  compare
    <$> A.findIndex (eqCoordModel coordA) cards
    <*> A.findIndex (eqCoordModel coordB) cards

coordModelToCoord ∷ DeckId × Card.Model → DeckId × CardId
coordModelToCoord = map _.cardId
