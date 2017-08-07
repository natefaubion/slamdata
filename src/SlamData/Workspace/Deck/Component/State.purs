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
  ( State
  , DisplayMode(..)
  , ResponsiveSize(..)
  , Fade(..)
  , MetaCard(..)
  , DisplayCard
  , CardDef
  , initialState
  , isFlipSide
  , isFrontSide
  , _name
  , _displayCards
  , _activeCardIndex
  , _pendingCardIndex
  , _presentAccessNextActionCardHintCanceler
  , _focusDeckHintDismissed
  , _focusDeckFrameHintDismissed
  , _presentAccessNextActionCardHint
  , _loadError
  , _displayMode
  , _initialSliderX
  , _initialSliderCardWidth
  , _sliderTransition
  , _sliderTranslateX
  , _slidingTo
  , _breakers
  , _focused
  , _responsiveSize
  , _fadeTransition
  , _providers
  , _PendingCard
  , _NextActionCard
  , _ErrorCard
  , addMetaCard
  , findLastCardIndex
  , findLastCard
  , findLastRealCard
  , fromModel
  , cardIndexFromId
  , cardIdFromIndex
  , activeCard
  , activeCardIndex
  , prevCardId
  , eqDisplayCard
  , compareCardIndex
  , updateCompletedCards
  ) where

import SlamData.Prelude

import Control.Monad.Aff (Canceler)

import Data.Array as A
import Data.Lens (Lens', lens, Prism', prism')

import Quasar.Advanced.Types (ProviderR)

import SlamData.Effects (SlamDataEffects)
import SlamData.Workspace.Card.CardId (CardId)
import SlamData.Workspace.Card.CardType (CardType)
import SlamData.Workspace.Card.Error as CE
import SlamData.Workspace.Card.Port as Port
import SlamData.Workspace.Deck.Gripper.Def (GripperDef)

data DisplayMode
  = FrontSide
  | FlipSide

data ResponsiveSize
  = XSmall
  | Small
  | Medium
  | Large
  | XLarge
  | XXLarge

data MetaCard
  = PendingCard
  | ErrorCard CE.CardError
  | NextActionCard Port.Out

data Fade
  = FadeNone
  | FadeIn
  | FadeOut

derive instance eqDisplayMode ∷ Eq DisplayMode

derive instance eqResponsiveSize ∷ Eq ResponsiveSize

derive instance eqFade ∷ Eq Fade

isFlipSide ∷ DisplayMode → Boolean
isFlipSide = (_ == FlipSide)

isFrontSide ∷ DisplayMode → Boolean
isFrontSide = (_ == FrontSide)

type CardDef =
  { cardId ∷ CardId
  , cardType ∷ CardType
  }

type DisplayCard = Either MetaCard CardDef

type State =
  { name ∷ String
  , loadError ∷ Maybe String
  , displayMode ∷ DisplayMode
  , displayCards ∷ Array DisplayCard
  , pendingCardIndex ∷ Maybe Int
  , activeCardIndex ∷ Maybe Int
  , presentAccessNextActionCardHintCanceler ∷ Maybe (Canceler SlamDataEffects)
  , presentAccessNextActionCardHint ∷ Boolean
  , initialSliderX ∷ Maybe Number
  , initialSliderCardWidth ∷ Maybe Number
  , sliderTransition ∷ Boolean
  , sliderTranslateX ∷ Number
  , slidingTo ∷ Maybe GripperDef
  , focused ∷ Boolean
  , finalized ∷ Boolean
  , responsiveSize ∷ ResponsiveSize
  , cardDimensions ∷ { width ∷ Number, height ∷ Number }
  , fadeTransition ∷ Fade
  , providers ∷ Array ProviderR
  , focusDeckHintDismissed ∷ Boolean
  , focusDeckFrameHintDismissed ∷ Boolean
  }

initialState ∷ State
initialState =
  { name: ""
  , loadError: Nothing
  , displayMode: FrontSide
  , displayCards: mempty
  , pendingCardIndex: Nothing
  , activeCardIndex: Nothing
  , presentAccessNextActionCardHintCanceler: Nothing
  , presentAccessNextActionCardHint: false
  , initialSliderX: Nothing
  , initialSliderCardWidth: Nothing
  , sliderTransition: false
  , sliderTranslateX: 0.0
  , slidingTo: Nothing
  , focused: false
  , finalized: false
  , responsiveSize: XLarge
  , cardDimensions: { width: 0.0, height: 0.0 }
  , fadeTransition: FadeNone
  , providers: mempty
  , focusDeckHintDismissed: true
  , focusDeckFrameHintDismissed: true
  }

-- | The name of the deck. Initially Nothing.
_name ∷ ∀ a r. Lens' {name ∷ a|r} a
_name = lens _.name _{name = _}

-- | The list of cards to be displayed in the deck
_displayCards ∷ ∀ a r. Lens' {displayCards ∷ a |r} a
_displayCards = lens _.displayCards _{displayCards = _}

-- | The `CardId` for the currently focused card. `Nothing` indicates the next
-- | action card.
_activeCardIndex ∷ ∀ a r. Lens' {activeCardIndex ∷ a |r} a
_activeCardIndex = lens _.activeCardIndex _{activeCardIndex = _}

-- | The index for the currently pending card. `Nothing` indicates no cards
-- | are pending.
_pendingCardIndex ∷ ∀ a r. Lens' {pendingCardIndex ∷ a |r} a
_pendingCardIndex = lens _.pendingCardIndex _{pendingCardIndex = _}

-- | An optional canceler for the delayed guiding of the user to add a card. Can
-- | be used to reset the delay of this guiding.
_presentAccessNextActionCardHintCanceler ∷ ∀ a r. Lens' {presentAccessNextActionCardHintCanceler ∷ a |r} a
_presentAccessNextActionCardHintCanceler = lens _.presentAccessNextActionCardHintCanceler _{presentAccessNextActionCardHintCanceler = _}

-- | Whether the add card guide should be presented or not.
_presentAccessNextActionCardHint ∷ ∀ a r. Lens' {presentAccessNextActionCardHint ∷ a |r} a
_presentAccessNextActionCardHint = lens _.presentAccessNextActionCardHint _{presentAccessNextActionCardHint = _}

-- | Whether there was an error loading the deck.
_loadError ∷ ∀ a r. Lens' {loadError ∷ a|r} a
_loadError = lens _.loadError _{loadError = _}

-- | backsided, dialog or normal (card)
_displayMode ∷ ∀ a r. Lens' {displayMode ∷ a |r} a
_displayMode = lens (_.displayMode) (_{displayMode = _})

-- | The x position of the card slider at the start of the slide interaction in
-- | pixels. If `Nothing` slide interaction is not in progress.
_initialSliderX ∷ ∀ a r. Lens' {initialSliderX ∷ a|r} a
_initialSliderX = lens _.initialSliderX _{initialSliderX = _}

-- | The width of the next action card at the start of the slide interaction in
-- | pixels. If `Nothing` either the slide interaction is not in progress or the
-- | next action card element reference is broken.
_initialSliderCardWidth ∷ ∀ a r. Lens' {initialSliderCardWidth ∷ a|r} a
_initialSliderCardWidth = lens _.initialSliderCardWidth _{initialSliderCardWidth = _}

-- | Whether the translation of the card slider should be animated or not.
-- | Should be true between the end of the slide interaction and the end of the
-- | transition.
_sliderTransition ∷ ∀ a r. Lens' {sliderTransition ∷ a |r} a
_sliderTransition = lens _.sliderTransition _{sliderTransition = _}

-- | The current x translation of the card slider during the slide interaction.
_sliderTranslateX ∷ ∀ a r. Lens' {sliderTranslateX ∷ a |r} a
_sliderTranslateX = lens _.sliderTranslateX _{sliderTranslateX = _}

_slidingTo ∷ ∀ a r. Lens' {slidingTo ∷ a|r} a
_slidingTo = lens _.slidingTo _{slidingTo = _}

_breakers ∷ ∀ a r. Lens' {breakers ∷ a|r} a
_breakers = lens _.breakers _{breakers = _}

_focused ∷ ∀ a r. Lens' {focused ∷ a|r} a
_focused = lens _.focused _{focused = _}

_responsiveSize ∷ ∀ a r. Lens' {responsiveSize ∷ a|r} a
_responsiveSize = lens _.responsiveSize _{responsiveSize = _}

_fadeTransition ∷ ∀ a r. Lens' {fadeTransition ∷ a|r} a
_fadeTransition = lens _.fadeTransition _{fadeTransition = _}

_providers ∷ ∀ a r. Lens' {providers ∷ a|r} a
_providers = lens _.providers _{providers = _}

_focusDeckHintDismissed ∷ ∀ a r. Lens' {focusDeckHintDismissed ∷ a|r} a
_focusDeckHintDismissed = lens _.focusDeckHintDismissed _{focusDeckHintDismissed = _}

_focusDeckFrameHintDismissed ∷ ∀ a r. Lens' {focusDeckFrameHintDismissed ∷ a|r} a
_focusDeckFrameHintDismissed = lens _.focusDeckFrameHintDismissed _{focusDeckFrameHintDismissed = _}

_NextActionCard ∷ Prism' MetaCard Port.Out
_NextActionCard = prism' NextActionCard case _ of
  NextActionCard a → Just a
  _ → Nothing

_ErrorCard ∷ Prism' MetaCard CE.CardError
_ErrorCard = prism' ErrorCard case _ of
  ErrorCard a → Just a
  _ → Nothing

_PendingCard ∷ Prism' MetaCard Unit
_PendingCard = prism' (const PendingCard) case _ of
  PendingCard → Just unit
  _ → Nothing

addMetaCard ∷ MetaCard → State → State
addMetaCard card state =
  state { displayCards = A.snoc init (Left card) }
  where
  init = A.filter isRight state.displayCards

findLastCardIndex ∷ State → Maybe Int
findLastCardIndex st =
  const (A.length st.displayCards - 1) <$> A.last st.displayCards

findLastCard ∷ State → Maybe DisplayCard
findLastCard state =
  A.last state.displayCards

findLastRealCard ∷ State → Maybe CardDef
findLastRealCard state =
  case A.index state.displayCards (A.length state.displayCards - 2) of
    Just (Right def) → Just def
    _ → Nothing

-- | Reconstructs a deck state from a deck model.
fromModel
  ∷ { name ∷ String
    , displayCards ∷ Array DisplayCard
    }
  → State
  → State
fromModel { name, displayCards } state =
  state
    { name = name
    , displayCards = displayCards
    , displayMode = FrontSide
    , activeCardIndex = Nothing
    , initialSliderX = Nothing
    }

cardIndexFromId ∷ CardId → State → Maybe Int
cardIndexFromId cardId = cardIndexFromId' cardId ∘ _.displayCards

cardIndexFromId' ∷ CardId → Array DisplayCard → Maybe Int
cardIndexFromId' cardId =
  A.findIndex (eq (Just cardId) ∘ map _.cardId ∘ hush)

cardIdFromIndex ∷ Int → State → Maybe CardId
cardIdFromIndex i st =
  A.index st.displayCards i >>= either (const Nothing) (Just ∘ _.cardId)

activeCard ∷ State → Maybe DisplayCard
activeCard st = A.index st.displayCards (activeCardIndex st)

activeCardIndex ∷ State → Int
activeCardIndex st =
  case st.activeCardIndex of
    Just ix | ix < ix' → ix
    _ → ix'
  where
    len = A.length st.displayCards
    ix' = if len <= 0 then 0 else len - 1

prevCardId ∷ CardId → State → Maybe CardId
prevCardId cardId st = do
  i ← cardIndexFromId cardId st
  cardIdFromIndex (i - 1) st

eqDisplayCard ∷ DisplayCard → DisplayCard → Boolean
eqDisplayCard (Right r1) (Right r2) = r1.cardId ≡ r2.cardId
eqDisplayCard (Left l1) (Left l2) = eqMetaCard l1 l2
eqDisplayCard _ _ = false

eqMetaCard ∷ MetaCard → MetaCard → Boolean
eqMetaCard = case _, _ of
  PendingCard, PendingCard → true
  ErrorCard _, ErrorCard _ → true
  NextActionCard _, NextActionCard _ → true
  _, _ → false

compareCardIndex
  ∷ CardId
  → CardId
  → Array CardId
  → Maybe Ordering
compareCardIndex a b cards =
  compare
    <$> A.findIndex (eq a) cards
    <*> A.findIndex (eq b) cards

updateCompletedCards ∷ Array CardDef → Port.Out → State → State
updateCompletedCards defs out@(port × _) st =
  st
    { displayCards = map Right realCards <> [ Left metaCard ]
    , activeCardIndex = activeIndex
    , pendingCardIndex = Nothing
    }
  where
  metaCard = case port of
    Port.CardError err → ErrorCard err
    _ → NextActionCard out

  realCards = case A.head defs of
    Nothing → []
    Just { cardId } →
      A.takeWhile (not ∘ eq cardId ∘ _.cardId)
        (A.mapMaybe hush st.displayCards)
        <> defs

  activeIndex = case st.activeCardIndex, metaCard, A.length realCards of
    -- If we already have an activeIndex, we should always preserve that.
    -- We'll rely on the renderer to clamp as necessary when there are
    -- too few cards.
    Just ix, _, _ → Just ix
    -- We should show errors, but we don't want to set the active index.
    -- If the error card is resolved through some means we should then show
    -- the top card.
    _, ErrorCard _, _ → Nothing
    -- If there are no real cards, we should go ahead and set the active
    -- index to the meta card. This ensures that when we add a card, we'll
    -- see the new card instead of an error (if there is one).
    _, _, 0 → Just 0
    -- If there are real cards, we should set it to the last real card.
    _, _, n → Just (n - 1)
