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
module SlamData.Workspace.Deck.Slider
  ( startSliding
  , stopSlidingAndSnap
  , updateSliderPosition
  , render
  , containerProperties
  ) where

import Control.Monad.Aff.Free (fromEff)

import SlamData.Prelude

import Data.Array ((..))
import Data.Array as Array
import Data.Int as Int
import Data.Lens ((.~), (^.))
import Data.Lens as Lens
import Data.Ord (max, min)
import Data.Tuple as Tuple

import CSS (CSS)

import DOM.HTML.Types (HTMLElement)

import Halogen as H
import Halogen.HTML.CSS.Indexed (style)
import Halogen.HTML.Events.Handler as HEH
import Halogen.HTML.Events.Indexed as HE
import Halogen.HTML.Events.Types (Event, MouseEvent)
import Halogen.HTML.Indexed as HH
import Halogen.HTML.Properties.Indexed (IProp(), I)
import Halogen.HTML.Properties.Indexed as HP
import Halogen.HTML.Properties.Indexed.ARIA as ARIA

import SlamData.Config as Config
import SlamData.Render.CSS as ClassNames
import SlamData.Workspace.AccessType as AccessType
import SlamData.Workspace.Card.CardId (CardId)
import SlamData.Workspace.Card.CardId as CardId
import SlamData.Workspace.Card.CardType as CT
import SlamData.Workspace.Card.Component as Card
import SlamData.Workspace.Card.Factory (cardTypeComponent)
import SlamData.Workspace.Card.Next.Component as Next
import SlamData.Workspace.Deck.Common (DeckHTML, DeckDSL)
import SlamData.Workspace.Deck.Component.ChildSlot as ChildSlot
import SlamData.Workspace.Deck.Component.Cycle (DeckComponent)
import SlamData.Workspace.Deck.Component.Query (Query)
import SlamData.Workspace.Deck.Component.Query as DCQ
import SlamData.Workspace.Deck.Component.State (VirtualState, State, CardDef)
import SlamData.Workspace.Deck.Component.State as DCS
import SlamData.Workspace.Deck.Gripper as Gripper

import Utils.CSS as CSSUtils
import Utils.DOM (getBoundingClientRect)

render ∷ DeckComponent → VirtualState → Boolean → DeckHTML
render comp vstate visible =
  HH.div
    ([ HP.key "deck-cards"
     , HP.classes [ ClassNames.cardSlider ]
     , HE.onTransitionEnd $ HE.input_ DCQ.StopSliderTransition
     , style
         $ (cardSliderTransformCSS state.activeCardIndex state.sliderTranslateX)
         *> (cardSliderTransitionCSS state.sliderTransition)
     ]
       ⊕ (guard (not visible) $> (HP.class_ ClassNames.invisible)))
    ((map (Tuple.uncurry (renderCard comp vstate)) (Array.zip state.cards (0 .. Array.length state.cards))) ⊕ [ renderNextActionCard vstate ])
  where
    state = DCS.runVirtualState vstate

stateStartSliding ∷ Event MouseEvent → Maybe Number → State → State
stateStartSliding mouseEvent cardWidth =
  (DCS._initialSliderX .~ Just mouseEvent.screenX)
    ∘ (DCS._initialSliderCardWidth .~ cardWidth)
    ∘ (DCS._sliderTransition .~ false)
    ∘ (DCS._displayMode .~ DCS.Normal)

startSliding ∷ Event MouseEvent → DeckDSL Unit
startSliding mouseEvent =
  getCardWidth
    >>= H.modify ∘ stateStartSliding mouseEvent

getCardWidth ∷ DeckDSL (Maybe Number)
getCardWidth =
  traverse getBoundingClientWidth
    =<< H.gets _.nextActionCardElement

getBoundingClientWidth ∷ HTMLElement → DeckDSL Number
getBoundingClientWidth =
  fromEff ∘ map _.width ∘ getBoundingClientRect

stateStopSlidingAndSnap ∷ Event MouseEvent → State → State
stateStopSlidingAndSnap mouseEvent =
  stateUpdateSliderPosition mouseEvent
    >>> startTransition
    >>> snap
    >>> stopSliding

stopSlidingAndSnap ∷ Event MouseEvent → DeckDSL Unit
stopSlidingAndSnap = H.modify ∘ stateStopSlidingAndSnap

stateUpdateSliderPosition ∷ Event MouseEvent → State → State
stateUpdateSliderPosition mouseEvent =
  maybe id (Lens.set DCS._sliderTranslateX ∘ translateXCalc mouseEvent.screenX)
    <$> _.initialSliderX
    <*> id

updateSliderPosition ∷ Event MouseEvent → DeckDSL Unit
updateSliderPosition = H.modify ∘ stateUpdateSliderPosition

translateXCalc ∷ Number → Number → Number
translateXCalc eventScreenX initialX =
  eventScreenX - initialX

stopSliding ∷ State → State
stopSliding =
    (DCS._initialSliderX .~ Nothing)
      ∘ (DCS._sliderTranslateX .~ 0.0)

snapActiveCardIndexByTranslationAndCardWidth ∷ VirtualState → Number → DCS.VirtualIndex → DCS.VirtualIndex
snapActiveCardIndexByTranslationAndCardWidth st cardWidth (DCS.VirtualIndex idx)
  | st ^. DCS._VirtualState ∘ DCS._sliderTranslateX <= -(offsetCardSpacing cardWidth / 2.0) =
    DCS.VirtualIndex $ min numberOfCards $ sub idx $ 1 + Int.floor ((translateX - (offsetCardSpacing cardWidth / 2.0)) / cardWidth)
    where
    numberOfCards = Array.length $ st ^. DCS._VirtualState ∘ DCS._cards
    translateX = st ^. DCS._VirtualState ∘ DCS._sliderTranslateX

  | st ^. DCS._VirtualState ∘ DCS._sliderTranslateX >= (offsetCardSpacing cardWidth / 2.0) =
    DCS.VirtualIndex $ max 0 $ add (1 + Int.floor ((-translateX - (offsetCardSpacing cardWidth / 2.0)) / cardWidth)) idx
    where
    translateX = st ^. DCS._VirtualState ∘ DCS._sliderTranslateX

  | otherwise =
    DCS.VirtualIndex idx

offsetCardSpacing ∷ Number → Number
offsetCardSpacing = add $ cardSpacingGridSquares * Config.gridPx

snapActiveCardIndex ∷ VirtualState → DCS.VirtualIndex
snapActiveCardIndex st =
  min idx $ maximumSnappingCardIndex st
  where
  idx = maybe id snap' (st ^. DCS._VirtualState ∘ DCS._initialSliderCardWidth) activeCardIndex
  snap' = snapActiveCardIndexByTranslationAndCardWidth st
  activeCardIndex = st ^. DCS._VirtualState ∘ DCS._activeCardIndex

-- We cannot snap to any card past a "blocking card".
maximumSnappingCardIndex ∷ VirtualState → DCS.VirtualIndex
maximumSnappingCardIndex st =
  case Array.findIndex (CT.blocking ∘ _.ty) cards of
    Just idx → DCS.VirtualIndex idx
    Nothing → DCS.VirtualIndex $ max 0 $ Array.length cards
  where
    cards = st ^. DCS._VirtualState ∘ DCS._cards

snap ∷ State → State
snap st = st # DCS._activeCardIndex .~ snapActiveCardIndex (DCS.virtualState st)

startTransition ∷ State → State
startTransition = DCS._sliderTransition .~ true

willChangeActiveCardWhenDropped ∷ VirtualState → Boolean
willChangeActiveCardWhenDropped vstate =
  st.activeCardIndex ≠ snapActiveCardIndex vstate
  where
    st = DCS.runVirtualState vstate

cardPositionCSS ∷ Int → CSS
cardPositionCSS index = do
  CSSUtils.left $ CSSUtils.calc $
    "(100% + " ⊕ show cardSpacingPx ⊕ "px) * " ⊕ show index

cardSliderTransformCSS ∷ DCS.VirtualIndex → Number → CSS
cardSliderTransformCSS activeCardIndex translateX =
  CSSUtils.transform
    $ CSSUtils.translate3d (cardSliderTranslateX activeCardIndex translateX) "0" "0"

cardSliderTransitionCSS ∷ Boolean → CSS
cardSliderTransitionCSS false = CSSUtils.transition "none"
cardSliderTransitionCSS true = CSSUtils.transition "all 0.33s"

cardSliderTranslateX ∷ DCS.VirtualIndex → Number → String
cardSliderTranslateX (DCS.VirtualIndex activeCardIndex) translateX =
  CSSUtils.calc
    $ "(-100% - " ⊕ show cardSpacingPx ⊕ "px)"
    ⊕ " * " ⊕ show activeCardIndex
    ⊕ " + " ⊕ show translateX ⊕ "px"

dropEffect ∷ Boolean → String
dropEffect true = "execute"
dropEffect false = "none"

containerProperties ∷ ∀ a. VirtualState → Array (IProp (onMouseUp ∷ I, onMouseLeave ∷ I, onMouseMove ∷ I | a) (Query Unit))
containerProperties vstate =
  [ ARIA.dropEffect $ dropEffect $ willChangeActiveCardWhenDropped vstate ]
    ⊕ (guard (isJust initialSliderX)
         $> (HE.onMouseUp \e → HEH.preventDefault $> Just (H.action (DCQ.StopSlidingAndSnap e))))
    ⊕ (guard (isJust initialSliderX)
         $> (HE.onMouseLeave \e → HEH.stopPropagation $> HEH.preventDefault $> Just (H.action (DCQ.StopSlidingAndSnap e))))
    ⊕ (guard (isJust initialSliderX)
         $> (HE.onMouseMove $ HE.input DCQ.UpdateSliderPosition))
  where
  initialSliderX = vstate ^. DCS._VirtualState ∘ DCS._initialSliderX


cardSelected ∷ VirtualState → CardId → Boolean
cardSelected state cardId =
  Just cardId ≡ DCS.activeCardId state

cardProperties ∷ ∀ a b. VirtualState → CardId → Array (IProp a b)
cardProperties state cardId =
  [ ARIA.disabled ∘ show ∘ not $ cardSelected state cardId ]

cardSpacingGridSquares ∷ Number
cardSpacingGridSquares = 2.0

cardSpacingPx ∷ Number
cardSpacingPx = cardSpacingGridSquares * Config.gridPx

renderCard ∷ DeckComponent → VirtualState → CardDef → Int → DeckHTML
renderCard comp vstate cardDef index =
  HH.div
    [ HP.key ("card" ⊕ CardId.cardIdToString cardDef.id)
    , HP.classes [ ClassNames.card ]
    , style $ cardPositionCSS index
    ]
    $ Gripper.renderGrippers
        (cardSelected vstate cardDef.id)
        (isJust state.initialSliderX)
        (Gripper.gripperDefsForCardId state.cards $ Just cardDef.id)
        ⊕ [ HH.div
              (cardProperties vstate cardDef.id)
              [ HH.slot' ChildSlot.cpCard slotId \_ → cardComponent ]
           ]
  where
  state = DCS.runVirtualState vstate
  slotId = ChildSlot.CardSlot cardDef.id
  cardComponent =
    { component: cardTypeComponent cardDef.ty cardDef.id state.browserFeatures comp
    , initialState:
        H.parentState
          Card.initialCardState { accessType = state.accessType }
    }

renderNextActionCard ∷ VirtualState → DeckHTML
renderNextActionCard vstate =
  HH.div
    ([ HP.key ("next-action-card")
     , HP.classes [ ClassNames.card ]
     , HP.ref (H.action ∘ DCQ.SetNextActionCardElement)
     , style ∘ cardPositionCSS $ Array.length state.cards
     ]
       ⊕ (guard (shouldHideNextActionCard vstate) $> (HP.class_ ClassNames.invisible))
    )
    (Gripper.renderGrippers
       (isNothing $ DCS.activeCardId vstate)
       (isJust state.initialSliderX)
       (Gripper.gripperDefsForCardId state.cards Nothing)
       ⊕ [ HH.slot' ChildSlot.cpCard (ChildSlot.CardSlot top) \_ →
             { component: Next.nextCardComponent
             , initialState: H.parentState Card.initialCardState
             }
         ]
    )
  where
  state = DCS.runVirtualState vstate

shouldHideNextActionCard ∷ VirtualState → Boolean
shouldHideNextActionCard vstate =
  (DCS.runVirtualState vstate).accessType ≡ AccessType.ReadOnly
