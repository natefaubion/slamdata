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

import SlamData.Prelude

import Data.Array ((..))
import Data.Array as Array
import Data.Int as Int
import Data.Lens ((.~), (?~))
import Data.Lens as Lens
import Data.Ord (max, min)
import Data.Tuple as Tuple

import CSS (CSS)

import Halogen as H
import Halogen.HTML.CSS.Indexed (style)
import Halogen.HTML.Events.Indexed as HE
import Halogen.HTML.Events.Types (Event, MouseEvent)
import Halogen.HTML.Indexed as HH
import Halogen.HTML.Properties.Indexed (IProp(), I)
import Halogen.HTML.Properties.Indexed as HP
import Halogen.HTML.Properties.Indexed.ARIA as ARIA

import SlamData.Config as Config
import SlamData.Render.CSS as ClassNames
import SlamData.Workspace.Card.CardId (CardId)
import SlamData.Workspace.Card.CardId as CardId
import SlamData.Workspace.Card.Model as Card
import SlamData.Workspace.Card.Component as CardC
import SlamData.Workspace.Card.Factory as Factory
import SlamData.Workspace.Deck.Common (DeckHTML, DeckDSL)
import SlamData.Workspace.Deck.Component.ChildSlot as ChildSlot
import SlamData.Workspace.Deck.Component.Cycle (DeckComponent)
import SlamData.Workspace.Deck.Component.Query (Query)
import SlamData.Workspace.Deck.Component.Query as DCQ
import SlamData.Workspace.Deck.Component.State (State)
import SlamData.Workspace.Deck.Component.State as DCS
import SlamData.Workspace.Deck.DeckId (DeckId)
import SlamData.Workspace.Deck.DeckId as DeckId
import SlamData.Workspace.Deck.Gripper as Gripper
import SlamData.Workspace.Deck.Gripper.Def (GripperDef(..))
import SlamData.Workspace.Wiring (Wiring)

import Utils.CSS as CSSUtils

render ∷ Wiring → DeckComponent → State → Boolean → DeckHTML
render wiring comp st visible =
  HH.div
    ([ HP.key "deck-cards"
     , HP.classes [ ClassNames.cardSlider ]
     , HE.onTransitionEnd $ HE.input_ DCQ.StopSliderTransition
     , style do
         cardSliderTransformCSS (fromMaybe 0 st.activeCardIndex) st.sliderTranslateX
         cardSliderTransitionCSS st.sliderTransition
     ]
     ⊕ (guard (not visible) $> (HP.class_ ClassNames.invisible)))
    $ map (Tuple.uncurry $ renderCard wiring comp st)
    $ Array.zip st.displayCards (0 .. Array.length st.displayCards)

startSliding ∷ Event MouseEvent → GripperDef → DeckDSL Unit
startSliding mouseEvent gDef = do
  cardWidth ← H.gets _.cardElementWidth
  H.modify
    $ (DCS._initialSliderX .~ Just mouseEvent.screenX)
    ∘ (DCS._initialSliderCardWidth .~ cardWidth)
    ∘ (DCS._sliderTransition .~ false)
    ∘ (DCS._displayMode .~ DCS.Normal)
    ∘ (DCS._slidingTo ?~ gDef)

stopSlidingAndSnap ∷ Event MouseEvent → DeckDSL Unit
stopSlidingAndSnap mEvent =
  H.modify
    $ stopSliding
    ∘ snap
    ∘ stateUpdateSliderPosition mEvent

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
  ∘ (DCS._slidingTo .~ Nothing)
  ∘ startTransition

clickBound ∷ Number
clickBound = 4.0

isClick ∷ Number -> Boolean
isClick n = n < clickBound ∧ n > -clickBound

snapActiveCardIndexByTranslationAndCardWidth
  ∷ State
  → Number
  → Int
  → Int
snapActiveCardIndexByTranslationAndCardWidth st cardWidth idx =
  let
    translateX = st.sliderTranslateX

    numberOfCards = Array.length st.displayCards

    halfOffset = (offsetCardSpacing cardWidth) / 2.0

    result | isClick translateX  = case st.slidingTo of
      Just (Previous true) → min (numberOfCards - 1) $ idx - one
      Just (Next true) → max 0 $ idx + one
      _ → idx
    result | translateX <= -1.0 * halfOffset =
      min (numberOfCards - 1)
      $ sub idx
      $ one
      + Int.floor ((translateX - halfOffset) / cardWidth)
    result | translateX >= halfOffset =
      max 0
      $ idx
      + one
      + Int.floor ((-translateX - halfOffset) / cardWidth)
    result | otherwise =
      idx
  in
    result

offsetCardSpacing ∷ Number → Number
offsetCardSpacing = add $ cardSpacingGridSquares * Config.gridPx

snapActiveCardIndex ∷ State → Int
snapActiveCardIndex st = maybe id snap' st.initialSliderCardWidth $ fromMaybe 0 st.activeCardIndex
  where
  snap' = snapActiveCardIndexByTranslationAndCardWidth st

snap ∷ State → State
snap st = st { activeCardIndex = Just $ snapActiveCardIndex st }

startTransition ∷ State → State
startTransition = DCS._sliderTransition .~ true

willChangeActiveCardWhenDropped ∷ State → Boolean
willChangeActiveCardWhenDropped st =
  fromMaybe 0 st.activeCardIndex ≠ snapActiveCardIndex st

cardPositionCSS ∷ Int → CSS
cardPositionCSS index = do
  CSSUtils.left $ CSSUtils.calc $
    "(100% + " ⊕ show cardSpacingPx ⊕ "px) * " ⊕ show index

cardSliderTransformCSS ∷ Int → Number → CSS
cardSliderTransformCSS activeCardIndex translateX =
  CSSUtils.transform
    $ CSSUtils.translate3d (cardSliderTranslateX activeCardIndex translateX) "0" "0"

cardSliderTransitionCSS ∷ Boolean → CSS
cardSliderTransitionCSS false = CSSUtils.transition "none"
cardSliderTransitionCSS true = CSSUtils.transition "all 0.125s"

cardSliderTranslateX ∷ Int → Number → String
cardSliderTranslateX activeCardIndex translateX =
  CSSUtils.calc
    $ "(-100% - " ⊕ show cardSpacingPx ⊕ "px)"
    ⊕ " * " ⊕ show activeCardIndex
    ⊕ " + " ⊕ show translateX ⊕ "px"

dropEffect ∷ Boolean → String
dropEffect true = "execute"
dropEffect false = "none"

containerProperties
  ∷ ∀ a
  . State
  → Array (IProp (onMouseUp ∷ I, onMouseLeave ∷ I, onMouseMove ∷ I | a) (Query Unit))
containerProperties st =
  [ ARIA.dropEffect $ dropEffect $ willChangeActiveCardWhenDropped st ]
    ⊕ (guard (isJust st.initialSliderX)
         $> (HE.onMouseUp \e →
                pure $ Just (H.action (DCQ.StopSlidingAndSnap e))))
    ⊕ (guard (isJust st.initialSliderX)
         $> (HE.onMouseLeave \e →
                pure $ Just (H.action (DCQ.StopSlidingAndSnap e))))
    ⊕ (guard (isJust st.initialSliderX)
         $> (HE.onMouseMove $ HE.input DCQ.UpdateSliderPosition))

cardSelected ∷ State → DeckId × CardId → Boolean
cardSelected state coord =
  Just coord ≡ DCS.activeCardCoord state

cardProperties ∷ ∀ a b. State → DeckId × CardId → Array (IProp a b)
cardProperties state coord =
  [ ARIA.disabled ∘ show ∘ not $ cardSelected state coord ]

cardSpacingGridSquares ∷ Number
cardSpacingGridSquares = 2.0

cardSpacingPx ∷ Number
cardSpacingPx = cardSpacingGridSquares * Config.gridPx

renderCard ∷ Wiring → DeckComponent → State → (DeckId × Card.Model) → Int → DeckHTML
renderCard wiring comp st (deckId × card) index =
  HH.div
    ([ HP.key key
    , HP.classes classes
    , style $ cardPositionCSS index
    ])
    $ Gripper.renderGrippers
        (cardSelected st (deckId × card.cardId))
        (isJust st.initialSliderX)
        (Gripper.gripperDefsForCard st.displayCards $ Just coord)
        ⊕ [ HH.div
              (cardProperties st coord)
              [ HH.slot' ChildSlot.cpCard slotId \_ → cardComponent ]
           ]
  where
  key = "card-" ⊕ DeckId.deckIdToString deckId ⊕ "-" ⊕ CardId.cardIdToString card.cardId
  classes =
    [ ClassNames.card ]
      ⊕ (guard (not $ isClick st.sliderTranslateX) $> ClassNames.cardSliding)
      ⊕ (guard (cardSelected st (deckId × card.cardId)) $> ClassNames.cardActive)
  coord = deckId × card.cardId
  slotId = ChildSlot.CardSlot coord
  cardOpts =
    { deckComponent: comp
    , path: st.path
    , cardId: card.cardId
    , deckId: st.id
    , level: st.level
    , accessType: st.accessType
    , wiring
    }

  cardComponent =
    { component: Factory.cardComponent card cardOpts
    , initialState: H.parentState CardC.initialCardState
    }
