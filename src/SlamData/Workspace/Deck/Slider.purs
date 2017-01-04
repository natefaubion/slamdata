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

import Data.Array as Array
import Data.Int as Int
import Data.Lens ((.~), (?~))
import Data.Lens as Lens
import Data.List ((:))
import Data.String as String

import CSS (CSS)

import Halogen as H
import Halogen.HTML.CSS.Indexed (style)
import Halogen.HTML.Events.Indexed as HE
import Halogen.HTML.Events.Types (Event, MouseEvent)
import Halogen.HTML.Indexed as HH
import Halogen.HTML.Properties.Indexed (IProp(), I)
import Halogen.HTML.Properties.Indexed as HP
import Halogen.HTML.Properties.Indexed.ARIA as ARIA

import SlamData.Render.CSS as ClassNames
import SlamData.Guide as Guide
import SlamData.Workspace.AccessType as AT
import SlamData.Workspace.Card.CardId as CardId
import SlamData.Workspace.Card.Component as CardC
import SlamData.Workspace.Card.Component.CSS as CardCSS
import SlamData.Workspace.Card.InsertableCardType as ICT
import SlamData.Workspace.Card.Factory as Factory
import SlamData.Workspace.Card.Next.Component as Next
import SlamData.Workspace.Card.Error.Component as Error
import SlamData.Workspace.Card.Pending.Component as Pending
import SlamData.Workspace.Deck.Common (DeckOptions, DeckHTML, DeckDSL)
import SlamData.Workspace.Deck.Component.ChildSlot as ChildSlot
import SlamData.Workspace.Deck.Component.Cycle (DeckComponent)
import SlamData.Workspace.Deck.Component.Query (Query)
import SlamData.Workspace.Deck.Component.Query as DCQ
import SlamData.Workspace.Deck.Component.State (State)
import SlamData.Workspace.Deck.Component.State as DCS
import SlamData.Workspace.Deck.Gripper as Gripper
import SlamData.Workspace.Deck.Gripper.Def (GripperDef(..))

import Utils.CSS as CSSUtils

render ∷ DeckOptions → (DeckOptions → DeckComponent) → State → Boolean → DeckHTML
render opts deckComponent st visible =
  HH.div
    [ HP.key "deck-cards"
    , HP.classes ([ ClassNames.cardSlider ] ⊕ (guard (not visible) $> ClassNames.invisible))
    , HE.onTransitionEnd $ HE.input_ DCQ.StopSliderTransition
    , style do
        cardSliderTransformCSS (DCS.activeCardIndex st) st.sliderTranslateX
        cardSliderTransitionCSS st.sliderTransition
    ]
    (Array.mapWithIndex maybeRenderCard st.displayCards)
  where
  activeIndex =
    DCS.activeCardIndex st

  nestedLayout =
    case opts.displayCursor of
      _ : _ : _ → true
      _ → false

  maybeRenderCard ∷ Int → DCS.DisplayCard → DeckHTML
  maybeRenderCard index card
    | nestedLayout && index ≠ activeIndex = HH.text ""
    | otherwise = renderCard opts deckComponent st activeIndex index card

startSliding ∷ Event MouseEvent → GripperDef → DeckDSL Unit
startSliding mouseEvent gDef = do
  cardWidth ← H.gets _.cardElementWidth
  H.modify
    $ (DCS._initialSliderX .~ Just mouseEvent.screenX)
    ∘ (DCS._initialSliderCardWidth .~ cardWidth)
    ∘ (DCS._sliderTransition .~ false)
    ∘ (DCS._fadeTransition .~ DCS.FadeIn)
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
  ∘ (DCS._fadeTransition .~ DCS.FadeOut)
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
offsetCardSpacing = add cardSpacingPx

snapActiveCardIndex ∷ State → Int
snapActiveCardIndex st = maybe id snap' st.initialSliderCardWidth $ DCS.activeCardIndex st
  where
  snap' = snapActiveCardIndexByTranslationAndCardWidth st

snap ∷ State → State
snap st = st { activeCardIndex = Just $ snapActiveCardIndex st }

startTransition ∷ State → State
startTransition = DCS._sliderTransition .~ true

willChangeActiveCardWhenDropped ∷ State → Boolean
willChangeActiveCardWhenDropped st =
  DCS.activeCardIndex st ≠ snapActiveCardIndex st

cardPositionCSS ∷ Int → CSS
cardPositionCSS index = do
  CSSUtils.left $ CSSUtils.calc $
    "(100% + " ⊕ show cardSpacingPx ⊕ "px) * " ⊕ show index

cardSliderTransformCSS ∷ Int → Number → CSS
cardSliderTransformCSS activeCardIndex translateX =
  CSSUtils.transform
    $ CSSUtils.translate3d ((show (-100 * activeCardIndex)) ⊕ "%") "0" "0"
    ⊕ CSSUtils.translate3d ((show (-cardSpacingPx * Int.toNumber activeCardIndex)) ⊕ "px") "0" "0"
    ⊕ CSSUtils.translate3d ((show translateX) ⊕ "px") "0" "0"

cardSliderTransitionCSS ∷ Boolean → CSS
cardSliderTransitionCSS false = CSSUtils.transition "none"
cardSliderTransitionCSS true = CSSUtils.transition "all 0.125s"

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

cardSelected ∷ State → DCS.DisplayCard → Boolean
cardSelected state card =
  fromMaybe true $
    DCS.eqDisplayCard card <$> DCS.activeCard state

cardPending ∷ State → DCS.DisplayCard → Boolean
cardPending state (Left _) = false
cardPending state (Right { cardId }) =
  case state.pendingCardIndex, DCS.cardIndexFromId cardId state of
    Just ix, Just ix' | ix < ix' → true
    _, _ → false

cardProperties ∷ ∀ a b. State → DCS.DisplayCard → Array (IProp a b)
cardProperties state card =
  [ ARIA.disabled ∘ show ∘ not $ cardSelected state card ]

cardSpacingGridSquares ∷ Number
cardSpacingGridSquares = 2.0

cardSpacingPx ∷ Number
cardSpacingPx = cardSpacingGridSquares * 24.0

cardKey ∷ DCS.DisplayCard → String
cardKey = case _ of
  Left DCS.PendingCard → "card-pending"
  Left (DCS.ErrorCard _) → "card-error"
  Left (DCS.NextActionCard _) → "card-next-action"
  Right { cardId } → "card-" ⊕ CardId.toString cardId

renderCard
  ∷ DeckOptions
  → (DeckOptions → DeckComponent)
  → State
  → Int
  → Int
  → DCS.DisplayCard
  → DeckHTML
renderCard opts deckComponent st activeIndex index card =
  HH.div
    [ HP.key (cardKey card)
    , HP.classes classes
    , style $ cardPositionCSS index
    ]
    if opts.accessType == AT.ReadOnly
    then
      [ HH.div (cardProperties st card) cardComponent
      , loadingPanel
      ]
    else
      Gripper.renderGrippers
        (cardSelected st card)
        (isJust st.initialSliderX)
        (Gripper.gripperDefsForCard st.displayCards card)
        ⊕ [ HH.div
              (cardProperties st card)
              (cardComponent ⊕ (guard presentAccessNextActionCardGuide $> renderGuide))
          , loadingPanel
          ]
  where
  cardComponent = pure case card of
    Left m → renderMeta st m
    Right cd → renderDef opts deckComponent st cd

  classes =
    [ ClassNames.card
    , HH.className case st.fadeTransition of
        DCS.FadeIn   → "sd-fade-in"
        DCS.FadeOut  → "sd-fade-out"
        DCS.FadeNone → "sd-fade-none"
    ]
      ⊕ (guard (not $ isClick st.sliderTranslateX) $> ClassNames.cardSliding)
      ⊕ (guard (cardSelected st card) $> ClassNames.cardActive)
      ⊕ (guard (cardPending st card) $> ClassNames.pending)

  renderGuide =
    Guide.render
      Guide.RightArrow
      (HH.className "sd-access-next-card-guide")
      DCQ.HideAccessNextActionCardGuide
      guideText

  outputs = case card of
    Right { cardType } → ICT.outputsFor (ICT.fromCardType cardType)
    _ → []

  guideText =
    outputs
      # map ICT.printIOType'
      # Array.catMaybes
      # String.joinWith " ? "
      # guideText'

  guideText' outputTypesString =
    "To do more with "
      ⊕ outputTypesString
      ⊕ " click or drag this gripper to the left and add a new card to the deck."

  isLastCard =
    fromMaybe false $
      DCS.eqDisplayCard card <$> DCS.findLastRealCard st

  presentAccessNextActionCardGuide =
    st.presentAccessNextActionCardGuide ∧ isLastCard ∧ st.focused

renderMeta
  ∷ State
  → DCS.MetaCard
  → DeckHTML
renderMeta st card =
  HH.div
    [ HP.classes [ CardCSS.deckCard ] ]
    [ case card of
        DCS.PendingCard →
          HH.div
            [ HP.classes [ HH.className "sd-card-pending" ] ]
            [ HH.slot' ChildSlot.cpPending unit \_ →
                { component: Pending.pendingCardComponent
                , initialState: Pending.initialState
                }
            ]
        DCS.ErrorCard message →
          HH.div
            [ HP.classes [ HH.className "sd-card-error" ] ]
            [ HH.slot' ChildSlot.cpError unit \_ →
                { component: Error.errorCardComponent
                , initialState: { message }
                }
            ]
        DCS.NextActionCard input →
          HH.div
            [ HP.classes [ HH.className "sd-card-next-action" ] ]
            [ HH.slot' ChildSlot.cpNext unit \_ →
                { component: Next.nextCardComponent
                , initialState: H.parentState (Next.initialState input)
                }
            ]
    ]

renderDef
  ∷ DeckOptions
  → (DeckOptions → DeckComponent)
  → State
  → DCS.CardDef
  → DeckHTML
renderDef opts deckComponent st { cardType, cardId } =
  HH.slot' ChildSlot.cpCard cardId \_ →
    let
      cardOpts =
        { deck: opts
        , deckComponent
        , cursor: opts.deckId : opts.cursor
        , displayCursor: opts.deckId : opts.displayCursor
        , cardId
        }
    in
      { component: Factory.cardComponent cardType cardOpts
      , initialState: H.parentState CardC.initialCardState
      }

loadingPanel ∷ DeckHTML
loadingPanel =
  HH.div
    [ HP.classes [ HH.className "sd-pending-overlay" ] ]
    [ HH.div_
        [ HH.i_ []
        , HH.span_ [ HH.text "Please wait while this card is evaluated" ]
        ]
  ]
