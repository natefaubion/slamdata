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

module SlamData.Workspace.Deck.Component
  ( component
  , module SlamData.Workspace.Deck.Component.Query
  , module DCS
  , module SlamData.Workspace.Deck.DeckPath
  ) where

import SlamData.Prelude

import Control.Monad.Aff as Aff
import Control.Monad.Aff.Bus as Bus
import Control.Monad.Eff.Exception as Exception
import Control.UI.Browser as Browser
import Data.Argonaut as J
import Data.Array as Array
import Data.Lens ((.~), _Left, _Just, is)
import Data.List ((:))
import Data.List as L
import Data.Set as Set
import Data.Time.Duration (Milliseconds(..))
import DOM.HTML.HTMLElement (getBoundingClientRect)
import Halogen as H
import Halogen.Component.Utils (sendAfter, busEventSource)
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import SlamData.ActionList.Component as ActionList
import SlamData.ActionList.Filter.Component as ActionFilter
import SlamData.Config as Config
import SlamData.FileSystem.Routing (parentURL)
import SlamData.GlobalError as GE
import SlamData.LocalStorage.Class as LS
import SlamData.LocalStorage.Keys as LSK
import SlamData.Quasar as Api
import SlamData.Wiring (DeckMessage(..))
import SlamData.Wiring as Wiring
import SlamData.Wiring.Cache as Cache
import SlamData.Workspace.AccessType as AT
import SlamData.Workspace.Action as WA
import SlamData.Workspace.Card.CardId (CardId)
import SlamData.Workspace.Card.CardType as CT
import SlamData.Workspace.Card.Component.Query as CQ
import SlamData.Workspace.Card.InsertableCardType as ICT
import SlamData.Workspace.Card.Model as Card
import SlamData.Workspace.Card.Next.Component as Next
import SlamData.Workspace.Card.Port as Port
import SlamData.Workspace.Class (navigateToDeck)
import SlamData.Workspace.Deck.BackSide as Back
import SlamData.Workspace.Deck.Common (DeckOptions, DeckHTML, DeckDSL)
import SlamData.Workspace.Deck.Common as Common
import SlamData.Workspace.Deck.Component.ChildSlot (cpCard, cpBackSide, cpNext)
import SlamData.Workspace.Deck.Component.Cycle (DeckComponent)
import SlamData.Workspace.Deck.Component.Query (Query(..), Message(..))
import SlamData.Workspace.Deck.Component.Render as DCR
import SlamData.Workspace.Deck.Component.State as DCS
import SlamData.Workspace.Deck.DeckId (DeckId)
import SlamData.Workspace.Deck.DeckPath (deckPath')
import SlamData.Workspace.Deck.Gripper.Def as GD
import SlamData.Workspace.Deck.Slider as Slider
import SlamData.Workspace.Dialog.Component as Dialog
import SlamData.Workspace.Eval.Card as EC
import SlamData.Workspace.Eval.Deck as ED
import SlamData.Workspace.Eval.Persistence as P
import SlamData.Workspace.Eval.Traverse as ET
import SlamData.Workspace.Guide as Guide
import SlamData.Workspace.Routing (mkWorkspaceURL)
import Utils as Utils
import Utils.DOM as DOM

component ∷ DeckOptions → DeckComponent
component opts =
  H.lifecycleParentComponent
    { render: render opts
    , eval: eval opts
    , initialState: const DCS.initialState
    , receiver: const Nothing
    , initializer: Just (H.action Init)
    , finalizer: Nothing
    }

render ∷ DeckOptions → DCS.State → DeckHTML
render opts st =
  case st.loadError of
    Just error →
      HH.div
        [ HP.class_ $ HH.ClassName "sd-workspace-error" ]
        [ DCR.renderError error ]
    _ → DCR.renderDeck opts component st

eval ∷ DeckOptions → Query ~> DeckDSL
eval opts = case _ of
  Init next → do
    { bus } ← Wiring.expose
    H.subscribe $ busEventSource (\msg → HandleMessage msg H.Listening) bus.decks
    H.subscribe $ busEventSource (\msg → HandleHintDismissalMessage msg H.Listening) bus.hintDismissals
    when (L.null opts.displayCursor) do
      H.subscribe $ busEventSource (\msg → HandleError msg H.Listening) bus.globalError
    H.modify
      ∘ (DCS._focusDeckHintDismissed .~ _)
      =<< (Utils.rightBool <$> LS.retrieve J.decodeJson LSK.dismissedFocusDeckHintKey)
    H.modify
      ∘ (DCS._focusDeckFrameHintDismissed .~ _)
      =<< (Utils.rightBool <$> LS.retrieve J.decodeJson LSK.dismissedFocusDeckFrameHintKey)
    updateCardSize
    loadDeck opts
    pure next
  PresentAccessNextActionCardHint next → do
    st ← H.get
    dismissedBefore ← getDismissedAccessNextActionCardHintBefore
    when
      (not dismissedBefore && isNothing st.initialSliderX && st.focused)
      (H.modify (DCS._presentAccessNextActionCardHint .~ true))
    pure next
  HideAccessNextActionCardHint next →
    dismissAccessNextActionCardHint $> next
  Publish next → do
    { path } ← Wiring.expose
    let deckPath = deckPath' path opts.deckId
    H.liftEff ∘ Browser.newTab $ mkWorkspaceURL deckPath (WA.Load AT.ReadOnly)
    pure next
  FlipDeck next → do
    displayMode ← H.gets _.displayMode
    if DCS.isFrontSide displayMode
      then switchToFlipside opts
      else switchToFrontside
    pure next
  UpdateCardSize next → do
    updateCardSize
    pure next
  ZoomIn next → do
    navigateToDeck (opts.deckId L.: opts.cursor)
    pure next
  ZoomOut next → do
    navigateToDeck opts.cursor
    pure next
  BackToFileSystem next → do
    { path } ← Wiring.expose
    void $ H.liftEff $ Browser.setHref $ parentURL $ Left path
    pure next
  StartSliding gDef mouseEvent next → do
    { cardDimensions } ← H.get
    Slider.startSliding mouseEvent gDef cardDimensions.width
    preloadCard gDef
    pure next
  StopSlidingAndSnap mouseEvent next → do
    st ← H.get
    Slider.stopSlidingAndSnap mouseEvent
    updateActiveState opts
    whenM nextActionCardIsActive dismissAccessNextActionCardHint
    pure next
  UpdateSliderPosition mouseEvent next →
    Slider.updateSliderPosition mouseEvent $> next
  StopSliderTransition next → do
    H.modify $ DCS._sliderTransition .~ false
    pure next
  Focus ev next → do
    st ← H.get
    H.liftEff $ DOM.stopPropagation ev
    when (not st.focused) do
      H.modify (DCS._focused .~ true)
      Wiring.focusDeck opts.deckId
    when
      (Common.willBePresentedWithChildFrameWhenFocused opts st)
      dismissFocusDeckHint
    nextActionCardIsActive' ← nextActionCardIsActive
    when
      (not nextActionCardIsActive' && not st.presentAccessNextActionCardHint)
      presentAccessNextActionCardHintAfterDelay
    pure next
  Defocus ev next → do
    st ← H.get
    H.modify (DCS._presentAccessNextActionCardHint .~ false)
    H.liftEff $ DOM.stopPropagation ev
    isFrame ← H.liftEff $ DOM.nodeEq (DOM.target ev) (DOM.currentTarget ev)
    when (st.focused && not (L.null opts.displayCursor)) do
      when isFrame do
        for_ (L.last opts.cursor) \rootId → Wiring.focusDeck rootId
    when
      (Common.willBePresentedWithChildFrameWhenFocused opts st)
      dismissFocusDeckFrameHint
    pure next
  DismissedCardGuide next → do
    when (L.null opts.displayCursor) $ void do
      queryNextAction (Next.PresentAddCardHint unit)
    pure next
  GetActiveCard k → do
    active ← H.gets DCS.activeCard
    pure (k (hush ∘ map _.cardId =<< active))
  HandleEval msg next →
    handleEval opts msg $> next
  HandleMessage msg next → do
    st ← H.get
    case msg of
      DeckFocused focusedDeckId → do
        when (opts.deckId ≡ focusedDeckId && not st.focused) do
          H.modify (DCS._focused .~ true)
        when (opts.deckId ≠ focusedDeckId && st.focused) $
          H.modify
            $ (DCS._focused .~ false)
            ∘ (DCS._presentAccessNextActionCardHint .~ false)
        whenM (not <$> nextActionCardIsActive) presentAccessNextActionCardHintAfterDelay
      SwitchToFront opts' → do
        when (opts'.deckId ≡ opts.deckId && opts'.cursor ≡ opts.cursor)
          switchToFrontside
      SwitchToFlip opts' → do
        when (opts'.deckId ≡ opts.deckId && opts'.cursor ≡ opts.cursor) $
          switchToFlipside opts
    pure next
  HandleHintDismissalMessage msg next → do
    case msg of
      Wiring.DeckFrameFocusHintDismissed →
        H.modify (DCS._focusDeckFrameHintDismissed .~ true)
      Wiring.DeckFocusHintDismissed →
        H.modify (DCS._focusDeckHintDismissed .~ true)
    pure next
  HandleError ge next → do
    Wiring.showDialog $ Dialog.Error opts $ GE.print ge
    pure next
  HandleNextAction msg next → handleNextAction opts msg $> next
  HandleBackFilter msg next → handleBackSideFilter msg $> next
  HandleBackAction msg next → handleBackSide opts msg $> next
  HandleGrab ev next → H.raise (GrabbedDeck ev) $> next
  DismissFocusDeckHint next → do
    dismissFocusDeckHint
    pure next
  DismissFocusDeckFrameHint next → do
    dismissFocusDeckFrameHint
    pure next

nextActionCardIsActive ∷ DeckDSL Boolean
nextActionCardIsActive = do
  st ← H.get
  pure $ DCS.activeCard st # is (_Just ∘ _Left ∘ DCS._NextActionCard)

dismissFocusDeckHint ∷ DeckDSL Unit
dismissFocusDeckHint = do
  wiring ← Wiring.expose
  H.liftAff $ Bus.write Wiring.DeckFocusHintDismissed wiring.bus.hintDismissals
  H.modify (DCS._focusDeckHintDismissed .~ true)
  LS.persist J.encodeJson LSK.dismissedFocusDeckHintKey true

dismissFocusDeckFrameHint ∷ DeckDSL Unit
dismissFocusDeckFrameHint = do
  wiring ← Wiring.expose
  H.liftAff $ Bus.write Wiring.DeckFrameFocusHintDismissed wiring.bus.hintDismissals
  H.modify (DCS._focusDeckFrameHintDismissed .~ true)
  LS.persist J.encodeJson LSK.dismissedFocusDeckFrameHintKey true

-- If an ActionList has the style display: none; then calculating its dimensions
-- will give 0, 0. (This is Mapped to Nothing.)
--
-- If we recalculate after flipping then there may be a momentary flash
-- (observed).
--
-- Sending the dimensions of the next action list to the flip side action list
-- prevents this.
--
-- If there is no next action card this can't be done so just flip and
-- recalculate.
switchToFlipside ∷ DeckOptions → DeckDSL Unit
switchToFlipside opts = do
  updateBackSide opts
  presentFlipGuideFirstTime
  _ ← queryNextActionList $ H.action $ ActionList.CalculateBoundingRect
  nextBoundingRect ← queryNextActionList $ H.request ActionList.GetBoundingRect
  case nextBoundingRect of
    Just (Just dimensions) → do
      _ ← queryBacksideActionList $ H.action $ ActionList.SetBoundingRect dimensions
      H.modify (_ { displayMode = DCS.FlipSide })
    _ → do
      H.modify (_ { displayMode = DCS.FlipSide })
      void $ queryBacksideActionList $ H.action $ ActionList.CalculateBoundingRect

switchToFrontside ∷ DeckDSL Unit
switchToFrontside = do
  _ ← queryBacksideActionList $ H.action $ ActionList.CalculateBoundingRect
  flipSideBoundingRect ← queryBacksideActionList $ H.request ActionList.GetBoundingRect
  case flipSideBoundingRect of
    Just (Just dimensions) → do
      _ ← queryNextActionList $ H.action $ ActionList.SetBoundingRect dimensions
      H.modify (_ { displayMode = DCS.FrontSide })
    _ → do
      H.modify (_ { displayMode = DCS.FrontSide })
      void $ queryNextActionList $ H.action $ ActionList.CalculateBoundingRect

handleBackSide ∷ DeckOptions → ActionList.Message Back.BackAction → DeckDSL Unit
handleBackSide opts = case _ of
  ActionList.Selected action → do
    { path } ← Wiring.expose
    st ← H.get
    case action of
      Back.Trash → do
        let
          active = DCS.activeCard st
          activeIx = DCS.activeCardIndex st
        switchToFrontside
        for_ (join $ hush <$> active) \{ cardId } → do
          when (activeIx > 0) do
            H.modify _ { activeCardIndex = Just (activeIx - 1) }
            updateActiveState opts
          H.lift $ P.removeCard opts.deckId cardId
      Back.Rename → do
        Wiring.showDialog $ Dialog.Rename opts st.name
      Back.Share → do
        getDeckTree opts.deckId >>= traverse_
          (Wiring.showDialog ∘ Dialog.Share opts ∘ ET.getSharingInput path)
      Back.Unshare → do
        getDeckTree opts.deckId >>= traverse_
          (Wiring.showDialog ∘ Dialog.Unshare opts ∘ ET.getSharingInput path)
      Back.Embed → do
        _ ← H.lift P.saveWorkspace
        getDeckTree opts.deckId >>= traverse_ \tree →
          Wiring.showDialog $ Dialog.Embed
            opts
            (ET.getSharingInput path tree)
            (ET.getVarMaps tree)
      Back.Publish → do
        _ ← H.lift P.saveWorkspace
        getDeckTree opts.deckId >>= traverse_ \tree →
          Wiring.showDialog $ Dialog.Publish
            opts
            (ET.getSharingInput path tree)
            (ET.getVarMaps tree)
      Back.DeleteDeck →
        if Array.length st.displayCards <= 1
          then H.lift $ Common.deleteDeck opts
          else Wiring.showDialog (Dialog.DeleteDeck opts)
      Back.Mirror → do
        let mirrorCard = (hush =<< DCS.activeCard st) <|> DCS.findLastRealCard st
        deck ← H.lift $ P.getDeck opts.deckId
        case deck >>= _.parent, mirrorCard <#> _.cardId of
          Just parentId, Just cardId | not (L.null opts.displayCursor) → do
            switchToFrontside
            void $ H.lift $ P.mirrorDeck parentId cardId opts.deckId
          _, Just cardId → do
            parentId ← H.lift $ P.wrapAndMirrorDeck cardId opts.deckId
            navigateToDeck (parentId L.: opts.cursor)
          _, _ → pure unit
      Back.WrapChoice CT.Draftboard → wrapDeck Card.singletonDraftboard
      Back.WrapChoice CT.Tabs → wrapDeck Card.singletonTabs
      Back.WrapChoice _ → pure unit
      Back.Wrap → pure unit
      Back.Unwrap → do
        deck ← H.lift $ P.getDeck opts.deckId
        for_ (_.parent <$> deck) case _ of
          Just cardId | not (L.null opts.displayCursor) → do
            void $ H.lift $ P.collapseDeck opts.deckId cardId
          _ → do
            childId ← H.lift $ P.unwrapDeck opts.deckId
            navigateToDeck (childId L.: opts.cursor)
  where
  wrapDeck ∷ (DeckId → Card.AnyCardModel) → DeckDSL Unit
  wrapDeck wrapper = do
    switchToFrontside
    parentId ← H.lift $ P.wrapDeck opts.deckId (wrapper opts.deckId)
    when (L.null opts.displayCursor) do
      navigateToDeck (parentId L.: opts.cursor)

handleBackSideFilter ∷ ActionFilter.Message → DeckDSL Unit
handleBackSideFilter = case _ of
  ActionFilter.FilterChanged str →
    void $ queryBacksideActionList $ H.action $ ActionList.UpdateFilter str

queryBacksideActionList ∷ ∀ a. ActionList.Query Back.BackAction a → DeckDSL (Maybe a)
queryBacksideActionList =
  H.query' cpBackSide unit

queryNextActionList ∷ ∀ a. ActionList.Query Next.NextAction a → DeckDSL (Maybe a)
queryNextActionList =
  H.query' cpNext unit ∘ Next.ToActionList

queryCardEval ∷ ∀ a. CardId → CQ.CardQuery a → DeckDSL (Maybe a)
queryCardEval cid =
  H.query' cpCard cid

queryNextAction ∷ ∀ a. Next.Query a → DeckDSL (Maybe a)
queryNextAction =
  H.query' cpNext unit

updateActiveState ∷ DeckOptions → DeckDSL Unit
updateActiveState opts = do
  st ← H.get
  { cache } ← Wiring.expose
  for_ st.activeCardIndex \cardIndex →
    H.lift $ Cache.put opts.deckId { cardIndex } cache.activeState

updateBackSide ∷ DeckOptions → DeckDSL Unit
updateBackSide { deckId, displayCursor } = do
  st ← H.get
  let
    ty = join (hush <$> DCS.activeCard st)
    tys = Array.mapMaybe hush st.displayCards
  void ∘ H.query' cpBackSide unit ∘ H.action ∘ ActionList.UpdateActions
    =<< getUpdatedBackActions { deckId, displayCursor } ty tys

getUpdatedBackActions
  ∷ Back.BackSideOptions
  → Maybe DCS.CardDef
  → Array DCS.CardDef
  → DeckDSL (Array (ActionList.Action Back.BackAction))
getUpdatedBackActions opts activeCard cards = do
  uw ← fromMaybe false <$> traverse (calculateUnwrappable opts) activeCard
  isAdvanced ← isRight <$> Api.retrieveAuthProviders
  pure $ Back.toActionListAction uw activeCard cards <$> Back.allBackActions isAdvanced

calculateUnwrappable ∷ Back.BackSideOptions → DCS.CardDef → DeckDSL Boolean
calculateUnwrappable { displayCursor, deckId } { cardId } =
  fromMaybe false <$> runMaybeT do
    deck ← MaybeT $ H.lift (P.getDeck deckId)
    card ← MaybeT $ H.lift (P.getCard cardId)
    let
      cardLen = Array.length deck.model.cards
      deckIds = Card.childDeckIds card.model
      mirrorLen = Set.size card.decks
    pure case card.model, displayCursor, deckIds of
      Card.Draftboard _, L.Nil    , _ : L.Nil → cardLen ≡ 1
      Card.Draftboard _, _ : L.Nil, _ → cardLen ≡ 1 && mirrorLen ≡ 1
      _, _, _ → false

getDismissedAccessNextActionCardHintBefore ∷ DeckDSL Boolean
getDismissedAccessNextActionCardHintBefore =
  H.lift
    $ either (const $ false) id
    <$> LS.retrieve J.decodeJson LSK.dismissedAccessNextActionCardHintKey

presentAccessNextActionCardHintAfterDelay ∷ DeckDSL Unit
presentAccessNextActionCardHintAfterDelay = void do
  H.modify (DCS._presentAccessNextActionCardHint .~ false)
  H.gets _.presentAccessNextActionCardHintCanceler >>= traverse_ cancel
  sendAfter Config.addCardGuideDelay PresentAccessNextActionCardHint
    >>= H.modify ∘ (DCS._presentAccessNextActionCardHintCanceler .~ _) ∘ Just
  where
  cancel =
    H.liftAff ∘ flip Aff.cancel (Exception.error "Cancelled")

dismissAccessNextActionCardHint ∷ DeckDSL Unit
dismissAccessNextActionCardHint = do
  H.modify (DCS._presentAccessNextActionCardHint .~ false)
  LS.persist J.encodeJson LSK.dismissedAccessNextActionCardHintKey true

handleNextAction ∷ DeckOptions → Next.Message → DeckDSL Unit
handleNextAction opts = case _ of
  Next.AddCard cardType → do
    void $ H.lift $ P.addCard opts.deckId cardType
    updateBackSide opts
    whenM (not <$> nextActionCardIsActive) presentAccessNextActionCardHintAfterDelay
  Next.PresentReason input cardType → do
    presentReason opts input cardType

presentReason ∷ DeckOptions → Port.Out → CT.CardType → DeckDSL Unit
presentReason opts input cardType =
  Wiring.showDialog dialog
  where
  insertableCardType = ICT.fromCardType cardType
  ioType = ICT.fromOut input
  reason = ICT.reason ioType cardType
  cardPaths = ICT.cardPathsBetween ioType insertableCardType
  dialog = Dialog.Reason opts cardType reason cardPaths

loadDeck ∷ DeckOptions → DeckDSL Unit
loadDeck opts = mbLoadError =<< runMaybeT do
  deck ← MaybeT $ H.lift $ P.getDeck opts.deckId
  cardDefs ← MaybeT $ getCardDefs deck.model.cards
  lift $ H.subscribe $ busEventSource (\msg → HandleEval msg H.Listening) deck.bus
  lift case deck.status of
    ED.NeedsEval cardId → do
      let displayCards = [ Left DCS.PendingCard ]
      H.modify $ DCS.fromModel { name: deck.model.name, displayCards }
      H.lift $ P.queueEval (Milliseconds 13.0) (opts.deckId L.: opts.cursor × cardId)
    ED.PendingEval cardId → do
      st ← H.get
      active ← activeCardIndex
      let displayCards = map Right cardDefs <> [ Left DCS.PendingCard ]
      H.modify
        $ (DCS._pendingCardIndex .~ DCS.cardIndexFromId cardId st)
        ∘ (DCS._activeCardIndex .~ active)
        ∘ DCS.fromModel { name: deck.model.name, displayCards }
      updateActiveState opts
    ED.Completed output → do
      active ← activeCardIndex
      H.modify
        $ (DCS.updateCompletedCards cardDefs output)
        ∘ (DCS._activeCardIndex .~ active)
        ∘ (DCS._pendingCardIndex .~ Nothing)
        ∘ DCS.fromModel { name: deck.model.name, displayCards: [] }
      updateActiveState opts
  where
  activeCardIndex = do
    { cache } ← Wiring.expose
    map _.cardIndex <$> H.lift (Cache.get opts.deckId cache.activeState)

  mbLoadError = case _ of
    Nothing → H.modify _ { loadError = Just "Unable to load deck due to an inconsistent model" }
    Just a  → pure a

handleEval ∷ DeckOptions → ED.EvalMessage → DeckDSL Unit
handleEval opts = case _ of
  ED.Pending cardId → do
    st ← H.get
    H.modify
      $ (DCS._pendingCardIndex .~ DCS.cardIndexFromId cardId st)
      ∘ (DCS.addMetaCard DCS.PendingCard)
  ED.Complete cardIds output → do
    getCardDefs cardIds >>= case _ of
      Nothing →
        H.modify _ { loadError = Just "Deck references non-existent cards" }
      Just cardDefs → do
        _ ← queryNextAction $ H.action $ Next.UpdateInput output
        H.modify (DCS.updateCompletedCards cardDefs output)
        updateActiveState opts
  ED.NameChange name → H.modify _ { name = name }
  ED.CardComplete cardId → do
    H.modify \st →
      let
        ix = DCS.cardIndexFromId cardId st
        lastIx = DCS.findLastCardIndex st
        pendingCardIndex = case ix, lastIx of
          Nothing, _ → st.pendingCardIndex
          _, Nothing → st.pendingCardIndex
          Just ix', Just lastIx'
            | lastIx' > ix' → Just $ ix' + one
            | otherwise → Nothing
      in st # DCS._pendingCardIndex .~ pendingCardIndex

  _ → pure unit

getCardDefs ∷ Array CardId → DeckDSL (Maybe (Array DCS.CardDef))
getCardDefs cardIds =
  sequence <$> for cardIds \cardId →
    map (mkCardDef cardId) <$> H.lift (P.getCard cardId)

mkCardDef ∷ CardId → EC.Cell → DCS.CardDef
mkCardDef cardId cell = { cardId, cardType: Card.modelCardType cell.model }

getDeckTree ∷ DeckId → DeckDSL (Maybe (ET.TraverseDeck ED.Cell EC.Cell))
getDeckTree deckId = do
  wiring ← Wiring.expose
  decks ← H.lift $ Cache.snapshot wiring.eval.decks
  cards ← H.lift $ Cache.snapshot wiring.eval.cards
  pure (ET.unfoldEvalTree decks cards deckId)

preloadCard ∷ GD.GripperDef → DeckDSL Unit
preloadCard gDef = do
  st ← H.get
  let
    ix = case gDef of
      GD.Previous true → (_ - 1) <$> st.activeCardIndex
      GD.Next true → (_ + 1) <$> st.activeCardIndex
      _ → Nothing
  for_ (ix >>= flip DCS.cardIdFromIndex st) \cardId → do
    void $ queryCardEval cardId $ H.action CQ.PreloadCard

updateCardSize ∷ DeckDSL Unit
updateCardSize =
  H.getHTMLElementRef Common.sizerRef >>= traverse_ \el → void do
    { width, height } ← H.liftEff $ getBoundingClientRect el
    H.modify _
      { cardDimensions = { width, height }
      , responsiveSize = breakpoint width
      }
    _ ← queryNextActionList $ H.action ActionList.CalculateBoundingRect
    _ ← queryBacksideActionList $ H.action ActionList.CalculateBoundingRect
    pure unit
  where
  breakpoint w
    | w < 240.0 = DCS.XSmall
    | w < 320.0 = DCS.Small
    | w < 420.0 = DCS.Medium
    | w < 540.0 = DCS.Large
    | w < 720.0 = DCS.XLarge
    | otherwise = DCS.XXLarge

presentFlipGuideFirstTime ∷ DeckDSL Unit
presentFlipGuideFirstTime = do
  whenM shouldPresentFlipGuide do
    { bus } ← Wiring.expose
    H.liftAff $ Bus.write Guide.FlipGuide bus.stepByStep

shouldPresentFlipGuide ∷ DeckDSL Boolean
shouldPresentFlipGuide =
  H.lift
    $ either (const true) not
    <$> LS.retrieve J.decodeJson LSK.dismissedFlipGuideKey
