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
  ( initialState
  , render
  , eval
  , peek
  , module SlamData.Workspace.Deck.Component.Query
  , module DCS
  , module SlamData.Workspace.Deck.DeckPath
  ) where

import SlamData.Prelude

import Control.Monad.Aff as Aff
import Control.Monad.Eff.Exception as Exception
import Control.Monad.Eff.Ref (readRef)
import Control.Monad.Aff.Bus as Bus
import Control.Monad.Aff.EventLoop as EventLoop
import Control.UI.Browser as Browser

import Data.Array as Array
import Data.Lens ((.~), (%~), (^?), (?~), _Left, _Just, is)
import Data.Lens.Prism.Coproduct as CoP
import Data.List as L

import DOM.HTML.HTMLElement (getBoundingClientRect)

import Halogen as H
import Halogen.Component.Opaque.Unsafe (opaqueState)
import Halogen.Component.Utils (liftH', raise', sendAfter', subscribeToBus')
import Halogen.HTML.Indexed as HH
import Halogen.HTML.Properties.Indexed as HP

import SlamData.Config as Config
import SlamData.FileSystem.Routing (parentURL)
import SlamData.GlobalError as GE
import SlamData.Guide as Guide
import SlamData.Wiring (DeckMessage(..))
import SlamData.Wiring as Wiring
import SlamData.Wiring.Cache as Cache
import SlamData.Workspace.AccessType as AT
import SlamData.Workspace.Action as WA
import SlamData.Workspace.Card.CardId (CardId)
import SlamData.Workspace.Card.CardType as CT
import SlamData.Workspace.Card.Common.EvalQuery as CEQ
import SlamData.Workspace.Card.Component (CardQueryP, CardQuery(..), InnerCardQuery, AnyCardQuery)
import SlamData.Workspace.Card.Component.Query as CQ
import SlamData.Workspace.Card.InsertableCardType as ICT
import SlamData.Workspace.Card.Model as Card
import SlamData.Workspace.Card.Port as Port
import SlamData.Workspace.Card.Next.Component.Query as Next
import SlamData.Workspace.Class (navigate, Routes(..))
import SlamData.Workspace.Deck.BackSide.Component as Back
import SlamData.Workspace.Deck.Common (DeckOptions, DeckHTML, DeckDSL)
import SlamData.Workspace.Deck.Component.ChildSlot (cpCard, ChildQuery, ChildSlot, cpDialog, cpBackSide, cpNext)
import SlamData.Workspace.Deck.Component.Cycle (DeckComponent)
import SlamData.Workspace.Deck.Component.Query (QueryP, Query(..))
import SlamData.Workspace.Deck.Component.Render as DCR
import SlamData.Workspace.Deck.Component.State as DCS
import SlamData.Workspace.Deck.DeckPath (deckPath, deckPath')
import SlamData.Workspace.Deck.DeckId (DeckId)
import SlamData.Workspace.Deck.Dialog.Component as Dialog
import SlamData.Workspace.Deck.Slider as Slider
import SlamData.Workspace.Eval.Card as EC
import SlamData.Workspace.Eval.Deck as ED
import SlamData.Workspace.Eval.Persistence as P
import SlamData.Workspace.Eval.Traverse as ET
import SlamData.Workspace.Routing (mkWorkspaceURL)

import Utils (hush)
import Utils.DOM (elementEq)
import Utils.LocalStorage as LocalStorage

initialState ∷ DCS.StateP
initialState = opaqueState DCS.initialDeck

render ∷ DeckOptions → (DeckOptions → DeckComponent) → DCS.State → DeckHTML
render opts deckComponent st =
  -- HACK: required so that nested finalizers get run. Since this is run inside
  -- of a separate runUI instance with Deck.Component.Nested, they will not
  -- get invoked by normal machinery. -nf
  if st.finalized
  then HH.div_ []
  else case st.loadError of
    Just error →
      HH.div
        [ HP.class_ $ HH.className "sd-workspace-error" ]
        [ DCR.renderError error ]
    _ → DCR.renderDeck opts deckComponent st

eval ∷ DeckOptions → Query ~> DeckDSL
eval opts = case _ of
  Init next → do
    { bus } ← liftH' Wiring.expose
    mb ← subscribeToBus' (H.action ∘ HandleMessage) bus.decks
    H.modify $ DCS._breakers .~ [mb]
    when (L.null opts.displayCursor) do
      eb ← subscribeToBus' (H.action ∘ HandleError) bus.globalError
      H.modify $ DCS._breakers %~ (Array.cons eb)
    updateCardSize
    loadDeck opts
    pure next
  PresentAccessNextActionCardGuide next → do
    H.modify (DCS._presentAccessNextActionCardGuide .~ true) $> next
  HideAccessNextActionCardGuide next →
    dismissAccessNextActionCardGuide $> next
  Finish next → do
    H.modify _ { finalized = true }
    H.gets _.breakers >>= traverse_ (H.fromAff ∘ EventLoop.break')
    pure next
  Publish next → do
    { path } ← liftH' Wiring.expose
    let deckPath = deckPath' path opts.deckId
    H.fromEff ∘ Browser.newTab $ mkWorkspaceURL deckPath (WA.Load AT.ReadOnly)
    pure next
  FlipDeck next → do
    updateBackSide opts
    displayMode ← H.gets _.displayMode
    H.modify
      $ DCS.changeDisplayMode
      $ case displayMode of
          DCS.Normal → DCS.Backside
          _ → DCS.Normal
    presentFlipGuideFirstTime
    pure next
  GrabDeck _ next →
    pure next
  UpdateCardSize next → do
    updateCardSize
    pure next
  ZoomIn next → do
    navigateToDeck (opts.deckId L.: opts.cursor)
    pure next
  ZoomOut next → do
    { path } ← liftH' Wiring.expose
    if L.null opts.cursor
      then void $ H.fromEff $ Browser.setHref $ parentURL $ Left path
      else navigateToDeck opts.cursor
    pure next
  StartSliding mouseEvent gDef next → do
    H.gets _.deckElement >>= traverse_ \el → do
      width ← getBoundingClientWidth el
      H.modify (DCS._cardElementWidth ?~ width)
      Slider.startSliding mouseEvent gDef
    pure next
  StopSlidingAndSnap mouseEvent next → do
    st ← H.get
    for_ st.activeCardIndex \oldIndex →
      for_ (DCS.cardIdFromIndex oldIndex st) \cardId →
        void $ queryCardEval cardId $ H.action CQ.DeactivateCard
    Slider.stopSlidingAndSnap mouseEvent
    updateActiveState opts
    when (DCS.activeCard st # is (_Just ∘ _Left ∘ DCS._NextActionCard)) do
      dismissAccessNextActionCardGuide
    pure next
  UpdateSliderPosition mouseEvent next →
    Slider.updateSliderPosition mouseEvent $> next
  SetCardElement element next → do
    H.modify _ { deckElement = element }
    pure next
  StopSliderTransition next → do
    sliderTransition ← H.gets _.sliderTransition
    when sliderTransition $
      H.modify $ DCS._sliderTransition .~ false
    pure next
  Focus next → do
    st ← H.get
    when (not st.focused) do
      H.modify (DCS._focused .~ true)
      { bus } ← liftH' Wiring.expose
      H.fromAff $ Bus.write (DeckFocused opts.deckId) bus.decks
      presentAccessNextActionCardGuideAfterDelay
    pure next
  Defocus ev next → do
    st ← H.get
    isFrame ← H.fromEff $ elementEq ev.target ev.currentTarget
    when (st.focused && isFrame) $
      for_ (L.last opts.cursor) \rootId → do
        { bus } ← liftH' Wiring.expose
        H.fromAff $ Bus.write (DeckFocused rootId) bus.decks
    H.modify (DCS._presentAccessNextActionCardGuide .~ false)
    pure next
  HandleEval msg next →
    handleEval opts msg $> next
  HandleMessage msg next → do
    st ← H.get
    case msg of
      DeckFocused focusedDeckId → do
        when (opts.deckId ≡ focusedDeckId && not st.focused) $
          H.modify (DCS._focused .~ true)
        when (opts.deckId ≠ focusedDeckId && st.focused) $
          H.modify (DCS._focused .~ false)
    pure next
  HandleError ge next → do
    showDialog $ Dialog.Error $ GE.print ge
    pure next
  DismissedCardGuide next → do
    when (L.null opts.displayCursor) $ void do
      queryNextAction (Next.PresentAddCardGuide unit)
    pure next
  GetActiveCard k → do
    active ← H.gets DCS.activeCard
    pure (k (hush ∘ map _.cardId =<< active))
  DismissDialog next →
    queryDialog (H.action Dialog.Dismiss)
      *> H.modify DCS.undoLastChangeDisplayMode
      $> next
  where
  getBoundingClientWidth =
    H.fromEff ∘ map _.width ∘ getBoundingClientRect

peek ∷ ∀ a. DeckOptions → H.ChildF ChildSlot ChildQuery a → DeckDSL Unit
peek opts (H.ChildF s q) =
  (peekCards ⊹ (\_ _ → pure unit) $ s)
   ⨁ peekBackSide opts
   ⨁ (peekDialog opts ⨁ (const $ pure unit))
   ⨁ peekNextAction opts
   ⨁ (const $ pure unit)
   ⨁ (const $ pure unit)
   $ q

peekDialog ∷ ∀ a. DeckOptions → Dialog.Query a → DeckDSL Unit
peekDialog opts = case _ of
  Dialog.Show _ _ → do
    H.modify (DCS.changeDisplayMode DCS.Dialog)
  Dialog.Dismiss _ → do
    H.modify DCS.undoLastChangeDisplayMode
  Dialog.FlipToFront _ →
    H.modify (DCS.changeDisplayMode DCS.Normal)
  Dialog.SetDeckName name _ → do
    H.modify (DCS.changeDisplayMode DCS.Normal)
    void $ liftH' $ P.renameDeck opts.deckId name
  Dialog.Confirm d b _ → do
    H.modify (DCS.changeDisplayMode DCS.Backside)
    case d of
      Dialog.DeleteDeck | b → deleteDeck opts
      _ → pure unit

peekBackSide ∷ ∀ a. DeckOptions → Back.Query a → DeckDSL Unit
peekBackSide opts (Back.DoAction action _) = do
  { path } ← liftH' Wiring.expose
  st ← H.get
  case action of
    Back.Trash → do
      let
        active = DCS.activeCard st
      for_ (join $ hush <$> active) \{ cardId } → do
        liftH' $ P.removeCard opts.deckId cardId
        H.modify
          $ (DCS.changeDisplayMode DCS.Normal)
          ∘ (DCS._presentAccessNextActionCardGuide .~ false)
      void $ H.queryAll' cpCard $ left $ H.action UpdateDimensions
    Back.Rename → do
      showDialog $ Dialog.Rename st.name
    Back.Share → do
      getDeckTree opts.deckId >>= traverse_
        (showDialog ∘ Dialog.Share ∘ ET.getSharingInput path)
    Back.Unshare → do
      getDeckTree opts.deckId >>= traverse_
        (showDialog ∘ Dialog.Unshare ∘ ET.getSharingInput path)
    Back.Embed → do
      getDeckTree opts.deckId >>= traverse_ \tree →
        showDialog $ Dialog.Embed
          (ET.getSharingInput path tree)
          (ET.getVarMaps tree)
    Back.Publish → do
      getDeckTree opts.deckId >>= traverse_ \tree →
        showDialog $ Dialog.Publish
          (ET.getSharingInput path tree)
          (ET.getVarMaps tree)
    Back.DeleteDeck →
      if Array.length st.displayCards <= 1
        then deleteDeck opts
        else showDialog Dialog.DeleteDeck
    Back.Mirror → do
      let mirrorCard = (hush =<< DCS.activeCard st) <|> DCS.findLastRealCard st
      deck ← liftH' $ P.getDeck opts.deckId
      case deck >>= _.parent, mirrorCard <#> _.cardId of
        Just parentId, Just cardId | not (L.null opts.displayCursor) → do
          liftH' $ P.mirrorDeck parentId cardId opts.deckId
          H.modify (DCS.changeDisplayMode DCS.Normal)
        _, Just cardId → do
          parentId ← liftH' $ P.wrapAndMirrorDeck cardId opts.deckId
          navigateToDeck (parentId L.: opts.cursor)
        _, _ → pure unit
    Back.Wrap → do
      parentId ← liftH' $ P.wrapDeck opts.deckId
      if L.null opts.displayCursor
        then navigateToDeck (parentId L.: opts.cursor)
        else H.modify $ DCS.changeDisplayMode DCS.Normal
    Back.Unwrap → do
      deck ← liftH' $ P.getDeck opts.deckId
      for_ (_.parent <$> deck) case _ of
        Just cardId | not (L.null opts.displayCursor) → do
          void $ liftH' $ P.collapseDeck opts.deckId cardId
        _ → do
          childId ← liftH' $ P.unwrapDeck opts.deckId
          navigateToDeck (childId L.: opts.cursor)
peekBackSide _ _ = pure unit

peekCards ∷ ∀ a. CardId → CardQueryP a → DeckDSL Unit
peekCards cardId = const (pure unit) ⨁ peekCardInner cardId

showDialog ∷ Dialog.Dialog → DeckDSL Unit
showDialog dlg = do
  queryDialog $ H.action $ Dialog.Show dlg
  H.modify (DCS.changeDisplayMode DCS.Dialog)

queryDialog ∷ Dialog.Query Unit → DeckDSL Unit
queryDialog = void ∘ H.query' cpDialog unit ∘ left

queryCard ∷ ∀ a. CardId → CQ.AnyCardQuery a → DeckDSL (Maybe a)
queryCard cid =
  H.query' cpCard cid
    ∘ right
    ∘ H.ChildF unit
    ∘ right

queryCardEval ∷ ∀ a. CardId → CQ.CardQuery a → DeckDSL (Maybe a)
queryCardEval cid =
  H.query' cpCard cid ∘ left

queryNextAction ∷ ∀ a. Next.Query a → DeckDSL (Maybe a)
queryNextAction =
  H.query' cpNext unit ∘ left

updateActiveState ∷ DeckOptions → DeckDSL Unit
updateActiveState opts = do
  st ← H.get
  { cache } ← liftH' Wiring.expose
  for_ st.activeCardIndex \cardIndex →
    liftH' $ Cache.put opts.deckId { cardIndex } cache.activeState
  case DCS.activeCard st of
    Just (Right { cardId }) → void $ queryCardEval cardId $ H.action CQ.ActivateCard
    _ → pure unit

updateBackSide ∷ DeckOptions → DeckDSL Unit
updateBackSide { cursor } = do
  st ← H.get
  let
    ty = join (hush <$> DCS.activeCard st)
    tys = Array.mapMaybe hush st.displayCards
  void $ H.query' cpBackSide unit $ H.action $ Back.UpdateCard ty tys

dismissedAccessNextActionCardGuideKey ∷ String
dismissedAccessNextActionCardGuideKey = "dismissedAccessNextActionCardGuide"

getDismissedAccessNextActionCardGuideBefore ∷ DeckDSL Boolean
getDismissedAccessNextActionCardGuideBefore =
  liftH'
    $ either (const $ false) id
    <$> LocalStorage.getLocalStorage dismissedAccessNextActionCardGuideKey

storeDismissedAccessNextActionCardGuide ∷ DeckDSL Unit
storeDismissedAccessNextActionCardGuide =
  liftH' $ LocalStorage.setLocalStorage dismissedAccessNextActionCardGuideKey true

presentAccessNextActionCardGuideAfterDelay ∷ DeckDSL Unit
presentAccessNextActionCardGuideAfterDelay = do
  dismissedBefore ← getDismissedAccessNextActionCardGuideBefore
  focused ← H.gets _.focused
  when (not dismissedBefore && focused) do
    cancelPresentAccessNextActionCardGuide
    canceler ← sendAfter' Config.addCardGuideDelay $ PresentAccessNextActionCardGuide unit
    H.modify $ DCS._presentAccessNextActionCardGuideCanceler .~ Just canceler

cancelPresentAccessNextActionCardGuide ∷ DeckDSL Boolean
cancelPresentAccessNextActionCardGuide =
  H.fromAff ∘ maybe (pure false) (flip Aff.cancel $ Exception.error "Cancelled")
    =<< H.gets _.presentAccessNextActionCardGuideCanceler

dismissAccessNextActionCardGuide ∷ DeckDSL Unit
dismissAccessNextActionCardGuide =
  H.gets _.presentAccessNextActionCardGuide >>=
    flip when do
      H.modify (DCS._presentAccessNextActionCardGuide .~ false)
      storeDismissedAccessNextActionCardGuide

resetAccessNextActionCardGuideDelay ∷ DeckDSL Unit
resetAccessNextActionCardGuideDelay =
  whenM cancelPresentAccessNextActionCardGuide
    presentAccessNextActionCardGuideAfterDelay

peekCardInner
  ∷ ∀ a
  . CardId
  → H.ChildF Unit InnerCardQuery a
  → DeckDSL Unit
peekCardInner cardId = H.runChildF ⋙
  (peekCardEvalQuery cardId ⨁ peekAnyCard cardId)

peekCardEvalQuery ∷ ∀ a. CardId → CEQ.CardEvalQuery a → DeckDSL Unit
peekCardEvalQuery cardId = case _ of
  CEQ.ZoomIn _ → raise' $ H.action ZoomIn
  _ → pure unit

peekAnyCard ∷ ∀ a. CardId → AnyCardQuery a → DeckDSL Unit
peekAnyCard cardId _ =
  resetAccessNextActionCardGuideDelay

peekNextAction ∷ ∀ a. DeckOptions → Next.QueryP a → DeckDSL Unit
peekNextAction opts q = do
  for_ (q ^? CoP._Left ∘ Next._AddCardType) $ void ∘ liftH' ∘ P.addCard opts.deckId
  for_ (q ^? CoP._Left ∘ Next._PresentReason) $ uncurry presentReason

presentReason ∷ Port.Port → CT.CardType → DeckDSL Unit
presentReason input cardType =
  showDialog dialog
  where
  insertableCardType = ICT.fromCardType cardType
  ioType = ICT.fromPort input
  reason = ICT.reason ioType cardType
  cardPaths = ICT.cardPathsBetween ioType insertableCardType
  dialog = Dialog.Reason cardType reason cardPaths

deleteDeck ∷ DeckOptions → DeckDSL Unit
deleteDeck opts = do
  st ← H.get
  liftH' $ P.deleteDeck opts.deckId
  navigateToDeck opts.cursor
  pure unit

loadDeck ∷ DeckOptions → DeckDSL Unit
loadDeck opts = mbLoadError =<< runMaybeT do
  deck ← MaybeT $ liftH' $ P.getDeck opts.deckId
  cardDefs ← MaybeT $ getCardDefs deck.model.cards
  breaker ← lift $ subscribeToBus' (H.action ∘ HandleEval) deck.bus
  case deck.status of
    ED.NeedsEval cardId → lift do
      let displayCards = [ Left DCS.PendingCard ]
      H.modify
        $ DCS.fromModel { name: deck.model.name, displayCards }
        ∘ DCS.addBreaker breaker
      liftH' $ P.queueEval 13 (opts.deckId L.: opts.cursor × cardId)
    ED.PendingEval cardId → lift do
      st ← H.get
      active ← activeCardIndex
      let displayCards = map Right cardDefs <> [ Left DCS.PendingCard ]
      H.modify
        $ (DCS._pendingCardIndex .~ DCS.cardIndexFromId cardId st)
        ∘ (DCS._activeCardIndex .~ active)
        ∘ DCS.fromModel { name: deck.model.name, displayCards }
        ∘ DCS.addBreaker breaker
      updateActiveState opts
    ED.Completed port → lift do
      active ← activeCardIndex
      H.modify
        $ (DCS.updateCompletedCards cardDefs port)
        ∘ (DCS._activeCardIndex .~ active)
        ∘ DCS.fromModel { name: deck.model.name, displayCards: [] }
        ∘ DCS.addBreaker breaker
      updateActiveState opts
  where
  activeCardIndex = do
    { cache } ← liftH' Wiring.expose
    map _.cardIndex <$> liftH' (Cache.get opts.deckId cache.activeState)

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
  ED.Complete cardIds port → do
    getCardDefs cardIds >>= case _ of
      Nothing →
        H.modify _ { loadError = Just "Deck references non-existent cards" }
      Just cardDefs → do
        queryNextAction $ H.action $ Next.UpdateInput port
        H.modify (DCS.updateCompletedCards cardDefs port)
        updateActiveState opts
  ED.NameChange name → H.modify _ { name = name }
  _ → pure unit

getCardDefs ∷ Array CardId → DeckDSL (Maybe (Array DCS.CardDef))
getCardDefs cardIds =
  sequence <$> for cardIds \cardId →
    map (mkCardDef cardId) <$> liftH' (P.getCard cardId)

mkCardDef ∷ CardId → EC.Cell → DCS.CardDef
mkCardDef cardId cell = { cardId, cardType: Card.modelCardType cell.model }

getDeckTree ∷ DeckId → DeckDSL (Maybe (ET.TraverseDeck ED.Cell EC.Cell))
getDeckTree deckId = do
  wiring ← liftH' Wiring.expose
  decks ← liftH' $ Cache.snapshot wiring.eval.decks
  cards ← liftH' $ Cache.snapshot wiring.eval.cards
  pure (ET.unfoldEvalTree decks cards deckId)

updateCardSize ∷ DeckDSL Unit
updateCardSize = do
  H.queryAll' cpCard $ left $ H.action UpdateDimensions
  H.gets _.deckElement >>= traverse_ \el → do
    { width } ← H.fromEff $ getBoundingClientRect el
    H.modify $ DCS._responsiveSize .~ breakpoint width
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
  H.gets _.displayMode >>=
    case _ of
      DCS.Backside → do
        { bus } ← liftH' Wiring.expose
        shouldPresentFlipGuide >>=
          if _
          then H.fromAff $ Bus.write Wiring.FlipGuide bus.stepByStep
          else pure unit
      _ → pure unit

shouldPresentFlipGuide ∷ DeckDSL Boolean
shouldPresentFlipGuide =
  liftH' $
    either (const true) not <$> LocalStorage.getLocalStorage Guide.dismissedFlipGuideKey

navigateToDeck ∷ L.List DeckId → DeckDSL Unit
navigateToDeck = case _ of
  L.Nil → navigateToIndex
  cursor → do
    { path, accessType, varMaps } ← liftH' Wiring.expose
    urlVarMaps ← H.fromEff $ readRef varMaps
    navigate $ WorkspaceRoute path cursor (WA.Load accessType) urlVarMaps

navigateToIndex ∷ DeckDSL Unit
navigateToIndex = do
  { path } ← liftH' Wiring.expose
  void $ H.fromEff $ Browser.setHref $ parentURL $ Left path
