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
  ) where

import SlamData.Prelude

import Control.Monad.Aff.Par (Par(..), runPar)
import Control.Monad.Aff.Bus as Bus
import Control.Monad.Aff.EventLoop as EventLoop
import Control.Monad.Aff.Promise as Pr
import Control.Monad.Except.Trans (ExceptT(..), runExceptT)
import Control.UI.Browser (newTab, locationObject, locationString, setHref)

import Data.Array as Array
import Data.Foldable (find, any)
import Data.Lens as Lens
import Data.Lens ((.~), (%~), (^?), (?~))
import Data.Lens.Prism.Coproduct (_Right)
import Data.List as L
import Data.Ord (max)
import Data.Path.Pathy ((</>))
import Data.Path.Pathy as Pathy
import Data.Set as Set
import Data.Time (Milliseconds(..))

import DOM.HTML.Location as Location

import Halogen as H
import Halogen.Component.Opaque.Unsafe (opaqueState)
import Halogen.Component.Utils (raise', subscribeToBus')
import Halogen.Component.Utils.Debounced (fireDebouncedQuery')
import Halogen.HTML.Events.Indexed as HE
import Halogen.HTML.Events.Handler as HEH
import Halogen.HTML.Indexed as HH
import Halogen.HTML.Properties.Indexed as HP
import Halogen.HTML.Properties.Indexed.ARIA as ARIA
import Halogen.Themes.Bootstrap3 as B

import SlamData.Config (workspaceUrl)
import SlamData.FileSystem.Resource as R
import SlamData.FileSystem.Routing (parentURL)
import SlamData.Quasar.Data (save) as Quasar
import SlamData.Render.Common (glyph)
import SlamData.Workspace.AccessType as AT
import SlamData.Workspace.Action as WA
import SlamData.Workspace.Card.CardId (CardId(..), _CardId)
import SlamData.Workspace.Card.CardType as CT
import SlamData.Workspace.Card.Common.EvalQuery as CEQ
import SlamData.Workspace.Card.Component (CardQueryP, CardQuery(..), InnerCardQuery, AnyCardQuery, _NextQuery)
import SlamData.Workspace.Card.Component.Query as CQ
import SlamData.Workspace.Card.Draftboard.Common as DBC
import SlamData.Workspace.Card.Eval as Eval
import SlamData.Workspace.Card.Model as Card
import SlamData.Workspace.Card.Next.Component as Next
import SlamData.Workspace.Card.Port (Port)
import SlamData.Workspace.Card.Port as Port
import SlamData.Workspace.Deck.BackSide.Component as Back
import SlamData.Workspace.Deck.Common (DeckHTML, DeckDSL)
import SlamData.Workspace.Deck.Component.ChildSlot (cpBackSide, cpCard, cpIndicator, ChildQuery, ChildSlot, CardSlot(..), cpDialog)
import SlamData.Workspace.Deck.Component.CSS as CSS
import SlamData.Workspace.Deck.Component.Query (QueryP, Query(..), DeckAction(..))
import SlamData.Workspace.Deck.Component.State as DCS
import SlamData.Workspace.Deck.Component.Cycle (DeckComponent)
import SlamData.Workspace.Deck.DeckId (DeckId)
import SlamData.Workspace.Deck.DeckLevel as DL
import SlamData.Workspace.Deck.Dialog.Component as Dialog
import SlamData.Workspace.Deck.Gripper as Gripper
import SlamData.Workspace.Deck.Indicator.Component as Indicator
import SlamData.Workspace.Deck.Model (deckIndex)
import SlamData.Workspace.Deck.Model as Model
import SlamData.Workspace.Deck.Slider as Slider
import SlamData.Workspace.Model as WM
import SlamData.Workspace.Routing (mkWorkspaceHash, mkWorkspaceURL)
import SlamData.Workspace.StateMode (StateMode(..))
import SlamData.Workspace.Wiring (Wiring, CardEval, Cache, DeckMessage(..), getDeck, putDeck, putCardEval, getCache, makeCache)

import Utils.DOM (getBoundingClientRect)
import Utils.Path (DirPath)

initialState ∷ DirPath → DeckId → DCS.StateP
initialState path = opaqueState ∘ DCS.initialDeck path

render ∷ Wiring → DeckComponent → DCS.State → DeckHTML
render wiring deckComponent st =
  -- HACK: required so that nested finalizers get run. Since this is run inside
  -- of a separate runUI instance with Deck.Component.Nested, they will not
  -- get invoked by normal machinery.
  if st.finalized
  then HH.div_ []
  else case st.stateMode of
    Error err → renderError err
    _ →
      -- WARNING: Very strange things happen when this is not in a div; see SD-1326.
      HH.div
        ([ HP.classes $ [ CSS.deckContainer ] ++ (if st.focused then [ HH.className "focused" ] else [])
         , HP.key "deck-container"
         , HE.onMouseUp (HE.input_ UpdateCardSize)
         , HE.onMouseDown \_ → HEH.stopPropagation $> Just (H.action Focus)
         ] ⊕ Slider.containerProperties st)
        [ HH.div
            [ HP.class_ CSS.deckFrame ]
            [ HH.button
                [ HP.classes [ CSS.flipDeck ]
                , HE.onClick (HE.input_ FlipDeck)
                , ARIA.label "Flip deck"
                , HP.title "Flip deck"
                ]
                [ HH.text "" ]
            , zoomButton
            , if st.level ≡ DL.root
                then HH.text ""
                else
                  HH.button
                    [ HP.classes [ CSS.grabDeck ]
                    , HE.onMouseDown (HE.input GrabDeck)
                    , ARIA.label "Grab deck"
                    , HP.title "Grab deck"
                    ]
                    [ HH.text "" ]
            , renderName
            , HH.button
                [ HP.classes [ CSS.grabDeck ]
                , HE.onMouseDown (HE.input GrabDeck)
                , ARIA.label "Grab deck"
                , HP.title "Grab deck"
                ]
                [ HH.text "" ]
            , HH.slot' cpIndicator unit \_ →
                { component: Indicator.comp
                , initialState: Indicator.initialState
                }
            , if st.level ≡ DL.root
                then HH.text ""
                else
                  HH.button
                    [ HP.classes [ CSS.resizeDeck ]
                    , HE.onMouseDown (HE.input ResizeDeck)
                    , ARIA.label "Resize deck"
                    , HP.title "Resize deck"
                    ]
                    [ HH.text "" ]
            ]
        , HH.div
            [ HP.class_ CSS.deck
            , HP.key "deck"
            ]
            [ Slider.render wiring deckComponent st $ st.displayMode ≡ DCS.Normal
            , renderBackside $ st.displayMode ≡ DCS.Backside
            , renderDialog $ st.displayMode ≡ DCS.Dialog
            ]
        ]

  where

  zoomButton
    | st.level ≡ DL.root =
        HH.button
         [ HP.classes [ CSS.zoomDeck ]
         , ARIA.label "Zoom out"
         , HP.title "Zoom out"
         , HE.onClick (HE.input_ ZoomOut)
         ]
         [ glyph B.glyphiconZoomOut ]
    | otherwise =
        HH.button
           [ HP.classes [ CSS.zoomDeck ]
           , ARIA.label "Zoom in"
           , HP.title "Zoom in"
           , HE.onClick (HE.input_ ZoomIn)
           ]
           [ glyph B.glyphiconZoomIn ]

  renderError err =
    HH.div
      [ HP.classes [ B.alert, B.alertDanger ] ]
      [ HH.h1
          [ HP.class_ B.textCenter ]
          [ HH.text err ]
      ]

  renderDialog visible =
    HH.div
      ([ HP.classes [ HH.className "deck-dialog-wrapper" ]
       , ARIA.hidden $ show $ not visible
       ] ⊕ (guard (not visible) $> HP.class_ CSS.invisible))
      [ HH.slot' cpDialog unit \_ →
         { component: Dialog.comp
         , initialState: H.parentState Dialog.initialState
         }
      ]

  renderBackside visible =
    HH.div
      ([ HP.classes [ CSS.cardSlider ]
       , ARIA.hidden $ show $ not visible
       ] ⊕ (guard (not visible) $> HP.class_ CSS.invisible))
      [ HH.div
          [ HP.classes [ CSS.card ] ]
          (Gripper.renderGrippers
             visible
             (isJust st.initialSliderX)
             (Gripper.gripperDefsForCard st.displayCards $ DCS.activeCardCoord st)
             ⊕ [ HH.slot' cpBackSide unit \_ →
                  { component: Back.comp
                  , initialState: Back.initialState
                  }
               ]
          )
      ]

  renderName =
    HH.div
      [ HP.classes [ CSS.deckName ] ]
      [ HH.text st.name ]

eval ∷ Wiring → Query ~> DeckDSL
eval wiring (Init next) = do
  pb ← subscribeToBus' (H.action ∘ RunPendingCards) wiring.pending
  mb ← subscribeToBus' (H.action ∘ HandleMessage) wiring.messaging
  H.modify $ DCS._breakers .~ [pb, mb]
  pure next
eval _ (Finish next) = do
  H.modify _ { finalized = true }
  H.gets _.breakers >>= traverse_ (H.fromAff ∘ EventLoop.break')
  pure next
eval wiring (Load dir deckId level next) = do
  H.modify $ DCS._level .~ level
  loadDeck wiring dir deckId
  pure next
eval wiring (SetModel deckId model level next) = do
  state ← H.get
  H.modify $ DCS._level .~ level
  setModel wiring
    { path: state.path
    , id: deckId
    , parent: model.parent
    , modelCards: Tuple deckId <$> model.cards
    , name: state.name
    }
  pure next
eval wiring (ExploreFile res next) = do
  H.modify
    $ (DCS.addCard $ Card.cardModelOfType CT.JTable)
    ∘ (DCS.addCard ∘ Card.OpenResource ∘ Just $ R.File res)
    ∘ (DCS._stateMode .~ Preparing)
  initialCard ← H.gets (map DCS.coordModelToCoord ∘ Array.head ∘ _.modelCards)
  for_ initialCard $ queuePendingCard wiring
  saveDeck wiring Nothing
  updateIndicator
  pure next
eval _ (Publish next) = do
  path ← H.gets DCS.deckPath
  H.fromEff ∘ newTab $ mkWorkspaceURL path (WA.Load AT.ReadOnly)
  pure next
-- TODO: How can we get rid of this? What is it's purpose? It smells.
eval wiring (Reset path next) = do
  st ← H.get
  setDeckState $
    (DCS.initialDeck path st.id)
      { stateMode = Ready
      , accessType = st.accessType
      , displayCards = [ st.id × nextActionCard ]
      }
  pure next
eval _ (SetParent parent next) =
  H.modify (DCS._parent .~ Just parent) $> next
eval _ (GetModelCards k) = k <$> getModelCards
eval wiring (SetModelCards modelCards next) = do
  st ← H.get
  setModel wiring
    { path: st.path
    , id: st.id
    , parent: st.parent
    , modelCards
    , name: st.name
    }
  saveDeck wiring Nothing
  pure next
eval _ (GetId k) = k <$> H.gets _.id
eval _ (GetParent k) = k <$> H.gets _.parent
eval wiring (Save coord next) = saveDeck wiring coord $> next
eval wiring (RunPendingCards { source, pendingCard, cards } next) = do
  st ← H.get
  let pendingCoord = DCS.coordModelToCoord pendingCard
  when (any (DCS.eqCoordModel pendingCoord) st.modelCards) do
    runPendingCards wiring source pendingCard cards
  pure next
eval wiring (QueuePendingCard next) = do
  H.gets _.pendingCard >>= traverse_ \pending → do
    modelCards ← getModelCards
    H.modify
      $ (DCS._modelCards .~ modelCards)
      ∘ (DCS._pendingCard .~ Nothing)
    queuePendingCard wiring pending
  pure next
eval _ (SetGlobalVarMap m next) = do
  st ← H.get
  when (m ≠ st.globalVarMap) do
    H.modify $ DCS._globalVarMap .~ m
    traverse_ runCard $ DCS.apiCards st
  pure next
eval _ (FlipDeck next) = do
  updateBackSide
  H.modify
    $ DCS._displayMode
    %~ case _ of
      DCS.Normal → DCS.Backside
      _ → DCS.Normal
  pure next
eval _ (GrabDeck _ next) = pure next
eval _ (UpdateCardSize next) = do
  H.queryAll' cpCard $ left $ H.action UpdateDimensions
  pure next
eval _ (ResizeDeck _ next) = pure next
eval _ (ZoomIn next) = do
  st ← H.get
  let deckHash = mkWorkspaceHash (DCS.deckPath st) (WA.Load st.accessType) st.globalVarMap
  H.fromEff $ locationObject >>= Location.setHash deckHash
  pure next
eval _ (ZoomOut next) = do
  st ← H.get
  case st.parent of
    Just (Tuple deckId _) → do
      let deckHash = mkWorkspaceHash (DCS.deckPath' st.path deckId) (WA.Load st.accessType) st.globalVarMap
      H.fromEff $ locationObject >>= Location.setHash deckHash
    Nothing →
      void $ H.fromEff $ setHref $ parentURL $ Left st.path
  pure next
eval _ (StartSliding mouseEvent gDef next) =
  Slider.startSliding mouseEvent gDef $> next
eval _ (StopSlidingAndSnap mouseEvent next) = do
  Slider.stopSlidingAndSnap mouseEvent
  updateIndicator
  pure next
eval _ (UpdateSliderPosition mouseEvent next) =
  Slider.updateSliderPosition mouseEvent $> next
eval _ (SetCardElement element next) =
  next <$ for_ element \el → do
    width ← getBoundingClientWidth el
    H.modify (DCS._cardElementWidth ?~ width)
  where
  getBoundingClientWidth =
    H.fromEff ∘ map _.width ∘ getBoundingClientRect
eval _ (StopSliderTransition next) =
  H.modify (DCS._sliderTransition .~ false) $> next
eval _ (DoAction _ next) = pure next
eval wiring (Focus next) = do
  st ← H.get
  when (not st.focused) do
    H.modify (DCS._focused .~ true)
    H.fromAff $ Bus.write (DeckFocused st.id) wiring.messaging
  pure next
eval _ (HandleMessage msg next) = do
  case msg of
    DeckFocused focusedDeckId -> do
      st <- H.get
      when (st.id /= focusedDeckId && st.focused) $
        H.modify (DCS._focused .~ false)
  pure next

peek ∷ ∀ a. Wiring → H.ChildF ChildSlot ChildQuery a → DeckDSL Unit
peek wiring (H.ChildF s q) =
  (peekCards wiring ⊹ (\_ _ → pure unit) $ s)
   ⨁ peekBackSide wiring
   ⨁ (const $ pure unit)
   ⨁ (peekDialog wiring ⨁ (const $ pure unit))
   $ q

peekDialog ∷ ∀ a. Wiring → Dialog.Query a → DeckDSL Unit
peekDialog _ (Dialog.Show _ _) =
  H.modify (DCS._displayMode .~ DCS.Dialog)
peekDialog _ (Dialog.Dismiss _) =
  H.modify (DCS._displayMode .~ DCS.Backside)
peekDialog wiring (Dialog.SetDeckName name _) =
  H.modify ((DCS._displayMode .~ DCS.Normal) ∘ (DCS._name .~ name))
    *> saveDeck wiring Nothing
peekDialog _ (Dialog.Confirm d b _) = do
  H.modify (DCS._displayMode .~ DCS.Backside)
  case d of
    Dialog.DeleteDeck | b → raise' $ H.action $ DoAction DeleteDeck
    _ → pure unit

peekBackSide ∷ ∀ a. Wiring → Back.Query a → DeckDSL Unit
peekBackSide _ (Back.UpdateFilter _ _) = pure unit
peekBackSide _ (Back.UpdateCardType _ _) = pure unit
peekBackSide wiring (Back.DoAction action _) =
  case action of
    Back.Trash → do
      state ← H.get
      lastId ← H.gets DCS.findLastRealCard
      for_ (DCS.activeCardCoord state <|> lastId) \trashId → do
        let rem = DCS.removeCard trashId state
        DBC.childDeckIds (snd <$> fst rem) #
          H.fromAff ∘ runPar ∘ traverse_ (Par ∘ DBC.deleteGraph state.path)
        H.set $ snd rem
        triggerSave Nothing
        updateActiveCardAndIndicator
        H.modify $ DCS._displayMode .~ DCS.Normal
        DCS.activeCardCoord (snd rem)
          # maybe (runCardUpdates state.id L.Nil) (queuePendingCard wiring)
      void $ H.queryAll' cpCard $ left $ H.action UpdateDimensions
    Back.Rename → do
      name ← H.gets _.name
      showDialog $ Dialog.Rename name
      H.modify (DCS._displayMode .~ DCS.Dialog)
    Back.Share → do
      pure unit
--      url ← mkShareURL SM.empty
--      showDialog $ Dialog.Share url
--      H.modify (DCS._displayMode .~ DCS.Dialog)
    Back.Embed → do
      varMap ← H.gets _.globalVarMap
      deckPath ← H.gets DCS.deckPath
      showDialog $ Dialog.Embed deckPath varMap
      H.modify (DCS._displayMode .~ DCS.Dialog)
    Back.Publish → do
      deckPath ← H.gets DCS.deckPath
      varMap ← H.gets _.globalVarMap
      showDialog $ Dialog.Publish deckPath varMap
      H.modify (DCS._displayMode .~ DCS.Dialog)
    Back.DeleteDeck → do
      cards ← H.gets _.modelCards
      if Array.null cards
        then raise' $ H.action $ DoAction DeleteDeck
        else do
          showDialog Dialog.DeleteDeck
          H.modify (DCS._displayMode .~ DCS.Dialog)
    Back.Mirror → do
      H.modify $ DCS._displayMode .~ DCS.Normal
      raise' $ H.action $ DoAction Mirror
    Back.Wrap → raise' $ H.action $ DoAction Wrap

mkShareURL ∷ Port.VarMap → DeckDSL String
mkShareURL varMap = do
  loc ← H.fromEff locationString
  path ← H.gets DCS.deckPath
  pure $ loc ⊕ "/" ⊕ workspaceUrl ⊕ mkWorkspaceHash path (WA.Load AT.ReadOnly) varMap

peekCards ∷ ∀ a. Wiring → CardSlot → CardQueryP a → DeckDSL Unit
peekCards wiring (CardSlot cardId) = (const $ pure unit) ⨁ peekCardInner wiring cardId

showDialog ∷ Dialog.Dialog → DeckDSL Unit
showDialog =
  queryDialog
    ∘ H.action
    ∘ Dialog.Show

queryDialog ∷ Dialog.Query Unit → DeckDSL Unit
queryDialog q = H.query' cpDialog unit (left q) *> pure unit

queryCard ∷ ∀ a. DeckId × CardId → CQ.AnyCardQuery a → DeckDSL (Maybe a)
queryCard cid =
  H.query' cpCard (CardSlot cid)
    ∘ right
    ∘ H.ChildF unit
    ∘ right

queryCardEval ∷ ∀ a. DeckId × CardId → CQ.CardQuery a → DeckDSL (Maybe a)
queryCardEval cid =
  H.query' cpCard (CardSlot cid) ∘ left

updateActiveCardAndIndicator ∷ DeckDSL Unit
updateActiveCardAndIndicator = do
  activeCardIndex ← H.gets _.activeCardIndex
  case activeCardIndex of
    Nothing → do
      H.modify $ \st →
        let
          lastCardIndex = max 0 $ Array.length st.displayCards - 1
          lastRealCardIndex = DCS.findLastRealCardIndex st
        in st { activeCardIndex = Just $ fromMaybe lastCardIndex lastRealCardIndex }
    Just _ → pure unit
  updateIndicator

updateIndicator ∷ DeckDSL Unit
updateIndicator = do
  cards ← H.gets _.displayCards
  H.query' cpIndicator unit
    $ H.action
    $ Indicator.UpdatePortList
    $ map (Card.modelCardType ∘ _.model ∘ snd) cards
  vid ← H.gets $ fromMaybe 0 ∘ _.activeCardIndex
  void $ H.query' cpIndicator unit $ H.action $ Indicator.UpdateActiveId vid

updateBackSide ∷ DeckDSL Unit
updateBackSide = do
  state ← H.get
  let ty = DCS.activeCardType state
  void
    $ H.query' cpBackSide unit
    $ H.action
    $ Back.UpdateCardType ty

createCard ∷ Wiring → CT.CardType → DeckDSL Unit
createCard wiring cardType = do
  deckId ← H.gets _.id
  (st × newCardId) ← H.gets ∘ DCS.addCard' $ Card.cardModelOfType cardType
  setDeckState st
  queuePendingCard wiring (deckId × newCardId)
  triggerSave $ Just (deckId × newCardId)

peekCardInner
  ∷ ∀ a
  . Wiring
  → DeckId × CardId
  → H.ChildF Unit InnerCardQuery a
  → DeckDSL Unit
peekCardInner wiring cardCoord = H.runChildF ⋙
  (peekCardEvalQuery cardCoord ⨁ (peekAnyCard wiring cardCoord))

peekCardEvalQuery ∷ ∀ a. DeckId × CardId → CEQ.CardEvalQuery a → DeckDSL Unit
peekCardEvalQuery cardCoord = case _ of
  CEQ.ModelUpdated CEQ.StateOnlyUpdate _ → triggerSave (Just cardCoord)
  CEQ.ModelUpdated CEQ.EvalModelUpdate _ → runCard cardCoord *> triggerSave (Just cardCoord)
  CEQ.ZoomIn _ → raise' $ H.action ZoomIn
  _ → pure unit


peekAnyCard ∷ ∀ a. Wiring → DeckId × CardId → AnyCardQuery a → DeckDSL Unit
peekAnyCard wiring cardCoord q = do
  for_ (q ^? _NextQuery ∘ _Right ∘ Next._AddCardType) $ createCard wiring

nextActionCard ∷ Card.Model
nextActionCard =
  { cardId: NextActionCardId
  , model: Card.NextAction
  }

errorCard ∷ Card.Model
errorCard =
  { cardId: ErrorCardId
  , model: Card.ErrorCard
  }

pendingEvalCard ∷ Card.Model
pendingEvalCard =
  { cardId: PendingCardId
  , model: Card.PendingCard
  }

type UpdateAccum =
  { cards ∷ L.List (DeckId × Card.Model)
  , steps ∷ L.List CardEval
  , updates ∷ L.List CardEval
  }

type UpdateResult =
  { displayCards ∷ Array (DeckId × Card.Model)
  , updates ∷ L.List CardEval
  }

queuePendingCard
  ∷ Wiring
  → DeckId × CardId
  → DeckDSL Unit
queuePendingCard wiring pendingCoord = do
  st ← H.get
  for_ (find (DCS.eqCoordModel pendingCoord) st.modelCards) \pendingCard →
    H.fromAff do
      cards ← makeCache
      Bus.write { source: st.id, pendingCard, cards } wiring.pending

runPendingCards
  ∷ Wiring
  → DeckId
  → DeckId × Card.Model
  → Cache (DeckId × CardId) CardEval
  → DeckDSL Unit
runPendingCards wiring source pendingCard pendingCards = do
  st ← H.get
  let
    pendingCoord = DCS.coordModelToCoord pendingCard
    splitCards = L.span (not ∘ DCS.eqCoordModel pendingCoord) $ L.toList st.modelCards
    prevCard = DCS.coordModelToCoord <$> L.last splitCards.init
    pendingCards = L.Cons pendingCard <$> L.tail splitCards.rest

  for_ pendingCards \cards → do
    input ← join <$> for prevCard (flip getCache wiring.cards)
    steps ← resume st (input >>= _.output) cards
    runCardUpdates source steps

  where
  resume st = go L.Nil where
    go steps input L.Nil = pure $ L.reverse steps
    go steps input (L.Cons c cs) = do
      step ←
        getCache (DCS.coordModelToCoord c) pendingCards >>= case _ of
          Just ev → pure ev
          Nothing → do
            ev ← evalCard st.path st.globalVarMap input c
            putCardEval ev pendingCards
            putCardEval ev wiring.cards
            pure ev
      go (L.Cons step steps) step.output cs

-- | When we initially eval a deck we want to use whatever is in the main cache.
-- | If we were to just queuePendingCard then everything would get freshly
-- | evaluated, which is unnecessary.
runInitialEval
  ∷ Wiring
  → DeckDSL Unit
runInitialEval wiring = do
  st ← H.get
  cards ← makeCache

  let
    cardCoords = DCS.coordModelToCoord <$> L.toList st.modelCards
    source = st.id

  for_ cardCoords \coord → do
    getCache coord wiring.cards >>= traverse_ \ev →
      putCardEval ev cards

  for_ (Array.head st.modelCards) \pendingCard →
    H.fromAff $ Bus.write { source, pendingCard, cards } wiring.pending

-- | Evaluates a card given an input and model.
evalCard
  ∷ DirPath
  → Port.VarMap
  → Maybe (Pr.Promise Port)
  → DeckId × Card.Model
  → DeckDSL CardEval
evalCard path globalVarMap input card = do
  output ← H.fromAff $ Pr.defer do
    input' ← for input Pr.wait
    case Card.modelToEval (snd card).model of
      Left err →
        pure $ Port.CardError $ "Could not evaluate card: " <> err
      Right cmd → do
        flip Eval.runEvalCard cmd
          { path
          , globalVarMap
          , cardCoord: DCS.coordModelToCoord card
          , input: input'
          }
  pure { input, card, output: Just output }

-- | Interprets a list of pending card evaluations into updates for the
-- | display cards. Takes care of the pending card, next action card and
-- | error card.
runCardUpdates ∷ DeckId → L.List CardEval → DeckDSL Unit
runCardUpdates source steps = do
  st ← H.get
  let
    realCards = Array.filter (Lens.has _CardId ∘ _.cardId ∘ snd) st.displayCards
    loadedCards = foldr (Set.insert ∘ DCS.coordModelToCoord) Set.empty realCards
    pendingCm = st.id × pendingEvalCard
    nextActionStep =
      { card: st.id × nextActionCard
      , input: _.output =<< L.last steps
      , output: Nothing
      }
    updateSteps =
      if AT.isEditable st.accessType
        then L.snoc steps nextActionStep
        else steps

  H.modify $
    DCS._displayCards .~ Array.snoc realCards pendingCm

  updateResult ←
    H.fromAff $ Pr.wait $
      updateCards st { steps: updateSteps, cards: mempty, updates: mempty }

  -- Splice in the new display cards and run their updates.
  for_ (Array.head updateResult.displayCards) \card → do
    let
      pendingId = DCS.coordModelToCoord card
      oldCards = Array.takeWhile (not ∘ DCS.eqCoordModel pendingId) realCards
      displayCards = oldCards <> updateResult.displayCards

    H.modify
      $ DCS._displayCards .~ displayCards

    for_ updateResult.updates $
      updateCard st source pendingId loadedCards

  -- Final cleanup
  when (st.stateMode == Preparing) do
    lastIndex ← H.gets DCS.findLastRealCardIndex
    H.modify
      $ (DCS._stateMode .~ Ready)
      ∘ (DCS._activeCardIndex .~ lastIndex)

  updateActiveCardAndIndicator

  where
  updateCards ∷ DCS.State → UpdateAccum → Pr.Promise UpdateResult
  updateCards st = case _ of
    { steps: L.Nil, cards, updates } →
      pure
        { displayCards: L.fromList $ L.reverse cards
        , updates: L.reverse updates
        }
    { steps: L.Cons x xs, cards, updates } → do
      output ← sequence x.output
      let cards' = L.Cons x.card cards
          updates' = L.Cons x updates
      updateCards st $ case output of
        Just (Port.CardError err) →
          let errorCard' = st.id × errorCard
              errorStep  = { input: x.output, output: Nothing, card: errorCard' }
          in
              { steps: L.Nil
              , cards: L.Cons errorCard' cards'
              , updates: L.Cons errorStep updates'
              }
        _ →
          { steps: xs
          , cards: cards'
          , updates: updates'
          }

  updateCard
    ∷ DCS.State
    → DeckId
    → DeckId × CardId
    → Set.Set (DeckId × CardId)
    → CardEval
    → DeckDSL Unit
  updateCard st source pendingId loadedCards step = void do
    input ← for step.input (H.fromAff ∘ Pr.wait)
    output ← for step.output (H.fromAff ∘ Pr.wait)

    let
      cardCoord = DCS.coordModelToCoord step.card
      evalInput = { path: st.path, globalVarMap: st.globalVarMap, input, cardCoord }

    -- We always load the card when we are updating a mirrored deck. This is
    -- so mirrors will always have the most recent model.
    when (not (Set.member cardCoord loadedCards) || st.id ≠ source) $ void do
      queryCardEval cardCoord $ H.action $ LoadCard (snd step.card)

    queryCardEval cardCoord $ H.action $ UpdateCard evalInput output

-- | Enqueues the card with the specified ID in the set of cards that are
-- | pending to run and enqueues a debounced query to trigger the cards to
-- | actually run.
runCard ∷ DeckId × CardId → DeckDSL Unit
runCard coord = do
  H.modify (DCS.addPendingCard coord)
  fireDebouncedQuery' (Milliseconds 500.0) DCS._runTrigger QueuePendingCard

-- | Triggers the H.query for autosave. This does not immediate perform the save
-- | H.action, but instead enqueues a debounced query to trigger the actual save.
triggerSave ∷ Maybe (DeckId × CardId) → DeckDSL Unit
triggerSave coord =
  fireDebouncedQuery' (Milliseconds 500.0) DCS._saveTrigger $ Save coord

-- | Saves the deck as JSON, using the current values present in the state.
saveDeck ∷ Wiring → Maybe (DeckId × CardId) → DeckDSL Unit
saveDeck wiring coord = do
  st ← H.get
  when (AT.isEditable st.accessType) do
    modelCards ← Array.span (not ∘ eq st.id ∘ fst) <$> getModelCards
    coord >>= DCS.eqCoordModel >>> flip find modelCards.init
      # maybe' (\_ → saveMainDeck st modelCards) (saveMirroredCard st)

  where
  saveMainDeck st modelCards = do
    let model =
          { parent: st.parent
          , mirror: map _.cardId <$> modelCards.init
          , cards: snd <$> modelCards.rest
          , name: st.name
          }

    when (isNothing st.parent) do
      let index = st.path </> Pathy.file "index"
      WM.getRoot index >>= case _ of
        Left _ → void $ WM.setRoot index st.id
        Right _ → pure unit

    putDeck st.id model wiring.decks
    Quasar.save (deckIndex st.path st.id) (Model.encode model) >>= case _ of
      Left err → do
        -- TODO: do something to notify the user saving failed
        pure unit
      Right _ → do
        when (st.level ≡ DL.root) $ do
          path' ← H.gets DCS.deckPath
          let deckHash = mkWorkspaceHash path' (WA.Load st.accessType) st.globalVarMap
          H.fromEff $ locationObject >>= Location.setHash deckHash

  saveMirroredCard st (deckId × card) =
    getDeck st.path deckId wiring.decks >>= case _ of
      Left err → do
        -- TODO: do something to notify the user saving failed
        pure unit
      Right deck → do
        let cards = deck.cards <#> \c → if c.cardId == card.cardId then card else c
            model = deck { cards = cards }
        putDeck deckId model wiring.decks
        void $ Quasar.save (deckIndex st.path deckId) $ Model.encode model

setDeckState ∷ DCS.State → DeckDSL Unit
setDeckState newState =
  H.modify \oldState →
    newState { cardElementWidth = oldState.cardElementWidth }

loadDeck ∷ Wiring → DirPath → DeckId → DeckDSL Unit
loadDeck wiring path deckId = do
  H.modify
    $ (DCS._stateMode .~ Loading)
    ∘ (DCS._displayCards .~ [ deckId × pendingEvalCard ])

  res ← runExceptT do
    deck ← ExceptT $ getDeck path deckId wiring.decks
    mirroredCards ← ExceptT $ H.fromAff $ loadMirroredCards deck.mirror
    pure $ deck × (mirroredCards <> (Tuple deckId <$> deck.cards))

  case res of
    Left err →
      H.modify $ DCS._stateMode .~ Error "There was a problem decoding the saved deck"
    Right (deck × modelCards) →
      setModel wiring
        { path
        , id: deckId
        , parent: deck.parent
        , modelCards
        , name: deck.name
        }
  where
  loadMirroredCards coords = do
    let deckIds = Array.nub (fst <$> coords)
    res ← sequence <$> runPar (traverse (Par ∘ flip (getDeck path) wiring.decks) deckIds)
    pure $ hydrateCards coords =<< map (Array.zip deckIds) res

  hydrateCards coords decks =
    for coords \(deckId × cardId) →
      case find (eq deckId ∘ fst) decks of
        Nothing → Left "Deck not found"
        Just (_ × deck) →
          case find (eq cardId ∘ _.cardId) deck.cards of
            Nothing → Left "Card not found"
            Just card → Right (deckId × card)

setModel
  ∷ Wiring
  → { path ∷ DirPath
    , id ∷ DeckId
    , parent ∷ Maybe (DeckId × CardId)
    , modelCards ∷ Array (DeckId × Card.Model)
    , name ∷ String
    }
  → DeckDSL Unit
setModel wiring model =
  case Array.head model.modelCards of
    Just pending → do
      st ← DCS.fromModel model <$>
        H.gets (DCS._stateMode .~ Preparing)
      setDeckState st
      runInitialEval wiring
    Nothing → do
      st ← DCS.fromModel model <$> H.get
      setDeckState st
      runCardUpdates st.id L.Nil

getModelCards ∷ DeckDSL (Array (DeckId × Card.Model))
getModelCards = do
  st ← H.get
  for st.modelCards \(deckId × card) → do
    currentState ←
      queryCardEval (deckId × card.cardId)
        $ H.request (SaveCard card.cardId $ Card.modelCardType card.model)
    pure $ deckId × (fromMaybe card currentState)
