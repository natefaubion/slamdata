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
  ( comp
  , initialState
  , module SlamData.Workspace.Deck.Component.Query
  , module DCS
  ) where

import SlamData.Prelude

import Control.Monad.Aff.Console (log)
import Control.Monad.Eff.Exception as Exn
import Control.Monad.Except.Trans (ExceptT(..), runExceptT)
import Control.UI.Browser (newTab, locationObject, locationString, setHref)

import Data.Array as Array
import Data.Foldable as Foldable
import Data.Lens ((.~), (%~), (^?), (?~))
import Data.Lens.Prism.Coproduct (_Right)
import Data.Path.Pathy ((</>))
import Data.Path.Pathy as Pathy
import Data.Set as S
import Data.Time (Milliseconds(..))
import Data.StrMap as SM

import Ace.Halogen.Component as Ace

import DOM.HTML.Location as Location

import Halogen as H
import Halogen.Component.Opaque.Unsafe (opaque, opaqueState)
import Halogen.Component.Utils (raise')
import Halogen.Component.Utils.Debounced (fireDebouncedQuery')
import Halogen.HTML.Events.Indexed as HE
import Halogen.HTML.Indexed as HH
import Halogen.HTML.Properties.Indexed as HP
import Halogen.Themes.Bootstrap3 as B
import Halogen.HTML.Properties.Indexed.ARIA as ARIA

import SlamData.Config (workspaceUrl)
import SlamData.Effects (Slam)
import SlamData.FileSystem.Resource as R
import SlamData.FileSystem.Routing (parentURL)
import SlamData.Quasar.Data (save, load) as Quasar
import SlamData.Render.Common (glyph)
import SlamData.Render.CSS as CSS
import SlamData.Workspace.AccessType as AT
import SlamData.Workspace.Action as WA
import SlamData.Workspace.Card.CardId (CardId())
import SlamData.Workspace.Card.CardType as CT
import SlamData.Workspace.Card.Common.EvalQuery as CEQ
import SlamData.Workspace.Card.Component (CardQueryP, CardQuery(..), InnerCardQuery, AnyCardQuery(..), _NextQuery)
import SlamData.Workspace.Card.Component.Query as CQ
import SlamData.Workspace.Card.JTable.Component as JTable
import SlamData.Workspace.Card.Next.Component as Next
import SlamData.Workspace.Card.OpenResource.Component as Open
import SlamData.Workspace.Card.Port (Port(..))
import SlamData.Workspace.Card.Port as Port
import SlamData.Workspace.Deck.BackSide.Component as Back
import SlamData.Workspace.Deck.Common (DeckHTML, DeckDSL)
import SlamData.Workspace.Deck.Component.ChildSlot (cpBackSide, cpCard, cpIndicator, ChildQuery, ChildSlot, CardSlot(..), cpDialog)
import SlamData.Workspace.Deck.Component.Query (QueryP, Query(..), DeckAction(..))
import SlamData.Workspace.Deck.Model (Deck)
import SlamData.Workspace.Deck.Component.State as DCS
import SlamData.Workspace.Deck.DeckId (DeckId(..), deckIdToString)
import SlamData.Workspace.Deck.Dialog.Component as Dialog
import SlamData.Workspace.Deck.Gripper as Gripper
import SlamData.Workspace.Deck.Indicator.Component as Indicator
import SlamData.Workspace.Deck.Model as Model
import SlamData.Workspace.Model as WS
import SlamData.Workspace.Deck.Slider as Slider
import SlamData.Workspace.Routing (mkWorkspaceHash, mkWorkspaceURL)
import SlamData.Workspace.StateMode (StateMode(..))

import Utils.DOM (getBoundingClientRect)
import Utils.Path (DirPath, FilePath)

initialState ∷ DCS.StateP
initialState = opaqueState DCS.initialDeck

comp ∷ H.Component DCS.StateP QueryP Slam
comp =
  opaque $ H.parentComponent
    -- Eta-expansion required by PSC because of the recursive reference of
    -- `comp` within `render`.
    { render: \s → render $ DCS.virtualState s
    , eval
    , peek: Just peek
    }

render ∷ DCS.VirtualState → DeckHTML
render vstate =
  case state.stateMode of
    Loading →
      HH.div
        [ HP.class_ CSS.board
        , HP.key "board"
        ]
        -- We need to render the cards but have them invisible during loading
        -- otherwise the various nested components won't initialise correctly.
        -- This div is required, along with the key, so that structurally it
        -- is in the same place in both `Loading` and `Ready` states.
        [ HH.div
            [ HP.class_ CSS.deck
            , HP.key "deck-container" ]
            [ Slider.render comp vstate $ state.displayMode ≡ DCS.Normal ]
        , HH.div
            [ HP.class_ CSS.loading ]
            []
        ]
    Ready →
      -- WARNING: Very strange things happen when this is not in a div; see SD-1326.
      HH.div
        ([ HP.class_ CSS.board
         , HP.key "board"
         ] ⊕ Slider.containerProperties vstate)
        [ HH.div
            [ HP.class_ CSS.deck
            , HP.key "deck-container"
            ]
            [ HH.button
                [ HP.classes [ CSS.flipDeck ]
                , HE.onClick (HE.input_ FlipDeck)
                , ARIA.label "Flip deck"
                , HP.title "Flip deck"
                ]
                [ HH.text "" ]
            , HH.button
                ([ ARIA.label "Zoom deck" ] ⊕
                 if state.topLevel
                   then [ HP.classes [ CSS.zoomOutDeck ]
                        , HP.title "Zoom out"
                        , HE.onClick (HE.input_ ZoomOut)
                        ]
                   else [ HP.classes [ CSS.zoomInDeck ]
                        , HP.title "Zoom in"
                        , HE.onClick (HE.input_ ZoomIn)
                        ])
                [ if state.topLevel
                    then glyph B.glyphiconZoomOut
                    else glyph B.glyphiconZoomIn ]
            , HH.button
                [ HP.classes [ CSS.grabDeck ]
                , HE.onMouseDown (HE.input GrabDeck)
                , ARIA.label "Grab deck"
                , HP.title "Grab deck"
                ]
                [ HH.text "" ]
            , Slider.render comp vstate $ state.displayMode ≡ DCS.Normal
            , HH.slot' cpIndicator unit \_ →
                { component: Indicator.comp
                , initialState: Indicator.initialState
                }
            , HH.button
                [ HP.classes [ CSS.resizeDeck ]
                , HE.onMouseDown (HE.input ResizeDeck)
                , ARIA.label "Resize deck"
                , HP.title "Resize deck"
                ]
                [ HH.text "" ]
            , renderBackside $ state.displayMode ≡ DCS.Backside
            , renderDialog $ state.displayMode ≡ DCS.Dialog
            ]
        ]

    Error err →
      HH.div
        [ HP.classes [ B.alert, B.alertDanger ] ]
        [ HH.h1
            [ HP.class_ B.textCenter ]
            [ HH.text err ]
        ]

  where
  state = DCS.runVirtualState vstate

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
             (isJust state.initialSliderX)
             (Gripper.gripperDefsForCardId state.cards $ DCS.activeCardId vstate)
             ⊕ [ HH.slot' cpBackSide unit \_ →
                  { component: Back.comp
                  , initialState: Back.initialState
                  }
               ]
          )
      ]

eval ∷ Natural Query DeckDSL
eval (RunActiveCard next) = do
  traverse_ runCard =<< H.gets (DCS.activeCardId ∘ DCS.virtualState)
  pure next
eval (Load dir deckId topLevel next) = do
  H.modify $ DCS._topLevel .~ topLevel
  loadDeck dir deckId
  pure next
eval (SetModel deckId model topLevel next) = do
  state ← H.get
  H.modify $ DCS._topLevel .~ topLevel
  setModel state.path (Just deckId) model
  pure next
eval (ExploreFile res next) = do
  H.modify
    $ (DCS._activeCardIndex .~ one)
    ∘ (DCS.addCard CT.JTable)
    ∘ (DCS.addCard CT.OpenResource)
  H.query' cpCard (CardSlot zero)
    $ right
    $ H.ChildF unit
    $ right
    $ OpenResourceQuery
    $ right
    $ H.action
    $ Open.ResourceSelected
    $ R.File res
  runCard zero
  -- Flush the eval queue
  saveDeck
  updateIndicator
  pure next
eval (Publish next) = do
  H.gets DCS.deckPath >>=
    traverse_ (H.fromEff ∘ newTab ∘ flip mkWorkspaceURL (WA.Load AT.ReadOnly))
  pure next
eval (Reset dir next) = do
  setDeckState $ DCS.initialDeck
    { path = dir
    , stateMode = Ready
    }
  updateIndicator
  pure next
eval (SetName name next) =
  H.modify (DCS._name .~ Just name) $> next
eval (SetParent parent next) =
  H.modify (DCS._parent .~ Just parent) $> next
eval (SetAccessType aType next) = do
  void $ H.queryAll' cpCard $ left $ H.action $ SetCardAccessType aType
  H.modify $ DCS._accessType .~ aType
  unless (AT.isEditable aType)
    $ H.modify (DCS._displayMode .~ DCS.Normal)
  pure next
eval (GetPath k) = k <$> H.gets DCS.deckPath
eval (GetId k) = k <$> H.gets _.id
eval (GetParent k) = k <$> H.gets _.parent
eval (Save next) = saveDeck $> next
eval (RunPendingCards next) = do
  -- Only run pending cards if we have a deckPath. Some cards run with the
  -- assumption that the deck is saved to disk.
  H.gets DCS.deckPath >>= traverse_ \_ → runPendingCards
  pure next
eval (GetGlobalVarMap k) = k <$> H.gets _.globalVarMap
eval (SetGlobalVarMap m next) = do
  st ← H.get
  when (m ≠ st.globalVarMap) do
    H.modify $ DCS._globalVarMap .~ m
    traverse_ runCard $ DCS.cardsOfType CT.API st
  pure next
eval (FlipDeck next) = do
  updateBackSide
  H.modify $
    DCS._displayMode %~
      case _ of
        DCS.Normal → DCS.Backside
        _ → DCS.Normal
  pure next
eval (GrabDeck _ next) = pure next
eval (ResizeDeck _ next) = pure next
eval (ZoomIn next) = do
  st ← H.get
  for_ (DCS.deckPath st) \path → do
    let deckHash = mkWorkspaceHash path (WA.Load st.accessType) st.globalVarMap
    H.fromEff $ locationObject >>= Location.setHash deckHash
  pure next
eval (ZoomOut next) = do
  st ← H.get
  for_ st.path \path →
    case st.parent of
      Just (Tuple deckId _) → do
        let deckHash = mkWorkspaceHash (DCS.deckPath' path deckId) (WA.Load st.accessType) st.globalVarMap
        H.fromEff $ locationObject >>= Location.setHash deckHash
      Nothing →
        void $ H.fromEff $ setHref $ parentURL $ Left path
  pure next
eval (StartSliding mouseEvent next) =
  Slider.startSliding mouseEvent $> next
eval (StopSlidingAndSnap mouseEvent next) = do
  Slider.stopSlidingAndSnap mouseEvent
  updateIndicator
  pure next
eval (UpdateSliderPosition mouseEvent next) =
  Slider.updateSliderPosition mouseEvent $> next
eval (SetCardElement element next) =
  next <$ for_ element \el → do
    width ← getBoundingClientWidth el
    H.modify (DCS._cardElementWidth ?~ width)
  where
  getBoundingClientWidth =
    H.fromEff ∘ map _.width ∘ getBoundingClientRect
eval (StopSliderTransition next) =
  H.modify (DCS._sliderTransition .~ false) $> next
eval (DoAction _ next) = pure next

peek ∷ ∀ a. H.ChildF ChildSlot ChildQuery a → DeckDSL Unit
peek (H.ChildF s q) =
  (peekCards ⊹ (\_ _ → pure unit) $ s)
   ⨁ peekBackSide
   ⨁ (const $ pure unit)
   ⨁ (peekDialog ⨁ (const $ pure unit))
   $ q

peekDialog ∷ ∀ a. Dialog.Query a → DeckDSL Unit
peekDialog (Dialog.Show _ _) =
  H.modify (DCS._displayMode .~ DCS.Dialog)
peekDialog (Dialog.Dismiss _) =
  H.modify (DCS._displayMode .~ DCS.Backside)
peekDialog (Dialog.Confirm d b _) = do
  H.modify (DCS._displayMode .~ DCS.Backside)
  case d of
    Dialog.DeleteDeck | b → raise' $ H.action $ DoAction DeleteDeck
    _ → pure unit

peekBackSide ∷ ∀ a. Back.Query a → DeckDSL Unit
peekBackSide (Back.UpdateFilter _ _) = pure unit
peekBackSide (Back.UpdateCardType _ _) = pure unit
peekBackSide (Back.DoAction action _) =
  case action of
    Back.Trash → do
      state ← H.get
      lastId ← H.gets DCS.findLastRealCard
      for_ (DCS.activeCardId (DCS.virtualState state) <|> lastId) \trashId → do
        H.modify $ DCS.removeCard trashId
        triggerSave
        updateNextActionCard
        updateIndicator
        H.modify $ DCS._displayMode .~ DCS.Normal
    Back.Share → do
      url ← mkShareURL SM.empty
      for_ url $ showDialog ∘ Dialog.Share
      H.modify (DCS._displayMode .~ DCS.Dialog)
    Back.Embed → do
      varMap ← H.gets _.globalVarMap
      url ← mkShareURL varMap
      for_ url (showDialog ∘ flip Dialog.Embed varMap)
      H.modify (DCS._displayMode .~ DCS.Dialog)
    Back.Publish →
      H.gets DCS.deckPath >>=
        traverse_ (H.fromEff ∘ newTab ∘ flip mkWorkspaceURL (WA.Load AT.ReadOnly))
    Back.DeleteDeck → do
      cards ← H.gets _.cards
      if Array.null cards
        then raise' $ H.action $ DoAction DeleteDeck
        else do
          showDialog Dialog.DeleteDeck
          H.modify (DCS._displayMode .~ DCS.Dialog)
    Back.Mirror → raise' $ H.action $ DoAction Mirror
    Back.Wrap → raise' $ H.action $ DoAction Wrap

mkShareURL ∷ Port.VarMap → DeckDSL (Maybe String)
mkShareURL varMap = do
  loc ← H.fromEff locationString
  saveDeck
  path ← H.gets DCS.deckPath
  pure $ path <#> \p →
    loc ⊕ "/" ⊕ workspaceUrl ⊕ mkWorkspaceHash p (WA.Load AT.ReadOnly) varMap

peekCards ∷ ∀ a. CardSlot → CardQueryP a → DeckDSL Unit
peekCards (CardSlot cardId) = peekCard cardId ⨁ peekCardInner cardId

-- | Peek on the card component to observe actions from the card control
-- | buttons.
peekCard ∷ ∀ a. CardId → CardQuery a → DeckDSL Unit
peekCard cardId = case _ of
  RunCard _ → runCard cardId
  RefreshCard _ → traverse_ runCard =<< H.gets DCS.findFirst
  StopCard _ → do
    -- TODO: does it even make sense to have a stop action on cards anymore?
    -- I don't think there's a button for it anyway. Maybe the deck as a whole
    -- should be stoppable via an action on the back? -gb
    H.modify $ DCS.removePendingCard cardId
    runPendingCards
  _ → pure unit

showDialog ∷ Dialog.Dialog → DeckDSL Unit
showDialog =
  queryDialog
    ∘ H.action
    ∘ Dialog.Show

queryDialog ∷ Dialog.Query Unit → DeckDSL Unit
queryDialog q = H.query' cpDialog unit (left q) *> pure unit

queryCard ∷ ∀ a. CardId → CQ.AnyCardQuery a → DeckDSL (Maybe a)
queryCard cid =
  H.query' cpCard (CardSlot cid)
    ∘ right
    ∘ H.ChildF unit
    ∘ right

updateIndicatorAndNextAction ∷ DeckDSL Unit
updateIndicatorAndNextAction = do
  updateIndicator
  updateNextActionCard

updateIndicator ∷ DeckDSL Unit
updateIndicator = do
  cids ← H.gets $ map _.id ∘ _.cards ∘ DCS.runVirtualState ∘ DCS.virtualState
  outs ←
    for cids \cid → do
      map join
        $ H.query' cpCard (CardSlot cid)
        $ left (H.request GetOutput)
  H.query' cpIndicator unit
    $ H.action
    $ Indicator.UpdatePortList outs
  vid ← H.gets $ DCS.runVirtualIndex ∘ _.activeCardIndex
  void $ H.query' cpIndicator unit $ H.action $ Indicator.UpdateActiveId vid

updateBackSide ∷ DeckDSL Unit
updateBackSide = do
  state ← H.gets DCS.virtualState
  let ty = DCS.activeCardType state
  void
    $ H.query' cpBackSide unit
    $ H.action
    $ Back.UpdateCardType ty

updateNextActionCard ∷ DeckDSL Unit
updateNextActionCard = do
  mbLid ← H.gets DCS.findLastRealCard
  inputPort ←
    map join $ for mbLid \lid →
      map join $ H.query' cpCard (CardSlot lid) $ left (H.request GetOutput)

  path ← H.gets DCS.deckPath
  globalVarMap ← H.gets _.globalVarMap
  let
    info ∷ CEQ.CardEvalInput
    info = { path, inputPort, cardId: top, globalVarMap }

  void
    $ H.query' cpCard (CardSlot top)
    $ left $ H.request (UpdateCard info)

createCard ∷ CT.CardType → DeckDSL Unit
createCard cardType = do
  cid ← H.gets DCS.findLastRealCard
  case cid of
    Nothing →
      H.modify $ DCS.addCard cardType
    Just cardId → do
      (st × newCardId) ← H.gets $ DCS.addCard' cardType

      setDeckState st
      input ←
        map join $ H.query' cpCard (CardSlot cardId) $ left (H.request GetOutput)
      for_ input \input' → do
        path ← H.gets DCS.deckPath
        let setupInfo = { path, inputPort: input', cardId: newCardId }
        void
          $ H.query' cpCard  (CardSlot newCardId)
          $ right
          $ H.ChildF unit
          $ left
          $ H.action (CEQ.SetupCard setupInfo)

      runCard newCardId

  updateIndicatorAndNextAction
  triggerSave

-- | Peek on the inner card components to observe `NotifyRunCard`, which is
-- | raised by actions within a card that should cause the card to run.
peekCardInner
  ∷ ∀ a. CardId → H.ChildF Unit InnerCardQuery a → DeckDSL Unit
peekCardInner cardId (H.ChildF _ q) =
  (peekEvalCard cardId) ⨁ (peekAnyCard cardId) $ q

peekEvalCard ∷ ∀ a. CardId → CEQ.CardEvalQuery a → DeckDSL Unit
peekEvalCard cardId (CEQ.NotifyRunCard _) = runCard cardId
peekEvalCard _ _ = pure unit

peekAnyCard ∷ ∀ a. CardId → AnyCardQuery a → DeckDSL Unit
peekAnyCard cardId q = do
  for_ (q ^? _NextQuery ∘ _Right ∘ Next._AddCardType) createCard
  when (queryShouldRun q) $ runCard cardId
  when (queryShouldSave q) triggerSave
  pure unit

queryShouldRun ∷ ∀ a. AnyCardQuery a → Boolean
queryShouldRun (SaveQuery q) = false
queryShouldRun (JTableQuery q) = coproduct (const true) jTableQueryShouldRun q
queryShouldRun _ = true

jTableQueryShouldRun ∷ ∀ a. JTable.Query a → Boolean
jTableQueryShouldRun (JTable.StartEnterCustomPageSize _) = false
jTableQueryShouldRun _ = true

queryShouldSave  ∷ ∀ a. AnyCardQuery a → Boolean
queryShouldSave (AceQuery q) = coproduct (const true) aceQueryShouldSave q
queryShouldSave _ = true

aceQueryShouldSave ∷ ∀ p a. H.ChildF p Ace.AceQuery a → Boolean
aceQueryShouldSave = H.runChildF >>> case _ of
  Ace.TextChanged _ → true
  _ → false

-- | Runs all card that are present in the set of pending cards.
runPendingCards ∷ DeckDSL Unit
runPendingCards = do
  H.gets _.pendingCard >>= traverse_ \pendingCard -> do
    cards ← H.gets (_.cards ∘ DCS.runVirtualState ∘ DCS.virtualState)
    path ← H.gets DCS.deckPath
    globalVarMap ← H.gets _.globalVarMap
    void $ Array.foldM (runStep pendingCard path globalVarMap) Nothing (_.id <$> cards)
    updateIndicatorAndNextAction
    -- triggerSave <-- why?
  where
  runStep
    :: CardId
    -> Maybe DirPath
    → Port.VarMap
    → Maybe Port
    → CardId
    → DeckDSL (Maybe Port)
  runStep pendingCard path globalVarMap inputPort cardId =
    if cardId < pendingCard
    then map join
      $ H.query' cpCard (CardSlot cardId)
      $ left (H.request GetOutput)
    else do
      let input = { path, inputPort, cardId, globalVarMap }
      outputPort ←
        map join
          $ H.query' cpCard (CardSlot cardId)
          $ left $ H.request (UpdateCard input)
      H.modify $ DCS._failingCards %~ case outputPort of
        Just (CardError msg) → S.insert cardId
        _ → S.delete cardId
      pure outputPort

-- | Enqueues the card with the specified ID in the set of cards that are
-- | pending to run and enqueues a debounced H.query to trigger the cards to
-- | actually run.
runCard ∷ CardId → DeckDSL Unit
runCard cardId = do
  H.modify (DCS.addPendingCard cardId)
  fireDebouncedQuery' (Milliseconds 500.0) DCS._runTrigger RunPendingCards

-- | Triggers the H.query for autosave. This does not immediate perform the save
-- | H.action, but instead enqueues a debounced H.query to trigger the actual save.
triggerSave ∷ DeckDSL Unit
triggerSave = fireDebouncedQuery' (Milliseconds 500.0) DCS._saveTrigger Save

-- | Saves the deck as JSON, using the current values present in the state.
saveDeck ∷ DeckDSL Unit
saveDeck = H.get >>= \st →
  if isUnsaved st ∧ isNewExploreDeck st
  -- If it's an unsaved Explore deck, it is safe to go ahead and run it.
  then runPendingCards
  else do
    cards ← Array.catMaybes <$> for st.cards \card →
      if card.id ≡ top
        then pure Nothing
        else
        H.query' cpCard (CardSlot card.id)
          $ left
          $ H.request (SaveCard card.id card.ty)

    let json = Model.encode { name: st.name, parent: st.parent, cards }

    for_ st.path \path → do
      deckId ← runExceptT do
        i ← ExceptT $ genId path st.id
        ExceptT $ Quasar.save (deckIndex path i) json
        pure i

      case deckId of
        Left err → do
          -- TODO: do something to notify the user saving failed
          pure unit
        Right deckId' → do
          H.modify $ DCS._id .~ Just deckId'
          -- runPendingCards would be deferred if there had previously been
          -- no `deckPath`. We need to flush the queue.
          when (isNothing $ DCS.deckPath st) runPendingCards
          when st.topLevel $ H.gets DCS.deckPath >>= traverse_ \path' → do
            let deckHash = mkWorkspaceHash path' (WA.Load st.accessType) st.globalVarMap
            H.fromEff $ locationObject >>= Location.setHash deckHash

  where

  isUnsaved ∷ DCS.State → Boolean
  isUnsaved = isNothing ∘ DCS.deckPath

  isNewExploreDeck ∷ DCS.State → Boolean
  isNewExploreDeck { cards } =
    let
      cardArrays = map _.ty cards
    in
      cardArrays ≡ [ CT.OpenResource ] ∨ cardArrays ≡ [ CT.OpenResource, CT.JTable ]

  genId ∷ DirPath → Maybe DeckId → DeckDSL (Either Exn.Error DeckId)
  genId path deckId = case deckId of
    Just id' → pure $ Right id'
    Nothing → map DeckId <$> WS.freshId (path </> Pathy.file "index")

deckIndex ∷ DirPath → DeckId → FilePath
deckIndex path deckId =
  path </> Pathy.dir (deckIdToString deckId) </> Pathy.file "index"

setDeckState ∷ DCS.State → DeckDSL Unit
setDeckState newState =
  H.modify \oldState →
    newState { cardElementWidth = oldState.cardElementWidth }

loadDeck ∷ DirPath → DeckId → DeckDSL Unit
loadDeck dir deckId = do
  H.modify $ DCS._stateMode .~ Loading
  json ← Quasar.load $ deckIndex dir deckId
  case Model.decode =<< json of
    Left err → do
      H.fromAff $ log err
      H.modify $ DCS._stateMode .~ Error "There was a problem decoding the saved deck"
    Right model →
      setModel (Just dir) (Just deckId) model

setModel ∷ Maybe DirPath → Maybe DeckId → Deck → DeckDSL Unit
setModel dir deckId model = do
  state ← H.get
  case DCS.fromModel dir deckId model state of
    Tuple cards st → do
      setDeckState st
      hasRun ← Foldable.or <$> for cards \card → do
        H.query' cpCard (CardSlot card.cardId)
          $ left $ H.action $ LoadCard card
        pure card.hasRun
      when hasRun $ traverse_ runCard (DCS.findFirst st)
      H.modify (DCS._stateMode .~ Ready)
  updateIndicator
