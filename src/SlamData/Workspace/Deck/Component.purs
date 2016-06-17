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
import Control.Monad.Aff.Par (Par(..), runPar)
import Control.UI.Browser (newTab, locationObject, locationString, setHref)

import Data.Array as Array
import Data.Lens as Lens
import Data.Lens ((.~), (%~), (^?), (?~))
import Data.Lens.Prism.Coproduct (_Right)
import Data.List as L
import Data.Map as Map
import Data.Set as Set
import Data.Ord (max)
import Data.Path.Pathy ((</>))
import Data.Path.Pathy as Pathy
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
import SlamData.Workspace.Card.CardId (CardId(..), _CardId)
import SlamData.Workspace.Card.CardType as CT
import SlamData.Workspace.Card.Model as Card
import SlamData.Workspace.Card.Eval as Eval
import SlamData.Workspace.Card.Component (CardQueryP, CardQuery(..), InnerCardQuery, AnyCardQuery(..), _NextQuery)
import SlamData.Workspace.Card.Component.Query as CQ
import SlamData.Workspace.Card.Draftboard.Common as DBC
import SlamData.Workspace.Card.JTable.Component as JTable
import SlamData.Workspace.Card.Next.Component as Next
import SlamData.Workspace.Card.Port (Port)
import SlamData.Workspace.Card.Port as Port
import SlamData.Workspace.Deck.BackSide.Component as Back
import SlamData.Workspace.Deck.Common (DeckHTML, DeckDSL)
import SlamData.Workspace.Deck.Component.ChildSlot (cpBackSide, cpCard, cpIndicator, ChildQuery, ChildSlot, CardSlot(..), cpDialog)
import SlamData.Workspace.Deck.Component.Query (QueryP, Query(..), DeckAction(..))
import SlamData.Workspace.Deck.Component.State as DCS
import SlamData.Workspace.Deck.DeckId (DeckId)
import SlamData.Workspace.Deck.DeckLevel as DL
import SlamData.Workspace.Deck.Dialog.Component as Dialog
import SlamData.Workspace.Deck.Gripper as Gripper
import SlamData.Workspace.Deck.Indicator.Component as Indicator
import SlamData.Workspace.Deck.Model (Deck, deckIndex)
import SlamData.Workspace.Deck.Model as Model
import SlamData.Workspace.Deck.Slider as Slider
import SlamData.Workspace.Model as WM
import SlamData.Workspace.Routing (mkWorkspaceHash, mkWorkspaceURL)
import SlamData.Workspace.StateMode (StateMode(..))

import Utils.DOM (getBoundingClientRect)
import Utils.Path (DirPath)

initialState ∷ DirPath → DeckId → DCS.StateP
initialState path = opaqueState ∘ DCS.initialDeck path

comp ∷ H.Component DCS.StateP QueryP Slam
comp =
  opaque $ H.parentComponent
    { render: \x → render x -- eta expansion required because of mutual recursion
    , eval
    , peek: Just peek
    }

render ∷ DCS.State → DeckHTML
render st =
  case st.stateMode of
    Error err → renderError err
    _ →
      -- WARNING: Very strange things happen when this is not in a div; see SD-1326.
      HH.div
        ([ HP.class_ CSS.board
         , HP.key "board"
         , HE.onMouseUp (HE.input_ UpdateCardSize)
         ] ⊕ Slider.containerProperties st)
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
            , if st.level ≡ DL.root
                then HH.button
                       [ ARIA.label "Zoom deck"
                       , HP.classes [ CSS.zoomOutDeck ]
                       , HP.title "Zoom out"
                       , HE.onClick (HE.input_ ZoomOut)
                       ]
                       [ glyph B.glyphiconZoomOut ]
                else HH.button
                       [ ARIA.label "Zoom deck"
                       , HP.classes [ CSS.zoomInDeck ]
                       , HP.title "Zoom in"
                       , HE.onClick (HE.input_ ZoomIn)
                       ]
                       [ glyph B.glyphiconZoomIn ]
            , HH.button
                [ HP.classes [ CSS.grabDeck ]
                , HE.onMouseDown (HE.input GrabDeck)
                , ARIA.label "Grab deck"
                , HP.title "Grab deck"
                ]
                [ HH.text "" ]
            , Slider.render comp st $ st.displayMode ≡ DCS.Normal
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
            , renderBackside $ st.displayMode ≡ DCS.Backside
            , renderDialog $ st.displayMode ≡ DCS.Dialog
            ]
        ]

  where

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

eval ∷ Query ~> DeckDSL
eval (Load dir deckId level next) = do
  H.modify $ DCS._level .~ level
  loadDeck dir deckId
  pure next
eval (SetModel deckId model level next) = do
  state ← H.get
  H.modify $ DCS._level .~ level
  setModel state.path deckId model
  pure next
eval (ExploreFile res next) = do
  H.modify
    $ (DCS.addCard $ Card.cardModelOfType CT.JTable)
    ∘ (DCS.addCard ∘ Card.OpenResource ∘ Just $ R.File res)
    ∘ (DCS._stateMode .~ Preparing)
  H.gets (map DCS.coordModelToCoord ∘ Array.head ∘ _.modelCards) >>= traverse \cid → do
    H.modify $ DCS.addPendingCard cid
    runPendingCards
  saveDeck
  updateIndicator
  pure next
eval (Publish next) = do
  path ← H.gets DCS.deckPath
  H.fromEff ∘ newTab $ mkWorkspaceURL path (WA.Load AT.ReadOnly)
  pure next
eval (Reset dir next) = do
  st ← H.get
  setDeckState $
    (DCS.initialDeck dir st.id)
      { stateMode = Ready
      , accessType = st.accessType
      }
  runPendingCards
  updateActiveCardAndIndicator
  pure next
eval (SetParent parent next) =
  H.modify (DCS._parent .~ Just parent) $> next
eval (GetId k) = k <$> H.gets _.id
eval (GetParent k) = k <$> H.gets _.parent
eval (Save next) = saveDeck $> next
eval (RunPendingCards next) = do
  runPendingCards
  pure next
eval (SetGlobalVarMap m next) = do
  st ← H.get
  when (m ≠ st.globalVarMap) do
    H.modify $ DCS._globalVarMap .~ m
    traverse_ runCard $ DCS.apiCards st
  pure next
eval (FlipDeck next) = do
  updateBackSide
  H.modify
    $ DCS._displayMode
    %~ case _ of
      DCS.Normal → DCS.Backside
      _ → DCS.Normal
  pure next
eval (GrabDeck _ next) = pure next
eval (UpdateCardSize next) = do
  H.queryAll' cpCard $ left $ H.action UpdateDimensions
  pure next
eval (ResizeDeck _ next) = pure next
eval (ZoomIn next) = do
  st ← H.get
  let deckHash = mkWorkspaceHash (DCS.deckPath st) (WA.Load st.accessType) st.globalVarMap
  H.fromEff $ locationObject >>= Location.setHash deckHash
  pure next
eval (ZoomOut next) = do
  st ← H.get
  case st.parent of
    Just (Tuple deckId _) → do
      let deckHash = mkWorkspaceHash (DCS.deckPath' st.path deckId) (WA.Load st.accessType) st.globalVarMap
      H.fromEff $ locationObject >>= Location.setHash deckHash
    Nothing →
      void $ H.fromEff $ setHref $ parentURL $ Left st.path
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
      for_ (DCS.activeCardCoord state <|> lastId) \trashId → do
        let rem = DCS.removeCard trashId state
        DBC.childDeckIds (snd <$> fst rem) #
          H.fromAff ∘ runPar ∘ traverse_ (Par ∘ DBC.deleteGraph state.path)
        H.set $ snd rem
        triggerSave
        updateActiveCardAndIndicator
        H.modify $ DCS._displayMode .~ DCS.Normal
        runPendingCards
      void $ H.queryAll' cpCard $ left $ H.action UpdateDimensions
    Back.Share → do
      url ← mkShareURL SM.empty
      showDialog $ Dialog.Share url
      H.modify (DCS._displayMode .~ DCS.Dialog)
    Back.Embed → do
      varMap ← H.gets _.globalVarMap
      url ← mkShareURL varMap
      showDialog $ Dialog.Embed url varMap
      H.modify (DCS._displayMode .~ DCS.Dialog)
    Back.Publish → do
      path ← H.gets DCS.deckPath
      H.fromEff ∘ newTab $ mkWorkspaceURL path (WA.Load AT.ReadOnly)
    Back.DeleteDeck → do
      cards ← H.gets _.modelCards
      if Array.null cards
        then raise' $ H.action $ DoAction DeleteDeck
        else do
          showDialog Dialog.DeleteDeck
          H.modify (DCS._displayMode .~ DCS.Dialog)
    Back.Mirror → raise' $ H.action $ DoAction Mirror
    Back.Wrap → raise' $ H.action $ DoAction Wrap

mkShareURL ∷ Port.VarMap → DeckDSL String
mkShareURL varMap = do
  loc ← H.fromEff locationString
  saveDeck
  path ← H.gets DCS.deckPath
  pure $ loc ⊕ "/" ⊕ workspaceUrl ⊕ mkWorkspaceHash path (WA.Load AT.ReadOnly) varMap

peekCards ∷ ∀ a. CardSlot → CardQueryP a → DeckDSL Unit
peekCards (CardSlot cardId) = const (pure unit) ⨁ peekCardInner cardId

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

createCard ∷ CT.CardType → DeckDSL Unit
createCard cardType =
  H.gets _.id >>= \deckId → do
    (st × newCardId) ← H.gets ∘ DCS.addCard' $ Card.cardModelOfType cardType
    setDeckState st
    runCard (deckId × newCardId)
    updateActiveCardAndIndicator
    triggerSave

peekCardInner
  ∷ ∀ a
  . DeckId × CardId
  → H.ChildF Unit InnerCardQuery a
  → DeckDSL Unit
peekCardInner cardCoord (H.ChildF _ q) =
  const (pure unit) ⨁ (peekAnyCard cardCoord) $ q

peekAnyCard ∷ ∀ a. DeckId × CardId → AnyCardQuery a → DeckDSL Unit
peekAnyCard cardCoord q = do
  for_ (q ^? _NextQuery ∘ _Right ∘ Next._AddCardType) createCard
  when (queryShouldRun q) $ runCard cardCoord
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

type RunCardsConfig =
  { pendingCardId ∷ Maybe (DeckId × CardId)
  , path ∷ DirPath
  , globalVarMap ∷ Port.VarMap
  , accessType ∷ AT.AccessType
  , deckId ∷ DeckId
  , modelCards ∷ Array (DeckId × Card.Model)
  }

type RunCardsMachine =
  { state ∷ Maybe Port
  , cards ∷ L.List (DeckId × Card.Model)
  , stack ∷ L.List (DeckId × Card.Model)
  , outputs ∷ Map.Map (DeckId × CardId) Port
  }

initialRunCardsState ∷ L.List (DeckId × Card.Model) → RunCardsMachine
initialRunCardsState input =
  { state: Nothing
  , cards: mempty
  , outputs: mempty
  , stack: input
  }

runCardsStateIsTerminal ∷ RunCardsConfig → RunCardsMachine → Boolean
runCardsStateIsTerminal cfg m =
  case m.stack of
    L.Nil →
      case _.cardId ∘ snd <$> L.head m.cards of
        Just ErrorCardId → true
        Just NextActionCardId → true
        _ → not $ AT.isEditable cfg.accessType
    _ → false

stepRunCards
  ∷ RunCardsConfig
  → RunCardsMachine
  → DeckDSL RunCardsMachine
stepRunCards cfg m @ { state, cards, stack, outputs } = do
  case stack of
    L.Nil →
      pure case L.head cards of
        Just card →
          case Map.lookup (DCS.coordModelToCoord card) outputs of
            Just (Port.CardError _) → pushCard errorCard m
            _ → pushNextActionCard m
        Nothing → pushNextActionCard m
    L.Cons x stack' → do
      let
        coord = DCS.coordModelToCoord x
        ltPending =
          fromMaybe true do
            coord' ← cfg.pendingCardId
            eq LT <$> DCS.compareCoordCards coord coord' cfg.modelCards
      state' ←
        if ltPending
          then pure $ Map.lookup coord outputs
          else Just <$> runStep cfg state x
      let
        cards' = L.Cons x cards
        outputs' = maybe id (Map.insert (DCS.coordModelToCoord x)) state' outputs
        stack'' =
          case state' of
            Just (Port.CardError _) → L.Nil
            _ → stack'
      pure
        { state: state'
        , cards: cards'
        , stack: stack''
        , outputs: outputs'
        }

  where
    pushCard card m =
      m { stack = L.Cons (cfg.deckId × card) m.stack }

    pushNextActionCard =
      if AT.isEditable cfg.accessType
      then pushCard nextActionCard
      else id

evalRunCardsMachine
  ∷ RunCardsConfig
  → RunCardsMachine
  → DeckDSL RunCardsMachine
evalRunCardsMachine cfg m =
  if runCardsStateIsTerminal cfg m
  then pure m
  else evalRunCardsMachine cfg =<< stepRunCards cfg m


-- | If the card model is represented in the live / Halogen deck, then we query it for its
-- | current state. Otherwise, we use the data we have stored in the model.
currentStateOfCard
  ∷ DeckId × Card.Model
  → DeckDSL (DeckId × Card.Model)
currentStateOfCard cm @ (deckId × card) =
  Tuple deckId ∘ fromMaybe card <$>
    queryCardEval (DCS.coordModelToCoord cm)
      (H.request (SaveCard card.cardId (Card.modelCardType card.model)))

runStep
  ∷ RunCardsConfig
  → Maybe Port
  → DeckId × Card.Model
  → DeckDSL Port
runStep cfg inputPort cm = do
  let
    input =
      { path: cfg.path
      , input: inputPort
      , cardCoord: DCS.coordModelToCoord cm
      , globalVarMap: cfg.globalVarMap
      }

  card' ← currentStateOfCard cm
  case Card.modelToEval (snd card').model of
    Left err → pure ∘ Port.CardError $ "Could not evaluate card: " <> err
    Right cmd → Eval.runEvalCard input cmd

type CardUpdate =
  { card ∷ DeckId × Card.Model
  , input ∷ Maybe Port
  , output ∷ Maybe Port
  }

displayCardUpdates ∷ DCS.State → RunCardsMachine → Array CardUpdate
displayCardUpdates st m =
  let
    outputs = st.displayCards <#> \c → Map.lookup (DCS.coordModelToCoord c) m.outputs
    inputs = Array.take (Array.length outputs) $ Array.cons Nothing outputs
  in
    Array.zipWith
      (\c f → f c)
      st.displayCards
      (Array.zipWith (\input output card → { card, input, output }) inputs outputs)

applyPendingState
  ∷ DeckId
  → Array (DeckId × Card.Model)
  → Array (DeckId × Card.Model)
applyPendingState deckId cards =
  Array.snoc realCards $
    deckId × { cardId: PendingCardId, model: Card.PendingCard }
  where
    realCards = Array.filter (Lens.has _CardId ∘ _.cardId ∘ snd) cards

-- | Runs all card that are present in the set of pending cards.
runPendingCards ∷ DeckDSL Unit
runPendingCards = do
  st ← H.get
  let
    config =
      { pendingCardId: st.pendingCard
      , path: st.path
      , globalVarMap: st.globalVarMap
      , accessType: st.accessType
      , modelCards: st.modelCards
      , deckId: st.id
      }
    initialMachineState =
      initialRunCardsState $ L.toList st.modelCards

  unless (runCardsStateIsTerminal config initialMachineState) do
    H.modify $ DCS._displayCards %~ applyPendingState st.id

  result ← evalRunCardsMachine config initialMachineState

  H.modify $ DCS._displayCards .~ L.fromList (L.reverse result.cards)
  state ← H.get
  traverse_ (updateCard st.path st.globalVarMap) $ displayCardUpdates state result

  when (st.stateMode == Preparing) do
    lastIndex ← H.gets DCS.findLastRealCardIndex
    H.modify
      $ (DCS._stateMode .~ Ready)
      ∘ (DCS._activeCardIndex .~ lastIndex)

  updateActiveCardAndIndicator
  where

  updateCard
    ∷ DirPath
    → Port.VarMap
    → CardUpdate
    → DeckDSL Unit
  updateCard path globalVarMap { card, input = mport, output } = do
    let cardCoord = DCS.coordModelToCoord card
    shouldLoad ← H.gets $ Set.member cardCoord ∘ _.cardsToLoad
    accessType ← H.gets _.accessType
    let input = { path, input: mport, cardCoord, globalVarMap }

    when shouldLoad do
      res ← H.query' cpCard (CardSlot cardCoord) $ left $ H.action (LoadCard (snd card))
      for_ res \_ →
        H.modify $ DCS._cardsToLoad %~ Set.delete cardCoord

    void $ H.query' cpCard (CardSlot cardCoord) $ left $ H.action (UpdateCard input output)

-- | Enqueues the card with the specified ID in the set of cards that are
-- | pending to run and enqueues a debounced H.query to trigger the cards to
-- | actually run.
runCard ∷ DeckId × CardId → DeckDSL Unit
runCard coord = do
  H.modify (DCS.addPendingCard coord)
  fireDebouncedQuery' (Milliseconds 500.0) DCS._runTrigger RunPendingCards

-- | Triggers the H.query for autosave. This does not immediate perform the save
-- | H.action, but instead enqueues a debounced H.query to trigger the actual save.
triggerSave ∷ DeckDSL Unit
triggerSave = fireDebouncedQuery' (Milliseconds 500.0) DCS._saveTrigger Save

-- | Saves the deck as JSON, using the current values present in the state.
saveDeck ∷ DeckDSL Unit
saveDeck = do
  st ← H.get
  when (AT.isEditable st.accessType) do
    cards ← for st.modelCards \(deckId × card) → do
      currentState ←
        queryCardEval (deckId × card.cardId)
          $ H.request (SaveCard card.cardId $ Card.modelCardType card.model)
      pure $ deckId × (fromMaybe card currentState)

    H.modify $ DCS._modelCards .~ cards

    let
      index = st.path </> Pathy.file "index"
      json = Model.encode
        { parent: st.parent
        , mirror: st.mirror
        , cards: snd <$> cards
        }

    when (isNothing st.parent) do
      WM.getRoot index >>= case _ of
        Left _ → void $ WM.setRoot index st.id
        Right _ → pure unit

    Quasar.save (deckIndex st.path st.id) json >>= case _ of
      Left err → do
        -- TODO: do something to notify the user saving failed
        pure unit
      Right _ →
        when (st.level ≡ DL.root) $ do
          path' ← H.gets DCS.deckPath
          let deckHash = mkWorkspaceHash path' (WA.Load st.accessType) st.globalVarMap
          H.fromEff $ locationObject >>= Location.setHash deckHash

setDeckState ∷ DCS.State → DeckDSL Unit
setDeckState newState =
  H.modify \oldState →
    newState { cardElementWidth = oldState.cardElementWidth }

loadDeck ∷ DirPath → DeckId → DeckDSL Unit
loadDeck dir deckId = do
  H.modify
    $ (DCS._stateMode .~ Loading)
    ∘ (DCS._displayCards .~ applyPendingState deckId [])
  json ← Quasar.load $ deckIndex dir deckId
  case Model.decode =<< json of
    Left err → do
      H.fromAff $ log err
      H.modify $ DCS._stateMode .~ Error "There was a problem decoding the saved deck"
    Right model →
      setModel dir deckId model

setModel ∷ DirPath → DeckId → Deck → DeckDSL Unit
setModel dir deckId model = do
  st ← DCS.fromModel dir deckId model <$> H.get
  setDeckState st
  runCards $ DCS.coordModelToCoord <$> st.modelCards
  updateActiveCardAndIndicator

  where
  runCards cards = do
    H.modify
      $ (DCS._cardsToLoad .~ Set.fromFoldable cards)
      ∘ (DCS._stateMode .~ Preparing)
      ∘ (fromMaybe id $ DCS.addPendingCard <$> Array.head cards)
    runPendingCards
