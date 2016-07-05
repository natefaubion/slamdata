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

module SlamData.Workspace.Component
  ( comp
  , module SlamData.Workspace.Component.State
  , module SlamData.Workspace.Component.Query
  ) where

import SlamData.Prelude

import Control.Monad.Maybe.Trans (MaybeT(..), runMaybeT)
import Control.UI.Browser (setHref, locationObject)

import Data.Array as Array
import Data.Lens ((^.), (.~), (?~))
import Data.List as List
import Data.Map as Map
import Data.Path.Pathy ((</>))
import Data.Path.Pathy as Pathy

import DOM.HTML.Location as Location

import Halogen as H
import Halogen.Component.ChildPath (injSlot, injQuery)
import Halogen.Component.Opaque.Unsafe (opaqueState)
import Halogen.HTML.Core (ClassName, className)
import Halogen.HTML.Events.Indexed as HE
import Halogen.HTML.Indexed as HH
import Halogen.HTML.Properties.Indexed as HP
import Halogen.Themes.Bootstrap3 as B

import SlamData.Effects (Slam)
import SlamData.FileSystem.Routing (parentURL)
import SlamData.Header.Component as Header
import SlamData.Quasar.Data as Quasar
import SlamData.Render.CSS as Rc
import SlamData.SignIn.Component as SignIn
import SlamData.Workspace.Action as WA
import SlamData.Workspace.AccessType as AT
import SlamData.Workspace.Card.CardId as CID
import SlamData.Workspace.Card.Draftboard.Common as DBC
import SlamData.Workspace.Card.Model as Card
import SlamData.Workspace.Component.ChildSlot (ChildQuery, ChildSlot, ChildState, cpDeck, cpHeader)
import SlamData.Workspace.Component.Query (QueryP, Query(..), fromWorkspace, fromDeck, toWorkspace, toDeck)
import SlamData.Workspace.Component.State (State, _accessType, _initialDeckId, _loaded, _path, _version, _stateMode, _globalVarMap, initialState)
import SlamData.Workspace.Deck.Common (wrappedDeck, defaultPosition)
import SlamData.Workspace.Deck.Component as Deck
import SlamData.Workspace.Deck.Component.Nested as DN
import SlamData.Workspace.Deck.DeckId (DeckId, freshDeckId)
import SlamData.Workspace.Deck.DeckLevel as DL
import SlamData.Workspace.Deck.Model as DM
import SlamData.Workspace.Model as Model
import SlamData.Workspace.Routing (mkWorkspaceHash)
import SlamData.Workspace.StateMode (StateMode(..))
import SlamData.Workspace.Wiring (Wiring, getDeck, putDeck, getCache, putCardEval)

import Utils.Path as UP

type StateP = H.ParentState State ChildState Query ChildQuery Slam ChildSlot
type WorkspaceHTML = H.ParentHTML ChildState Query ChildQuery Slam ChildSlot
type WorkspaceDSL = H.ParentDSL State ChildState Query ChildQuery Slam ChildSlot

comp ∷ Wiring → H.Component StateP QueryP Slam
comp wiring =
  H.lifecycleParentComponent
    { render: render wiring
    , eval: eval wiring
    , peek: Just (peek wiring ∘ H.runChildF)
    , initializer: Just $ Init unit
    , finalizer: Nothing
    }

render ∷ Wiring → State → WorkspaceHTML
render wiring state =
  HH.div
    [ HP.classes classes
    , HE.onClick (HE.input_ DismissAll)
    ]
    $ header ⊕ deck
  where
  header ∷ Array WorkspaceHTML
  header = do
    guard (not shouldHideTopMenu)
    pure $ HH.slot' cpHeader unit \_→
      { component: Header.comp
      , initialState: H.parentState Header.initialState
      }

  deck ∷ Array WorkspaceHTML
  deck =
    pure case state.stateMode, state.path, state.initialDeckId of
      Loading, _, _ →
        HH.div [ HP.classes [ workspaceClass ] ]
          []
      Error err, _, _→ showError err
      _, Just path, Just deckId →
        HH.div [ HP.classes [ workspaceClass ] ]
          [ HH.slot' cpDeck unit \_ →
             let
               init =
                 opaqueState $
                   (Deck.initialDeck path deckId)
                     { globalVarMap = state.globalVarMap
                     }
              in { component: DN.comp (deckOpts path deckId) init
                 , initialState: DN.initialState
                 }
          ]
      _, Nothing, _ → showError "Missing workspace path"
      _, _, Nothing → showError "Missing deck id (impossible!)"

  deckOpts path deckId =
    { path
    , level: DL.root
    , accessType: state.accessType
    , wiring
    }

  showError err =
    HH.div [ HP.classes [ B.alert, B.alertDanger ] ]
      [ HH.h1
          [ HP.class_ B.textCenter ]
          [ HH.text err ]
      ]

  shouldHideTopMenu ∷ Boolean
  shouldHideTopMenu = AT.isReadOnly (state ^. _accessType)

  shouldHideEditors ∷ Boolean
  shouldHideEditors = AT.isReadOnly (state ^. _accessType)

  classes ∷ Array ClassName
  classes =
    if shouldHideEditors
      then [ Rc.workspaceViewHack ]
      else [ Rc.dashboard ]

  workspaceClass ∷ ClassName
  workspaceClass =
    if shouldHideTopMenu
      then className "sd-workspace-hidden-top-menu"
      else className "sd-workspace"

eval ∷ Wiring → Natural Query WorkspaceDSL
eval _ (Init next) = do
  deckId ← H.fromEff freshDeckId
  H.modify (_initialDeckId ?~ deckId)
  pure next
eval _ (SetGlobalVarMap varMap next) = do
  H.modify (_globalVarMap .~ varMap)
  queryDeck $ H.action $ Deck.SetGlobalVarMap varMap
  pure next
eval _ (DismissAll next) = do
  querySignIn $ H.action SignIn.DismissSubmenu
  pure next
eval _ (Reset path next) = do
  H.modify _
    { path = Just path
    , stateMode = Ready
    , accessType = AT.Editable
    }
  queryDeck $ H.action $ Deck.Reset path
  queryDeck $ H.action $ Deck.Focus
  pure next
eval _ (Load path deckId accessType next) = do
  oldAccessType <- H.gets _.accessType
  H.modify (_accessType .~ accessType)

  queryDeck (H.request Deck.GetId) >>= \deckId' →
    case deckId, deckId' of
      Just a, Just b | a == b && oldAccessType == accessType → pure unit
      _, _ → load
  pure next

  where
  load = do
    H.modify _
      { stateMode = Loading
      , path = Just path
      }
    queryDeck $ H.action $ Deck.Reset path
    maybe loadRoot loadDeck deckId
    void $ queryDeck $ H.action $ Deck.Focus

  loadDeck deckId = void do
    H.modify _ { stateMode = Ready }
    queryDeck $ H.action $ Deck.Load path deckId DL.root

  loadRoot =
    rootDeck path >>=
      either (\err → H.modify $ _stateMode .~ Error err) loadDeck

rootDeck ∷ UP.DirPath → WorkspaceDSL (Either String DeckId)
rootDeck path = Model.getRoot (path </> Pathy.file "index")

peek ∷ ∀ a. Wiring → ChildQuery a → WorkspaceDSL Unit
peek wiring = ((const $ pure unit) ⨁ peekDeck) ⨁ (const $ pure unit)
  where
  peekDeck (Deck.DoAction (Deck.Unwrap decks) _) = void $ runMaybeT do
    state ← lift H.get
    path ← MaybeT $ pure state.path
    oldId ← MaybeT $ queryDeck $ H.request Deck.GetId
    parent ← lift $ join <$> queryDeck (H.request Deck.GetParent)
    newId × (_ × deck) ← MaybeT $ pure $ List.head $ Map.toList decks
    let deck' = deck { parent = parent }

    lift do
      queryDeck $ H.action $ Deck.SetModel newId deck' DL.root
      queryDeck $ H.action $ Deck.Save Nothing

      case parent of
        Just parentCoord@(Tuple deckId cardId) → do
          getDeck path deckId wiring.decks >>= traverse_ \parentDeck → void do
            let cards = DBC.replacePointer oldId newId cardId parentDeck.cards
            putDeck path deckId (parentDeck { cards = cards }) wiring.decks
          let deckHash = mkWorkspaceHash (Deck.deckPath' path newId) (WA.Load state.accessType) state.globalVarMap
          H.fromEff $ locationObject >>= Location.setHash deckHash
        Nothing -> do
          let index = path </> Pathy.file "index"
          void $ Model.setRoot index newId

  peekDeck (Deck.DoAction Deck.Wrap _) = void $ runMaybeT do
    path ← MaybeT $ H.gets _.path
    let index = path </> Pathy.file "index"
    parent ← lift $ join <$> queryDeck (H.request Deck.GetParent)
    oldId ← MaybeT $ queryDeck (H.request Deck.GetId)
    newId ← lift $ H.fromEff freshDeckId

    let transitionDeck newDeck = do
          traverse_ (queryDeck ∘ H.action)
            [ Deck.SetParent (Tuple newId (CID.CardId 0))
            , Deck.Save Nothing
            , Deck.Reset path
            , Deck.SetModel newId newDeck DL.root
            , Deck.Save Nothing
            ]

    lift case parent of
      Just (Tuple deckId cardId) → do
        getDeck path deckId wiring.decks >>= traverse_ \parentDeck → void do
          let cards = DBC.replacePointer oldId newId cardId parentDeck.cards
          putDeck path deckId (parentDeck { cards = cards }) wiring.decks
          for_ cards (unsafeUpdateCachedDraftboard wiring deckId)
          transitionDeck $ (wrappedDeck defaultPosition oldId) { parent = parent }
      Nothing → void do
        transitionDeck $ wrappedDeck defaultPosition oldId
        Model.setRoot index newId
  peekDeck (Deck.DoAction Deck.DeleteDeck _) = do
    st ← H.get
    for_ st.path \path → do
      res ← Quasar.delete $ Left path
      case res of
        -- TODO: do something to notify the user deleting failed
        Left err → pure unit
        Right _ → void $ H.fromEff $ setHref $ parentURL $ Left path
  peekDeck (Deck.DoAction Deck.Mirror _) = void $ runMaybeT do
    state ← lift H.get
    path ← MaybeT $ pure state.path
    newIdShared ← lift $ H.fromEff freshDeckId
    newIdMirror ← lift $ H.fromEff freshDeckId
    newIdParent ← lift $ H.fromEff freshDeckId
    oldId ← MaybeT $ queryDeck (H.request Deck.GetId)
    oldModel ← MaybeT $ queryDeck (H.request Deck.GetModel)
    let
      index = path </> Pathy.file "index"
      freshCard = CID.CardId 0
      parentRef = Just (newIdParent × freshCard)
      wrappedDeck = DM.emptyDeck
        { parent = oldModel.parent
        , cards = pure
          { cardId: freshCard
          , model: Card.Draftboard
            { decks: Map.fromFoldable
              [ oldId × defaultPosition
              , newIdMirror × defaultPosition
                  { y = defaultPosition.y + defaultPosition.height + 1.0 }
              ]
            }
          }
        }
    if Array.null oldModel.cards
      then do
        let mirrored = oldModel { parent = parentRef }
        putDeck path oldId mirrored wiring.decks
        putDeck path newIdMirror mirrored wiring.decks
      else do
        let
          mirrored ∷ DM.Deck -- Needed because of some sort of constraint-generalization bug?
          mirrored = oldModel
            { parent = parentRef
            , mirror = oldModel.mirror <> map (Tuple newIdShared ∘ _.cardId) oldModel.cards
            , cards = mempty
            , name = oldModel.name
            }
        putDeck path oldId mirrored wiring.decks
        putDeck path newIdMirror (mirrored { name = "" }) wiring.decks
        putDeck path newIdShared (oldModel { name = "" }) wiring.decks
    putDeck path newIdParent wrappedDeck wiring.decks
    lift case oldModel.parent of
      Just (Tuple deckId cardId) →
        getDeck path deckId wiring.decks >>= traverse_ \parentDeck → void do
          let cards = DBC.replacePointer oldId newIdParent cardId parentDeck.cards
          putDeck path deckId (parentDeck { cards = cards }) wiring.decks
          for_ cards (unsafeUpdateCachedDraftboard wiring deckId)
      Nothing →
        void $ Model.setRoot index newIdParent
    let deckHash = mkWorkspaceHash (Deck.deckPath' path newIdParent) (WA.Load state.accessType) state.globalVarMap
    H.fromEff $ locationObject >>= Location.setHash deckHash

  peekDeck _ = pure unit

-- | This shouldn't be done in general, but since draftboards have no inputs or
-- | outputs it's OK to just swap out the model for the cached card eval.
unsafeUpdateCachedDraftboard
  ∷ Wiring
  → DeckId
  → Card.Model
  → WorkspaceDSL Unit
unsafeUpdateCachedDraftboard wiring deckId model =
  case model of
    { cardId, model: Card.Draftboard db } → do
      let coord = deckId × cardId
      getCache coord wiring.cards >>= traverse_ \ce → do
        let card = map (_ { model = Card.Draftboard db }) ce.card
        putCardEval (ce { card = card }) wiring.cards
    _ → pure unit

queryDeck ∷ ∀ a. Deck.Query a → WorkspaceDSL (Maybe a)
queryDeck = H.query' cpDeck unit ∘ right

querySignIn ∷ ∀ a. SignIn.Query a → WorkspaceDSL Unit
querySignIn =
  void
    ∘ H.query' cpHeader unit
    ∘ right
    ∘ H.ChildF (injSlot Header.cpSignIn unit)
    ∘ injQuery Header.cpSignIn
    ∘ left
