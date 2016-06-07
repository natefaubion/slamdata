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

import Control.UI.Browser (setHref)

import Data.Lens ((^.), (.~))
import Data.Path.Pathy ((</>))
import Data.Path.Pathy as Pathy

import Halogen as H
import Halogen.Component.ChildPath (injSlot, injQuery)
import Halogen.Component.Opaque.Unsafe (opaqueQuery, peekOpaqueQuery)
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
import SlamData.Workspace.AccessType as AT
import SlamData.Workspace.Card.CardId as CID
import SlamData.Workspace.Component.ChildSlot (ChildQuery, ChildSlot, ChildState, cpDeck, cpHeader)
import SlamData.Workspace.Component.Query (QueryP, Query(..), fromWorkspace, fromDeck, toWorkspace, toDeck)
import SlamData.Workspace.Component.State (State, _accessType, _loaded, _path, _version, _stateMode,  initialState)
import SlamData.Workspace.Deck.Common (wrappedDeck, defaultPosition)
import SlamData.Workspace.Deck.Component as Deck
import SlamData.Workspace.Deck.DeckId (DeckId(..))
import SlamData.Workspace.Model as Model
import SlamData.Workspace.StateMode (StateMode(..))

import Utils.Path as UP

type StateP = H.ParentState State ChildState Query ChildQuery Slam ChildSlot
type WorkspaceHTML = H.ParentHTML ChildState Query ChildQuery Slam ChildSlot
type WorkspaceDSL = H.ParentDSL State ChildState Query ChildQuery Slam ChildSlot

comp ∷ H.Component StateP QueryP Slam
comp =
  H.parentComponent
    { render
    , eval
    , peek: Just (peek ∘ H.runChildF)
    }

render ∷ State → WorkspaceHTML
render state =
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
  deck = pure case state.stateMode of
    Loading →
      HH.div [ HP.classes [ workspaceClass ] ]
        []

    Ready →
      HH.div [ HP.classes [ workspaceClass ] ]
        [ HH.slot' cpDeck unit \_ →
           { component: Deck.comp
           , initialState: Deck.initialState
           }
        ]

    Error err →
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

eval ∷ Natural Query WorkspaceDSL
eval (SetAccessType aType next) = do
  H.modify (_accessType .~ aType)
  queryDeck $ H.action $ Deck.SetAccessType aType
  pure next
eval (DismissAll next) = do
  querySignIn $ H.action SignIn.DismissSubmenu
  pure next
eval (GetPath k) = k <$> H.gets _.path
eval (Reset path next) = do
  H.modify _
    { path = path
    , stateMode = Ready
    }
  queryDeck $ H.action $ Deck.Reset path
  pure next
eval (Load path deckId next) = do
  queryDeck (H.request Deck.GetId) >>= join >>> \deckId' →
    case deckId, deckId' of
      Just a, Just b | a == b → pure unit
      _, _ → load
  pure next

  where
  load = do
    H.modify _
      { stateMode = Loading
      , path = Just path
      }
    queryDeck $ H.action $ Deck.Reset (Just path)
    maybe loadRoot loadDeck deckId

  loadDeck deckId = void do
    H.modify _ { stateMode = Ready }
    queryDeck $ H.action $ Deck.Load path deckId Deck.Root

  loadRoot =
    rootDeck path >>=
      either (\err → H.modify $ _stateMode .~ Error err) loadDeck

rootDeck ∷ UP.DirPath → WorkspaceDSL (Either String DeckId)
rootDeck path = map (map DeckId) $ Model.getRoot (path </> Pathy.file "index")

peek ∷ ∀ a. ChildQuery a → WorkspaceDSL Unit
peek = (peekOpaqueQuery peekDeck) ⨁ (const $ pure unit)
  where
  peekDeck (Deck.DoAction Deck.Mirror _) = pure unit
  peekDeck (Deck.DoAction Deck.Wrap _) = do
    st ← H.get
    for_ st.path \path → do
      let index = path </> Pathy.file "index"
      queryDeck (H.request Deck.GetId) >>= join >>> traverse_ \oldId → do
        Model.freshId index >>= traverse_ \newId → do
          let newDeck = wrappedDeck defaultPosition oldId
              newId' = DeckId newId
          traverse_ (queryDeck ∘ H.action)
            [ Deck.SetParent (Tuple newId' (CID.CardId 0))
            , Deck.Save
            , Deck.Reset (Just path)
            , Deck.SetModel newId' newDeck Deck.Root
            , Deck.Save
            ]
          Model.setRoot newId index
  peekDeck (Deck.DoAction Deck.DeleteDeck _) = do
    st ← H.get
    for_ st.path \path → do
      res ← Quasar.delete $ Left path
      case res of
        -- TODO: do something to notify the user deleting failed
        Left err → pure unit
        Right _ → void $ H.fromEff $ setHref $ parentURL $ Left path
  peekDeck _ = pure unit

queryDeck ∷ ∀ a. Deck.Query a → WorkspaceDSL (Maybe a)
queryDeck = H.query' cpDeck unit ∘ opaqueQuery

querySignIn ∷ ∀ a. SignIn.Query a → WorkspaceDSL Unit
querySignIn =
  void
    ∘ H.query' cpHeader unit
    ∘ right
    ∘ H.ChildF (injSlot Header.cpSignIn unit)
    ∘ injQuery Header.cpSignIn
    ∘ left
