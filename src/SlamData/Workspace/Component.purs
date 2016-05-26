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

import Data.Lens ((^.), (.~), (?~))
import Data.List as L
import Data.Map as Map
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
import SlamData.Header.Component as Header
import SlamData.Render.CSS as Rc
import SlamData.SignIn.Component as SignIn
import SlamData.Workspace.AccessType as AT
import SlamData.Workspace.Card.CardId as CID
import SlamData.Workspace.Card.CardType as CT
import SlamData.Workspace.Card.Draftboard.Component.State as DBS
import SlamData.Workspace.Component.ChildSlot (ChildQuery, ChildSlot, ChildState, cpDeck, cpHeader)
import SlamData.Workspace.Component.Query (QueryP, Query(..), fromWorkspace, fromDeck, toWorkspace, toDeck)
import SlamData.Workspace.Component.State (State, _accessType, _loaded, _parentHref, _path, _version, _stateMode,  initialState)
import SlamData.Workspace.Deck.Component as Deck
import SlamData.Workspace.Deck.DeckId (DeckId(..))
import SlamData.Workspace.Deck.Model as DM
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
eval (SetParentHref href next) = H.modify (_parentHref ?~ href) $> next
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
eval (Load path deckIds next) = do
  H.modify _
    { stateMode = Loading
    , path = Just path
    , root = Nothing
    }
  queryDeck $ H.action $ Deck.Reset (Just path)
  case L.head deckIds of
    Just deckId →
      loadDeck deckId
    Nothing →
      rootDeck path >>=
        either (\err → H.modify $ _stateMode .~ Error err) loadDeck
  pure next

  where
  loadDeck deckId = void do
    H.modify _
      { root = Just deckId
      , stateMode = Ready
      }
    queryDeck $ H.action $ Deck.Load path deckId

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
      queryDeck (H.action Deck.Save)
      queryDeck (H.request Deck.GetId) >>= join >>> traverse_ \oldId → do
        Model.freshId index >>= traverse_ \newId → do
          queryDeck $ H.action $ Deck.Reset (Just path)
          queryDeck $ H.action $ Deck.SetModel (DeckId newId) (wrappedDeck st.path oldId)
          queryDeck $ H.action $ Deck.Save
          Model.setRoot newId index
  peekDeck _ = pure unit

  wrappedDeck ∷ Maybe UP.DirPath → DeckId → DM.Deck
  wrappedDeck path deckId =
    DM.emptyDeck
      { cards =
          [ { cardId: CID.CardId 0
            , cardType: CT.Draftboard
            , state: DBS.encode $ DBS.initialState
                { decks = Map.singleton deckId
                    { x: 1.0
                    , y: 1.0
                    , width: 20.0
                    , height: 10.0
                    }
                , path = path
                }
            , hasRun: false
            }
          ]
      }

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
