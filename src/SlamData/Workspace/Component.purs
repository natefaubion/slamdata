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
import Data.Path.Pathy ((</>))
import Data.Path.Pathy as Pathy

import Halogen as H
import Halogen.Component.ChildPath (injSlot, injQuery)
import Halogen.Component.Opaque.Unsafe (opaqueQuery)
import Halogen.HTML.Core (ClassName, className)
import Halogen.HTML.Events.Indexed as HE
import Halogen.HTML.Indexed as HH
import Halogen.HTML.Properties.Indexed as HP

import SlamData.Effects (Slam)
import SlamData.Header.Component as Header
import SlamData.Render.CSS as Rc
import SlamData.SignIn.Component as SignIn
import SlamData.Workspace.AccessType as AT
import SlamData.Workspace.Component.ChildSlot (ChildQuery, ChildSlot, ChildState, cpDeck, cpHeader)
import SlamData.Workspace.Component.Query (QueryP, Query(..), fromWorkspace, fromDeck, toWorkspace, toDeck)
import SlamData.Workspace.Component.State (State, _accessType, _browserFeatures,  _loaded, _parentHref, _path, _version, initialState)
import SlamData.Workspace.Deck.Component as Deck
import SlamData.Workspace.Deck.DeckId (DeckId(..))
import SlamData.Workspace.Model as Model

import Utils.Path as UP

type StateP = H.ParentState State ChildState Query ChildQuery Slam ChildSlot
type WorkspaceHTML = H.ParentHTML ChildState Query ChildQuery Slam ChildSlot
type WorkspaceDSL = H.ParentDSL State ChildState Query ChildQuery Slam ChildSlot

comp ∷ H.Component StateP QueryP Slam
comp =
  H.parentComponent
    { render
    , eval
    , peek: Nothing
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
  deck =
    [ HH.div [ HP.classes [ workspaceClass ] ]
        [ HH.slot' cpDeck unit \_ →
           { component: Deck.comp
           , initialState: Deck.initialState (state.browserFeatures)
           }
        ]
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
eval (Reset features path next) = do
  H.modify (_path .~ Just path)
  queryDeck $ H.action $ Deck.Reset features path Nothing
  pure next
eval (Load features path deckIds next) = do
  H.modify (_path .~ Just path)
  let load = void ∘ queryDeck ∘ H.action ∘ Deck.Load features path
  case L.head deckIds of
    Just deckId → load deckId
    Nothing → rootDeck path >>= either (const (pure unit)) load
    -- Show load error somehow
  pure next

rootDeck ∷ UP.DirPath → WorkspaceDSL (Either String DeckId)
rootDeck path = map (map DeckId) $ Model.getRoot (path </> Pathy.file "index")

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
