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

module SlamData.FileSystem.Listing.Item.Component where

import SlamData.Prelude

import Data.Lens (Lens', lens, (%~), (.~))

import Halogen as H
import Halogen.HTML.Events as HE
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Halogen.HTML.Properties.ARIA as ARIA

import DOM.Event.Types (MouseEvent, mouseEventToEvent)
import DOM.Event.Event (preventDefault)

import SlamData.Monad (Slam)
import SlamData.FileSystem.Listing.Item (Item(..), itemResource)
import SlamData.FileSystem.Listing.Item.Component.CSS as CSS
import SlamData.FileSystem.Resource (Resource(..), Mount(..), resourceName, resourcePath, isMount, isFile, isWorkspace, isViewMount, hiddenTopLevel)
import SlamData.Render.Common as RC
import SlamData.Render.Icon as I

type State =
  { item ∷ Item
  , isSearching ∷ Boolean
  , isHidden ∷ Boolean
  }

_item ∷ Lens' State Item
_item = lens _.item _{item = _}

_isSearching ∷ Lens' State Boolean
_isSearching = lens _.isSearching _{isSearching = _}

_isHidden ∷ Lens' State Boolean
_isHidden = lens _.isHidden _{isHidden = _}

data Query a
  = Select a
  | PresentActions a
  | HideActions a
  | Deselect a
  | HandleAction Message MouseEvent a
  | SetIsSearching Boolean a
  | SetIsHidden Boolean a

data Message
  = Open Resource
  | Configure Resource
  | Move Resource
  | Edit Resource
  | Download Resource
  | Remove Resource
  | Share Resource
  | Selected

type HTML = H.ComponentHTML Query
type DSL = H.ComponentDSL State Query Message Slam

component ∷ State → H.Component HH.HTML Query Unit Message Slam
component initialState =
  H.component
    { initialState: const initialState
    , render
    , eval
    , receiver: const Nothing
    }

render ∷ State → HTML
render state = case state.item of
  SelectedItem _ → itemView state true true
  ActionsPresentedItem _ → itemView state false true
  Item _ → itemView state false false
  PhantomItem _ →
    HH.div
      [ HP.classes [ CSS.fileSystemItem, CSS.phantom ] ]
      [ HH.div
          [ HP.classes [ CSS.itemContent ] ]
          [ HH.span
              [ HP.class_ CSS.phantomSpinner ]
              [ RC.spinnerSmall ]
          , HH.span
              [ HP.class_ CSS.phantomLabel ]
              [ HH.text $ itemName state ]
          ]
      ]

eval ∷ Query ~> DSL
eval (Select next) = do
  H.modify (_item %~ toggle)
  H.raise Selected
  pure next
  where
  toggle (Item r) = SelectedItem r
  toggle (ActionsPresentedItem r) = SelectedItem r
  toggle it = it
eval (Deselect next) = H.modify (_item %~ deselect) $> next
  where
  deselect (SelectedItem r) = Item r
  deselect it = it
eval (PresentActions next) = H.modify (_item %~ presentActions) $> next
  where
  presentActions (Item r) = ActionsPresentedItem r
  presentActions it = it
eval (HideActions next) = H.modify (_item %~ hideActions) $> next
  where
  hideActions (ActionsPresentedItem r) = Item r
  hideActions it = it
eval (HandleAction msg ev next) = do
  H.liftEff $ preventDefault (mouseEventToEvent ev)
  H.raise msg
  pure next
eval (SetIsSearching bool next) = H.modify (_isSearching .~ bool) $> next
eval (SetIsHidden bool next) = H.modify (_isHidden .~ bool) $> next

itemName ∷ State → String
itemName { isSearching, item } =
  let toName = if isSearching then resourcePath else resourceName
  in toName $ itemResource item

itemIsHidden ∷ Item → Boolean
itemIsHidden = hiddenTopLevel <<< itemResource

presentHiddenItem ∷ State → Boolean
presentHiddenItem = not <<< _.isHidden

presentItem ∷ State → Item → Boolean
presentItem state item = (isHidden && presentHiddenItem state) || not isHidden
  where
  isHidden = itemIsHidden item

itemView ∷ State → Boolean → Boolean → HTML
itemView state@{ item } selected presentActions | not (presentItem state item) =
  HH.text ""
itemView state@{ item } selected presentActions | otherwise =
  HH.div
    [ HP.classes itemClasses
    , HE.onClick (HE.input_ Select)
    , HE.onMouseEnter (HE.input_ PresentActions)
    , HE.onMouseLeave (HE.input_ HideActions)
    , HE.onDoubleClick $ HE.input $ HandleAction (Open (itemResource item))
    , ARIA.label label
    ]
    [ HH.div
        [ HP.classes [ CSS.itemContent ] ]
        [ HH.a
            [ HE.onClick $ HE.input $ HandleAction (Open (itemResource item)) ]
            [ HH.span [ HP.class_ $ CSS.itemIcon ]
              [ icon $ itemResource item ]
            , HH.text $ itemName state
            ]
        ]
      , HH.ul
        [ HP.classes $ [ CSS.itemToolbar ] ⊕ (guard selected $> CSS.selected) ]
        $ itemActions presentActions item
    ]
  where
  itemClasses ∷ Array HH.ClassName
  itemClasses =
    [ CSS.fileSystemItem ]
    <> (guard selected $> HH.ClassName "list-group-item-info")
    <> (if itemIsHidden item && presentHiddenItem state then [ CSS.itemHidden ] else [ ])

  label ∷ String
  label | selected  = "Deselect " <> itemName state
  label | otherwise = "Select " <> itemName state

  icon ∷ ∀ p i. Resource → HH.HTML p i
  icon = case _ of
    File _ → I.file
    Workspace _ → I.workspaceSm
    Directory _ → I.folderSm
    Mount (Database _) → I.databaseSm
    Mount (View _) → I.documentSm
    -- TODO: Different icon
    Mount (Module _) → I.databaseSm

itemActions ∷ Boolean → Item → Array HTML
itemActions presentActions item | not presentActions = pure $ HH.text ""
itemActions presentActions item | otherwise =
  edit <> conf <> common <> share
  where
  r ∷ Resource
  r = itemResource item

  conf ∷ Array HTML
  conf = guard (isMount r) $>
    itemAction Configure "Configure" I.cog

  edit ∷ Array HTML
  edit = guard (isWorkspace r) $>
    itemAction Edit "Edit" I.editSm

  common ∷ Array HTML
  common =
    [ itemAction Move "Move / rename" I.exchangeSm
    , itemAction Download "Download" I.downloadSm
    , itemAction Remove "Remove" I.trashCanSm
    ]

  share ∷ Array HTML
  share = guard (isFile r || isWorkspace r || isViewMount r) $>
    itemAction Share "Share" I.shareSm

  itemAction ∷ (Resource → Message) → String → HTML → HTML
  itemAction act label icon =
    HH.li_
      [ HH.button
          [ HE.onClick $ HE.input $ HandleAction $ act (itemResource item)
          , HP.title label
          , ARIA.label label
          , HP.classes [ CSS.fileAction, HH.ClassName "sd-tooltip" ]
          ]
          [ icon ]
      ]
