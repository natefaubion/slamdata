{-
Copyright 2017 SlamData, Inc.

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

module SlamData.Workspace.MillerColumns.Column.Component
  ( component
  , component'
  , module SlamData.Workspace.MillerColumns.Column.Component.ColumnWidth
  , module SlamData.Workspace.MillerColumns.Column.Component.Query
  , module SlamData.Workspace.MillerColumns.Column.Component.State
  , module SlamData.Workspace.MillerColumns.Column.Options
  ) where

import SlamData.Prelude

import Control.Monad.Fork.Class (fork)
import CSS as CSS
import Data.Array as A
import Data.List as L
import Data.Time.Duration (Milliseconds(..))
import DOM.Classy.Element (scrollTop, scrollHeight, clientHeight) as DOM
import Halogen as H
import Halogen.Component.Proxy (proxyQI)
import Halogen.Component.Utils.Debounced (debouncedEventSource, runDebounceTrigger, cancelDebounceTrigger)
import Halogen.HTML as HH
import Halogen.HTML.CSS as HCSS
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.HTML.Properties.ARIA as ARIA
import SlamData.Monad (Slam)
import SlamData.Render.Common as RC
import SlamData.Render.Icon as I
import SlamData.Workspace.MillerColumns.Column.Component.ColumnWidth (ColumnWidth(..), defaultColumnWidth, minColumnWidth)
import SlamData.Workspace.MillerColumns.Column.Component.Item as Item
import SlamData.Workspace.MillerColumns.Column.Component.Query (Message(..), Message', Query(..), Query')
import SlamData.Workspace.MillerColumns.Column.Component.Request as Req
import SlamData.Workspace.MillerColumns.Column.Component.State (ColumnState(..), State, initialState)
import SlamData.Workspace.MillerColumns.Column.Options (ColumnOptions(..))

type HTML a i o = H.ParentHTML (Query a i o) (Item.Query a o) i Slam
type DSL a i o = H.ParentDSL (State a i o) (Query a i o) (Item.Query a o) i (Message' a i o) Slam

component
  ∷ ∀ a i o
  . Eq a
  ⇒ Ord i
  ⇒ ColumnOptions a i o
  → i
  → H.Component HH.HTML (Query' a i o) (ColumnWidth × Maybe a) (Message' a i o) Slam
component opts = proxyQI ∘ component' opts

component'
  ∷ ∀ a i o
  . Eq a
  ⇒ Ord i
  ⇒ ColumnOptions a i o
  → i
  → H.Component HH.HTML (Query a i o) (ColumnWidth × Maybe a) (Message' a i o) Slam
component' (ColumnOptions colSpec) colPath =
  H.lifecycleParentComponent
    { initialState
    , render
    , eval
    , initializer: Just (H.action Init)
    , finalizer: Nothing
    , receiver: HE.input HandleInput
    }
  where

  render ∷ State a i o → HTML a i o
  render { items, state, selected, filterText, width } =
    HH.div
      [ HP.class_ (HH.ClassName "sd-miller-column-container")
      , HCSS.style (CSS.width (CSS.px (unwrap width)))
      ]
      [ HH.div
          [ HP.class_ (HH.ClassName "sd-miller-column-filter") ]
          [ HH.div
              [ HP.class_ (HH.ClassName "sd-action-filter-icon") ]
              [ I.searchSm ]
          , HH.input
              [ HP.class_ (HH.ClassName "sd-form-input")
              , HP.value filterText
              , HE.onValueInput (HE.input HandleFilterChange)
              ]
          , HH.button
              [ HP.class_ (HH.ClassName "sd-action-filter-clear")
              , HP.type_ HP.ButtonButton
              , HE.onClick (HE.input_ (UpdateFilter ""))
              , HP.enabled (filterText /= "")
              ]
              [ RC.clearFieldIcon "Clear filter" ]
          ]
      , renderSelected selected
      , HH.div
          [ HP.class_ (HH.ClassName "sd-miller-column-items")
          , HP.ref refItems
          , HE.onScroll (HE.input_ HandleScroll)
          ]
          [ HH.ul
              [ HP.class_ (HH.ClassName "sd-miller-column-items-list") ]
              $ A.fromFoldable (renderItem selected <$> items)
                  <> (guard (state == Loading) $> loadIndicator)
          ]
      ]

  refItems ∷ H.RefLabel
  refItems = H.RefLabel "refItems"

  loadIndicator ∷ HTML a i o
  loadIndicator =
    HH.li
      [ HP.class_ $ HH.ClassName "sd-miller-column-loading" ]
      [ HH.div
          [ HP.class_ $ HH.ClassName "sd-miller-column-spinner" ]
          [RC.spinnerSmall]
      , HH.div
          []
          [HH.span_ [ HH.text "Loading…" ]]
      ]


  renderSelected ∷ Maybe a → HTML a i o
  renderSelected = case _ of
    Nothing →
      HH.div
        [ HP.class_ (HH.ClassName "sd-miller-column-selection") ]
        [ HH.span_ [ HH.text "No selection" ] ]
    Just x →
      let
        label = colSpec.label x
        deselLabel = "Deselect '" <> label <> "'"
      in
        HH.div
          [ HP.classes
              [ HH.ClassName "sd-miller-column-selection"
              , HH.ClassName "selected"
              ]
          , HP.title deselLabel
          , ARIA.label deselLabel
          , HE.onClick $ HE.input_ Deselect
          ]
          [ HH.span
              [ HP.class_ (HH.ClassName "sd-miller-column-selection-label") ]
              [ HH.text label ]
          , RC.clearFieldIcon deselLabel
          ]

  renderItem ∷ Maybe a → a → HTML a i o
  renderItem selected item =
    let
      itemId = colSpec.id item
      selectedId = colSpec.id <$> selected
    in
      HH.slot
        itemId
        (colSpec.renderItem itemId item)
        (if Just itemId == selectedId then Item.Selected else Item.Deselected)
        (HE.input (HandleMessage itemId))

  eval ∷ Query a i o ~> DSL a i o
  eval = case _ of
    Init next → do
      trigger ← debouncedEventSource (Milliseconds 750.0)
      H.modify (_ { filterTrigger = trigger })
      _ ← fork load
      H.raise (Left Initialized)
      pure next
    SetSelection selected next → do
      H.modify (_ { selected = selected })
      pure next
    GetSelection k →
      k <$> H.gets _.selected
    Deselect next → do
      H.raise (Left Deselected)
      pure next
    HandleFilterChange text next → do
      trigger ← H.gets _.filterTrigger
      H.modify (\st → st { lastRequestId = Req.succ st.lastRequestId, filterText = text })
      lift $ runDebounceTrigger trigger (UpdateFilter text)
      pure next
    UpdateFilter text next → do
      trigger ← H.gets _.filterTrigger
      lift $ cancelDebounceTrigger trigger
      H.modify (_ { filterText = text, items = L.Nil, lastLoadRequest = Nothing })
      load
      pure next
    HandleScroll next → do
      fill
      pure next
    HandleMessage itemId msg next → do
      case msg of
        Left (Item.RaisePopulate a) → do
          H.modify (_ { selected = Just a })
          H.raise $ Left $ Selected itemId a
        Right o → do
          H.raise $ Right o
      pure next
    HandleInput (newWidth × newSelected) next → do
      { width, selected } ← H.get
      when (width /= newWidth || selected /= newSelected) $
        H.modify (_ { width = newWidth, selected = newSelected })
      pure next
    FulfilLoadRequest { requestId, items, nextOffset } next → do
      expectedId ← H.gets _.lastRequestId
      when (requestId == expectedId) do
        H.modify \st' → st'
          { items = st'.items <> items
          , nextOffset = nextOffset
          , state = Loaded
          }
        fill
      pure next

  fill ∷ DSL a i o Unit
  fill =
    H.getHTMLElementRef refItems >>= traverse_ \items → do
      remain ← H.liftEff do
        height <- DOM.clientHeight items
        scrollTop <- DOM.scrollTop items
        scrollHeight <- DOM.scrollHeight items
        pure (scrollHeight - height - scrollTop)
      when (remain < 20.0) load

  load ∷ DSL a i o Unit
  load = do
    { filterText, nextOffset, lastLoadRequest, lastRequestId } ← H.get
    let
      shouldLoad = case lastLoadRequest of
        Nothing → true
        Just { filter, offset } →
          nextOffset /= Nothing && (filter /= filterText || offset /= nextOffset)
    when shouldLoad do
      let
        requestId = Req.succ lastRequestId
        params = { requestId, filter: filterText, offset: nextOffset }
      H.modify (_ { state = Loading, lastLoadRequest = Just params, lastRequestId = requestId })
      H.raise $ Left $ LoadRequest params
