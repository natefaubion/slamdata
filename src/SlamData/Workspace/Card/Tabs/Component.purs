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

module SlamData.Workspace.Card.Tabs.Component where

import SlamData.Prelude

import Data.Array as Array
import Data.Int (toNumber)
import Data.Ord (abs)

import Halogen as H
import Halogen.HTML.CSS as HC
import Halogen.HTML.Events as HE
import Halogen.HTML as HH
import Halogen.HTML.Elements.Keyed as HK
import Halogen.HTML.Properties as HP
import Halogen.Component.Utils (busEventSource)
import Halogen.Component.Utils.Drag as Drag

import CSS as C

import SlamData.Workspace.AccessType as AT
import SlamData.Workspace.Card.CardType as CT
import SlamData.Workspace.Card.Common (CardOptions)
import SlamData.Workspace.Card.Component as CC
import SlamData.Workspace.Card.Eval.State as ES
import SlamData.Workspace.Card.Model as Card
import SlamData.Workspace.Card.Tabs.Component.State (State, initialState, modelFromState, updateName, activateTab, reorder)
import SlamData.Workspace.Card.Tabs.Component.Query (Query(..))
import SlamData.Workspace.Deck.Component.Query as DCQ
import SlamData.Workspace.Deck.DeckId as DID
import SlamData.Workspace.Eval.Deck as ED
import SlamData.Workspace.Eval.Persistence as P
import SlamData.Workspace.LevelOfDetails (LevelOfDetails(..))

type TabsDSL = CC.InnerCardParentDSL State Query DCQ.Query DID.DeckId

type TabsHTML = CC.InnerCardParentHTML Query DCQ.Query DID.DeckId

tabsComponent ∷ CardOptions → CC.CardComponent
tabsComponent options =
  CC.makeCardComponent CT.tabs component options
  where
  component = H.parentComponent
    { render: render options
    , eval: coproduct evalCard (evalTabs options)
    , initialState: const initialState
    , receiver: const Nothing
    }

render ∷ CardOptions → State → TabsHTML
render cardOpts st =
  HH.div
    [ HP.classes [ HH.ClassName "sd-tab-container" ] ]
    [ HH.div
        [ HP.classes [ HH.ClassName "sd-tab-header" ] ]
        [ HH.ul
            [ HP.classes [ HH.ClassName "sd-tab-set" ] ]
            (Array.mapWithIndex renderTab st.tabs)
        , HH.button
            [ HP.classes [ HH.ClassName "sd-tab-add-btn" ]
            , HE.onClick (HE.input_ (right ∘ AddTab))
            ]
            [ HH.text "+" ]
        ]
    , HK.div
        [ HP.classes [ HH.ClassName "sd-tab-body" ] ]
        (Array.mapWithIndex renderDeck st.tabs)
    ]
  where
    renderTab ix tab =
      HH.li
        [ HP.classes $
            (guard (st.activeTab ≡ Just ix) $> HH.ClassName "active")
            <> tabClasses ix
        , HC.style do
            C.width (C.pct $ 1.0 / toNumber (Array.length st.tabs) * 100.0)
        ]
        [ HH.button (tabAttrs ix)
            [ HH.text
                if tab.name ≡ ""
                  then "Untitled Deck"
                  else tab.name
            ]
        ]

    tabAttrs ix =
      [ HC.style (tabStyles ix)
      , HE.onMouseDown (HE.input (\ev → right ∘ OrderStart ix ev))
      ] <>
      if cardOpts.deck.accessType ≡ AT.ReadOnly
        then []
        else
          [ HE.onMouseEnter (HE.input_ (right ∘ OrderOver ix))
          , HE.onMouseLeave (HE.input_ (right ∘ OrderOut ix))
          ]

    tabClasses ix =
      [ HH.ClassName "sd-tab-btn" ] <>
        case st.ordering of
          Just opts | isOrdering ix opts → [ HH.ClassName "ordering" ]
          Just opts | opts.over == Just ix → [ HH.ClassName "ordering-over" ]
          _ → []

    tabStyles ix = do
      case st.ordering of
        Just opts | isOrdering ix opts → C.left (C.px opts.offset)
        _ → pure unit

    isOrdering ix opts =
      opts.source ≡ ix && abs opts.offset > 10.0

    renderDeck ix tab =
      DID.toString tab.deckId × HH.div
        [ HP.classes $
            (guard (st.activeTab ≡ Just ix) $> HH.ClassName "active")
            <> [ HH.ClassName "sd-tab-slot" ]
        ]
        if tab.loaded
          then [ HH.slot tab.deckId (mkDeckComponent tab.deckId) unit (const Nothing) ]
          else [ HH.text "" ]

    mkDeckComponent deckId =
      cardOpts.deckComponent
        { accessType: cardOpts.deck.accessType
        , cursor: cardOpts.cursor
        , displayCursor: cardOpts.displayCursor
        , deckId
        }

evalCard ∷ CC.CardEvalQuery ~> TabsDSL
evalCard = case _ of
  CC.Activate next →
    pure next
  CC.Deactivate next →
    pure next
  CC.Save k →
    map (k ∘ Card.Tabs ∘ modelFromState) H.get
  CC.Load (Card.Tabs model) next → do
    st ← H.get
    let
      newIds = Array.difference model.tabs (_.deckId <$> st.tabs)
      tabLen = Array.length model.tabs
      activeTab = clampActiveTab model.tabs =<< st.activeTab
    tabs ← Array.catMaybes <$> for model.tabs \deckId → runMaybeT do
      cell ← MaybeT $ H.lift $ P.getDeck deckId
      H.lift $ H.subscribe $ busEventSource (\msg → right (H.request (HandleMessage deckId msg))) cell.bus
      pure { deckId, name: cell.model.name, loaded: false }
    H.modify
      $ activateTab activeTab
      ∘ _ { tabs = tabs }
    pure next
  CC.Load _ next →
    pure next
  CC.ReceiveInput _ _ next →
    pure next
  CC.ReceiveOutput _ _ next →
    pure next
  CC.ReceiveState (ES.ActiveTab ix) next → do
    st ← H.get
    when (st.activeTab ≠ Just ix) do
      H.modify $ activateTab (clampActiveTab st.tabs ix)
    pure next
  CC.ReceiveState _ next →
    pure next
  CC.ReceiveDimensions _ reply → do
    _ ← H.queryAll (H.action DCQ.UpdateCardSize)
    pure (reply High)

evalTabs ∷ CardOptions → Query ~> TabsDSL
evalTabs cardOpts = case _ of
  AddTab next → do
    ix ← H.gets (Array.length ∘ _.tabs)
    _ ← H.lift $ P.addDeckToTabs cardOpts.cardId
    H.raise $ CC.stateUpdate $ ES.ActiveTab ix
    pure next
  HandleMessage deckId msg reply → do
    st ← H.get
    if Array.any (eq deckId ∘ _.deckId) st.tabs
      then do
        case msg of
          ED.NameChange name → H.modify (updateName deckId name)
          _ → pure unit
        pure (reply H.Listening)
      else
        pure (reply H.Done)
  OrderStart ix ev next → do
    st ← H.get
    let
      updateTabs =
        activateTab (clampActiveTab st.tabs ix)
      opts =
        { source: ix
        , offset: 0.0
        , over: Nothing
        }
    if cardOpts.deck.accessType ≡ AT.ReadOnly
      then H.modify updateTabs
      else do
        H.subscribe $ Drag.dragEventSource ev \drag → Just (right (Ordering ix drag H.Listening))
        H.modify $ updateTabs ∘ _ { ordering = Just opts }
    H.raise $ CC.stateUpdate $ ES.ActiveTab ix
    pure next
  Ordering ix ev next → do
    st ← H.get
    for_ st.ordering \opts →
      case ev of
        Drag.Move _ d →
          H.modify _ { ordering = Just opts { offset = d.offsetX } }
        Drag.Done _ →
          case opts.over of
            Just ix' → do
              H.modify _
                { ordering = Nothing
                , activeTab = Just ix'
                , tabs = reorder ix ix' st.tabs
                }
              H.raise CC.modelUpdateSilently
              H.raise $ CC.stateUpdate $ ES.ActiveTab ix'
            Nothing →
              H.modify _ { ordering = Nothing }
    pure next
  OrderOver ix next → do
    st ← H.get
    for_ st.ordering \opts →
      when (opts.source ≠ ix) do
        H.modify _ { ordering = Just (opts { over = Just ix }) }
    pure next
  OrderOut ix next → do
    st ← H.get
    for_ st.ordering \opts →
      when (opts.source ≠ ix) do
        H.modify _ { ordering = Just (opts { over = Nothing }) }
    pure next

clampActiveTab ∷ ∀ a. Array a → Int → Maybe Int
clampActiveTab as ix = case Array.length as of
  0 → Nothing
  n | ix < n → Just ix
  n → Just (n - 1)
