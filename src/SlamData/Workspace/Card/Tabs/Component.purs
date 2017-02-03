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

import Control.Monad.Aff.EventLoop as EventLoop

import Data.Array as Array
import Data.Int (toNumber)
import Data.Ord (abs)

import Halogen as H
import Halogen.HTML.CSS.Indexed as HC
import Halogen.HTML.Events.Indexed as HE
import Halogen.HTML.Indexed as HH
import Halogen.HTML.Properties.Indexed as HP
import Halogen.Component.Utils (liftH', subscribeToBus')
import Halogen.Component.Utils.Drag as Drag

import CSS as C

import SlamData.Monad (Slam)
import SlamData.Workspace.AccessType as AT
import SlamData.Workspace.Card.CardType as CT
import SlamData.Workspace.Card.Common (CardOptions)
import SlamData.Workspace.Card.Component as CC
import SlamData.Workspace.Card.Eval.State as ES
import SlamData.Workspace.Card.Model as Card
import SlamData.Workspace.Card.Port as Port
import SlamData.Workspace.Card.Tabs.Component.State (State, initialState, modelFromState, updateName, activateTab, reorder)
import SlamData.Workspace.Card.Tabs.Component.Query (Query(..), QueryC)
import SlamData.Workspace.Deck.Component.Query as DCQ
import SlamData.Workspace.Deck.Component.Nested.Query as DNQ
import SlamData.Workspace.Deck.Component.Nested.State as DNS
import SlamData.Workspace.Deck.DeckId as DID
import SlamData.Workspace.Eval.Deck as ED
import SlamData.Workspace.Eval.Persistence as P

type TabsDSL = H.ParentDSL State DNS.State QueryC DNQ.QueryP Slam DID.DeckId

type TabsHTML = H.ParentHTML DNS.State QueryC DNQ.QueryP Slam DID.DeckId

tabsComponent ∷ CardOptions → CC.CardComponent
tabsComponent options = CC.makeCardComponent
  { options
  , cardType: CT.Tabs
  , component: H.parentComponent
      { render: render options
      , eval: coproduct evalCard (evalTabs options)
      , peek: Just (peek options)
      }
  , initialState: H.parentState initialState
  , _State: CC._TabsState
  , _Query: CC.makeQueryPrism' CC._TabsQuery
  }

render ∷ CardOptions → State → TabsHTML
render cardOpts st =
  HH.div
    [ HP.classes [ HH.className "sd-tab-container" ] ]
    [ HH.div
        [ HP.classes [ HH.className "sd-tab-header" ] ]
        [ HH.ul
            [ HP.classes [ HH.className "sd-tab-set" ] ]
            (Array.mapWithIndex renderTab st.tabs)
        , HH.button
            [ HP.classes [ HH.className "sd-tab-add-btn" ]
            , HE.onClick (HE.input_ (right ∘ AddTab))
            ]
            [ HH.text "+" ]
        ]
    , HH.div
        [ HP.classes [ HH.className "sd-tab-body" ] ]
        (Array.mapWithIndex renderDeck st.tabs)
    ]
  where
    renderTab ix tab =
      HH.li
        [ HP.classes $
            (guard (st.activeTab ≡ Just ix) $> HH.className "active")
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
      [ HH.className "sd-tab-btn" ] <>
        case st.ordering of
          Just opts | isOrdering ix opts → [ HH.className "ordering" ]
          Just opts | opts.over == Just ix → [ HH.className "ordering-over" ]
          _ → []

    tabStyles ix = do
      case st.ordering of
        Just opts | isOrdering ix opts → C.left (C.px opts.offset)
        _ → pure unit

    isOrdering ix opts =
      opts.source ≡ ix && abs opts.offset > 10.0

    renderDeck ix tab =
      HH.div
        [ HP.key (DID.toString tab.deckId)
        , HP.classes $
            (guard (st.activeTab ≡ Just ix) $> HH.className "active")
            <> [ HH.className "sd-tab-slot" ]
        ]
        if tab.loaded
          then [ HH.slot tab.deckId (mkDeckComponent tab.deckId) ]
          else [ HH.text "" ]

    mkDeckComponent deckId _ =
      let
        deckOpts =
          { accessType: cardOpts.deck.accessType
          , cursor: cardOpts.cursor
          , displayCursor: cardOpts.displayCursor
          , deckId
          }
      in
        { component: cardOpts.deckComponent deckOpts
        , initialState: DNS.initialState
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
      tabLen = Array.length model.tabs
      activeTab = clampActiveTab model.tabs =<< st.activeTab
    for_ st.tabs (H.fromAff ∘ EventLoop.break' ∘ _.breaker)
    tabs ← Array.catMaybes <$> for model.tabs \deckId → runMaybeT do
      cell ← MaybeT $ liftH' $ P.getDeck deckId
      breaker ← lift $ subscribeToBus' (right ∘ H.action ∘ HandleMessage deckId) cell.bus
      pure { deckId, breaker, name: cell.model.name, loaded: false }
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
  CC.ReceiveDimensions _ next → do
    H.queryAll (right (H.action DCQ.UpdateCardSize))
    pure next
  CC.ModelUpdated _ next →
    pure next
  CC.ZoomIn next →
    pure next

evalTabs ∷ CardOptions → Query ~> TabsDSL
evalTabs cardOpts = case _ of
  AddTab next → do
    ix ← addTab cardOpts
    CC.raiseUpdatedP' $ CC.EvalStateUpdate $ ES.ActiveTab ix
    CC.raiseUpdatedP' CC.EvalModelUpdate
    pure next
  HandleMessage deckId msg next → do
    case msg of
      ED.NameChange name →
        H.modify (updateName deckId name)
      _ → pure unit
    pure next
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
        Drag.subscribe' ev (right ∘ H.action ∘ Ordering ix)
        H.modify $ updateTabs ∘ _ { ordering = Just opts }
    CC.raiseUpdatedP' $ CC.EvalStateUpdate $ ES.ActiveTab ix
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
              CC.raiseUpdatedP' CC.EvalModelUpdate
              CC.raiseUpdatedP' $ CC.EvalStateUpdate $ ES.ActiveTab ix'
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

peek ∷ ∀ a. CardOptions → H.ChildF DID.DeckId DNQ.QueryP a → TabsDSL Unit
peek opts _ = pure unit

clampActiveTab ∷ ∀ a. Array a → Int → Maybe Int
clampActiveTab as ix = case Array.length as of
  0 → Nothing
  n | ix < n → Just ix
  n → Just (n - 1)

addTab ∷ CardOptions → TabsDSL Int
addTab opts = do
  deckId × cell ← liftH' $ P.freshDeck ED.emptyDeck (ED.Completed Port.emptyOut)
  liftH' $ P.linkToParent opts.cardId deckId
  breaker ← subscribeToBus' (right ∘ H.action ∘ HandleMessage deckId) cell.bus
  let tab = { deckId, breaker, name: cell.model.name, loaded: false }
  st ← H.get
  H.modify _ { tabs = Array.snoc st.tabs tab }
  pure (Array.length st.tabs)

queryDeck ∷ ∀ a. DID.DeckId → DCQ.Query a → TabsDSL (Maybe a)
queryDeck slot = H.query slot ∘ right
