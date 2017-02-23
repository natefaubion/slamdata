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

module SlamData.Notification.Component
  ( State
  , Query(..)
  , NotificationItem
  , RenderMode(..)
  , Expanded
  , DetailsPresented
  , Message(..)
  , component
  , initialState
  , renderModeFromAccessType
  , module N
  ) where

import SlamData.Prelude

import Control.Monad.Aff (later', forkAff)
import Control.Monad.Aff.AVar (AVar, makeVar, takeVar, putVar)

import Data.Array as Array
import Data.HeytingAlgebra (tt, ff, implies)
import Data.Int as Int
import Data.Time.Duration (Milliseconds(..))

import Halogen as H
import Halogen.Component.Utils (busEventSource)
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP

import SlamData.Monad (Slam)
import SlamData.Notification as N
import SlamData.Wiring as Wiring
import SlamData.Workspace.AccessType (AccessType(Editable, ReadOnly))

import Utils (lowercaseFirstChar, removeLastCharIfPeriod, parenthesize)

data Expanded = Expanded | Shrunk

data DetailsPresented = DetailsPresented | DetailsHidden

detailsPresentedToBoolean ∷ DetailsPresented → Boolean
detailsPresentedToBoolean =
  case _ of
    DetailsPresented → true
    DetailsHidden → false

booleanToDetailsPresented ∷ Boolean → DetailsPresented
booleanToDetailsPresented =
  case _ of
    true → DetailsPresented
    false → DetailsHidden

instance heytingAlgebraDetailsPresented :: HeytingAlgebra DetailsPresented where
  ff =
    booleanToDetailsPresented ff
  tt =
    booleanToDetailsPresented tt
  implies a b =
    booleanToDetailsPresented
      $ (detailsPresentedToBoolean a) `implies` (detailsPresentedToBoolean b)
  conj a =
    booleanToDetailsPresented
      ∘ (conj $ detailsPresentedToBoolean a)
      ∘ detailsPresentedToBoolean
  disj a =
    booleanToDetailsPresented
      ∘ (disj $ detailsPresentedToBoolean a)
      ∘ detailsPresentedToBoolean
  not =
    booleanToDetailsPresented
      ∘ not
      ∘ detailsPresentedToBoolean

data RenderMode = Notifications | ExpandableList Expanded

type State =
  { tick ∷ Int
  , queue ∷ Array NotificationItem
  , current ∷ Maybe NotificationItem
  , dismissed ∷ Maybe NotificationItem
  , renderMode ∷ RenderMode
  }

type NotificationItem =
  { id ∷ Int
  , dismiss ∷ AVar Unit
  , options ∷ N.NotificationOptions
  , detailsPresented ∷ DetailsPresented
  }

renderModeFromAccessType ∷ AccessType → RenderMode
renderModeFromAccessType =
  case _ of
    Editable → Notifications
    ReadOnly → ExpandableList Shrunk

initialState ∷ RenderMode → State
initialState renderMode =
  { tick: 0
  , queue: [ ]
  , current: Nothing
  , dismissed: Nothing
  , renderMode
  }

data Query a
  = Init a
  | Push N.NotificationOptions (H.SubscribeStatus -> a)
  | ToggleDetail a
  | Action N.Action a
  | Dismiss a
  | ExpandList a
  | UpdateRenderMode RenderMode a

data Status
  = Current
  | Dismissed

type Message = N.Action

type NotifyHTML = H.ComponentHTML Query

type NotifyDSL = H.ComponentDSL State Query Message Slam

component ∷ RenderMode → H.Component HH.HTML Query Unit Message Slam
component renderMode =
  H.lifecycleComponent
    { initialState: const (initialState renderMode)
    , render
    , eval
    , initializer: Just (H.action Init)
    , finalizer: Nothing
    , receiver: const Nothing
    }

render ∷ State → NotifyHTML
render st =
  case st.renderMode of
    Notifications →
      HH.div
        [ HP.class_ (HH.ClassName "sd-notifications") ]
        [ renderNotification Dismissed st.dismissed
        , renderNotification Current st.current
        ]
    ExpandableList Shrunk →
      HH.div
        [ HP.class_ $ HH.ClassName "sd-notifications-list" ]
        if (Array.length all > 0)
          then
            [ HH.button
                [ HP.classes [ HH.ClassName "btn", HH.ClassName "btn-primary" ]
                , HP.type_ HP.ButtonButton
                , HE.onClick $ HE.input_ ExpandList
                ]
                [ HH.text "More details" ]
            ]
          else
            []
    ExpandableList Expanded →
      HH.div
        [ HP.class_ (HH.ClassName "sd-notifications-list") ]
        [ HH.ul_ $ renderListItem <$> all ]

  where
  all =
    maybe [] pure st.current <> st.queue

  renderNotification status = maybe (HH.text "") \n →
    HH.div
      [ HP.class_ (HH.ClassName "sd-notification-spacer") ]
      [ HH.div
          [ HP.classes (notificationClasses status n.options.notification) ]
          [ HH.div
              [ HP.class_ (HH.ClassName "sd-notification-text") ]
              ([ HH.text (notificationText n.options.notification) ]
              <> case n.options of
                   { detail: Just opts, actionOptions: Just aopts } → [ renderMessage aopts, renderDetail n opts ]
                   { detail: Just opts } → [ renderDetail n opts ]
                   { actionOptions: Just aopts } → [ renderMessage aopts, renderAction aopts ]
                   _ → [])
          , HH.div
              [ HP.class_ (HH.ClassName "sd-notification-buttons") ]
              [ HH.button
                  [ HP.class_ (HH.ClassName "sd-notification-dismiss")
                  , HP.type_ HP.ButtonButton
                  , HE.onClick (HE.input_ Dismiss)
                  ]
                  [ HH.text "×" ]
              ]
          ]
      ]

  renderListItem n =
    HH.li_
      [ HH.text
          $ removeLastCharIfPeriod (notificationText n.options.notification)
          <> maybe
               ""
               (\(N.Details s) →
                  " " <> (parenthesize $ lowercaseFirstChar $ removeLastCharIfPeriod $ s))
               n.options.detail
          <> "."
      ]

  renderDetail n (N.Details d) =
    HH.div
      [ HP.class_ (HH.ClassName "sd-notification-detail") ]
      [ HH.button
          [ HP.classes [ HH.ClassName "btn", HH.ClassName "btn-default", HH.ClassName "btn-sm" ]
          , HP.type_ HP.ButtonButton
          , HE.onClick (HE.input_ ToggleDetail)
          ]
          [ HH.text
              case n.detailsPresented of
                DetailsPresented → "Hide detail"
                DetailsHidden → "Show detail"
          ]
      , n.options.actionOptions
          # maybe (HH.text "") renderActionButton
      , case n.detailsPresented of
          DetailsPresented →
            HH.div
              [ HP.class_ (HH.ClassName "sd-notification-detail") ]
              [ HH.p_ [ HH.text d ] ]
          DetailsHidden → HH.text ""
      ]

  renderAction opts@(N.ActionOptions a) =
    HH.div
      [ HP.class_ (HH.ClassName "sd-notification-detail") ]
      [ renderActionButton opts ]

  renderActionButton (N.ActionOptions a) =
    HH.button
      [ HP.classes [ HH.ClassName "btn", HH.ClassName "btn-primary", HH.ClassName "btn-sm" ]
      , HP.type_ HP.ButtonButton
      , HE.onClick (HE.input_ $ Action a.action)
      ]
      [ HH.text a.actionMessage ]

  renderMessage (N.ActionOptions { message })
      | message ≡ "" = HH.text ""
      | otherwise =
          HH.div
            [ HP.class_ (HH.ClassName "sd-notification-detail") ]
            [ HH.p_ [ HH.text message ] ]

  notificationClasses status n =
    [ HH.ClassName "sd-notification"
    , HH.ClassName case n of
        N.Info _    → "info"
        N.Warning _ → "warning"
        N.Error _   → "error"
    , HH.ClassName case status of
        Current   → "current"
        Dismissed → "dismissed"
    ]

  notificationText = case _ of
    N.Info s → s
    N.Warning s → s
    N.Error s → s

eval ∷ Query ~> NotifyDSL
eval = case _ of
  Init next → do
    { bus } ← Wiring.expose
    H.subscribe $ busEventSource (H.request <<< Push) bus.notify
    pure next
  Push options reply → do
    st ← H.get
    dismiss ← H.liftAff makeVar
    when
      (not (options `optionsEqItem` st.current ∨ options `optionsEqItem` Array.last st.queue))
      do
        let item =
              { id: st.tick
              , dismiss
              , options
              , detailsPresented: DetailsHidden
              }
        H.modify _
          { tick = st.tick + 1
          , queue = Array.snoc st.queue item
          }
        when (isNothing st.current) drainQueue
    pure $ reply H.Listening
  Dismiss next → do
    dismissNotification
    pure next
  Action act next → do
    dismissNotification
    H.raise act
    pure next
  ToggleDetail next → do
    current ← H.gets _.current
    for_ current \curr →
      H.modify _ { current = Just curr { detailsPresented = not curr.detailsPresented } }
    pure next
  ExpandList next →
    H.modify _ { renderMode = ExpandableList Expanded } $> next
  UpdateRenderMode renderMode next →
    H.modify _ { renderMode = renderMode } $> next
  where
  optionsEqItem ∷ N.NotificationOptions → Maybe NotificationItem → Boolean
  optionsEqItem options =
    maybe false (N.optionsWithoutActionEq options) ∘ map _.options


dismissNotification ∷ NotifyDSL Unit
dismissNotification = do
  current ← H.gets _.current
  for_ current \{ dismiss } →
    H.liftAff $ putVar dismiss unit

drainQueue ∷ NotifyDSL Unit
drainQueue = do
  st ← H.get
  case Array.uncons st.queue of
    Nothing → do
      H.modify _
        { current = Nothing
        , dismissed = st.current
        }
    Just { head, tail } → do
      H.modify _
        { queue = tail
        , current = Just head
        , dismissed = st.current
        }

      -- TODO: Make notifications with timeouts not disapear in ExpandableList
      -- render mode
      for_ head.options.timeout \(Milliseconds ms) →
        H.liftAff $ forkAff $ later' (Int.floor ms) (putVar head.dismiss unit)

      H.liftAff $ takeVar head.dismiss
      drainQueue
