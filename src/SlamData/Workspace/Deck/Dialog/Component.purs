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

module SlamData.Workspace.Deck.Dialog.Component
  ( Dialog(..)
  , State
  , initialState
  , Query(..)
  , ChildSlot
  , ChildQuery
  , ChildState
  , StateP
  , QueryP
  , comp
  ) where

import SlamData.Prelude

import Halogen as H
import Halogen.Component.ChildPath (ChildPath, (:>), cpL, cpR)
import Halogen.HTML.Indexed as HH
import Halogen.HTML.Properties.Indexed as HP
import Halogen.Component.Utils (raise')

import SlamData.Dialog.Error.Component as Error
import SlamData.Workspace.Card.Port.VarMap as Port
import SlamData.Workspace.Deck.Dialog.Confirm.Component as Confirm
import SlamData.Workspace.Deck.Dialog.Embed.Component as Embed
import SlamData.FileSystem.Dialog.Share.Component as Share
import SlamData.Effects (Slam)

data Dialog
  = Error String
  | Embed String Port.VarMap
  | Share String
  | DeleteDeck

type State = Maybe Dialog

initialState ∷ State
initialState = Nothing

data Query a
  = Dismiss a
  | Confirm Dialog Boolean a
  | Show Dialog a

type ChildState =
  Error.State ⊹ Embed.State ⊹ Share.State ⊹ Confirm.State

type ChildQuery =
  Error.Query ⨁ Embed.Query ⨁ Share.Query ⨁  Confirm.Query

type ChildSlot =
  Unit ⊹ Unit ⊹ Unit ⊹ Unit

cpError
  ∷ ChildPath
       Error.State ChildState
       Error.Query ChildQuery
       Unit ChildSlot
cpError = cpL

cpEmbed
  ∷ ChildPath
       Embed.State ChildState
       Embed.Query ChildQuery
       Unit ChildSlot
cpEmbed = cpR :> cpL

cpShare
  ∷ ChildPath
      Share.State ChildState
      Share.Query ChildQuery
      Unit ChildSlot
cpShare = cpR :> cpR :> cpL

cpDeleteDeck
  ∷ ChildPath
      Confirm.State ChildState
      Confirm.Query ChildQuery
      Unit ChildSlot
cpDeleteDeck = cpR :> cpR :> cpR

type StateP = H.ParentState State ChildState Query ChildQuery Slam ChildSlot
type QueryP = Coproduct Query (H.ChildF ChildSlot ChildQuery)

type HTML = H.ParentHTML ChildState Query ChildQuery Slam ChildSlot
type DSL = H.ParentDSL State ChildState Query ChildQuery Slam ChildSlot

comp ∷ H.Component StateP QueryP Slam
comp = H.parentComponent { render, eval, peek: Just (peek ∘ H.runChildF) }

render ∷ State → HTML
render state =
  HH.div
    [ HP.classes [ HH.className "deck-dialog" ] ]
    $ foldMap (pure ∘ dialog) state
  where

  dialog (Error str) =
    HH.slot' cpError unit \_ →
      { component: Error.nonModalComp
      , initialState: Error.State str
      }

  dialog (Embed url varMap) =
    HH.slot' cpEmbed unit \_ →
      { component: Embed.comp
      , initialState: { url, varMap }
      }

  dialog (Share str) =
    HH.slot' cpShare unit \_ →
      { component: Share.nonModalComp
      , initialState: Share.State str
      }

  dialog DeleteDeck =
    HH.slot' cpDeleteDeck unit \_ →
      { component: Confirm.comp
      , initialState:
          { title: "Delete deck"
          , body: "Are you sure you want to delete this deck?"
          , cancel: "Cancel"
          , confirm: "Delete"
          }
      }

eval ∷ Natural Query DSL
eval (Dismiss next) = H.set Nothing $> next
eval (Confirm _ _ next) = H.set Nothing $> next
eval (Show d next) = H.set (Just d) $> next

peek ∷ ∀ a. ChildQuery a → DSL Unit
peek =
  errorPeek ⨁ embedPeek ⨁ sharePeek ⨁ deleteDeckPeek

-- Send `Dismiss` after child's `Dismiss` to simplify parent of
-- this component peeking. (I.e. it can observe only this component queries and
-- don't provide separate handlers for embed dialog)
errorPeek ∷ ∀ a. Error.Query a → DSL Unit
errorPeek (Error.Dismiss _) = do
  raise' $ Dismiss unit

embedPeek ∷ ∀ a. Embed.Query a → DSL Unit
embedPeek (Embed.Dismiss _) = do
  raise' $ Dismiss unit
embedPeek _ = pure unit

sharePeek ∷ ∀ a. Share.Query a → DSL Unit
sharePeek (Share.Dismiss _) = do
  raise' $ Dismiss unit
sharePeek _ = pure unit

deleteDeckPeek ∷ ∀ a. Confirm.Query a → DSL Unit
deleteDeckPeek (Confirm.Confirm b _) = do
  H.get >>= traverse_ \dialog →
    raise' $ Confirm dialog b unit
