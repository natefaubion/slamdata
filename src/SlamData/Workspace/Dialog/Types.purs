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

module SlamData.Workspace.Dialog.Types where

import SlamData.Prelude
import Data.Map as Map
import SlamData.Theme.Theme as Theme
import SlamData.Workspace.Card.CardId (CardId)
import SlamData.Workspace.Card.CardType (CardType)
import SlamData.Workspace.Card.InsertableCardType (InsertableCardType)
import SlamData.Workspace.Card.Port.VarMap as Port
import SlamData.Workspace.Deck.Options (DeckOptions)
import SlamData.Workspace.Dialog.Share.Model (SharingInput)
import SlamData.Workspace.Dialog.VizUnavailable.Component as VU
import SlamData.License (LicenseProblem)

data Dialog
  = Error DeckOptions String
  | Embed DeckOptions SharingInput (Map.Map CardId Port.VarMap)
  | Publish DeckOptions SharingInput (Map.Map CardId Port.VarMap)
  | Reason DeckOptions CardType String (Array (Array InsertableCardType))
  | Share DeckOptions SharingInput
  | Unshare DeckOptions SharingInput
  | Rename DeckOptions String
  | Theme DeckOptions (Maybe Theme.Theme)
  | DeleteDeck DeckOptions
  | LicenseProblem LicenseProblem
  | VizUnavailable VU.State
