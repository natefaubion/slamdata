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

module SlamData.Workspace.Deck.Model where

import SlamData.Prelude

import Data.Codec.Argonaut as CA
import SlamData.Workspace.Card.CardId as CID

type Deck =
  { name ∷ String
  , cards ∷ Array CID.CardId
  }

emptyDeck ∷ Deck
emptyDeck =
  { name: ""
  , cards: mempty
  }

codec ∷ CA.JsonCodec Deck
codec =
  CA.object "Deck" $ CA.record
    # CA.recordProp (SProxy ∷ SProxy "name") CA.string
    # CA.recordProp (SProxy ∷ SProxy "cards") (CA.array CID.codec)

eqDeck ∷ Deck → Deck → Boolean
eqDeck d1 d2 = d1.name ≡ d2.name && d1.cards ≡ d2.cards
