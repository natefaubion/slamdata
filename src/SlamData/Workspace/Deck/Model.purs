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

module SlamData.Workspace.Deck.Model where

import SlamData.Prelude

import Control.Monad.Error.Class (throwError)

import Data.Argonaut (Json, (:=), (~>), (.?), decodeJson, jsonEmptyObject)
import Data.Path.Pathy ((</>))
import Data.Path.Pathy as Pathy

import SlamData.Workspace.Card.CardId (CardId)
import SlamData.Workspace.Card.Model as Card
import SlamData.Workspace.Deck.DeckId (DeckId, deckIdToString)

import Utils.Path (DirPath, FilePath)

type Deck =
  { parent ∷ Maybe (DeckId × CardId)
  , mirror ∷ Maybe (DeckId × Int)
  , cards ∷ Array Card.Model
  }

emptyDeck :: Deck
emptyDeck =
  { parent: Nothing
  , mirror: Nothing
  , cards: mempty
  }

encode ∷ Deck → Json
encode r
   = "version" := 3
  ~> "parent" := r.parent
  ~> "mirror" := r.mirror
  ~> "cards" := map Card.encode r.cards
  ~> jsonEmptyObject

decode ∷ Json → Either String Deck
decode = decodeJson >=> \obj → do
  case obj .? "version" of
    Right n | n ≠ 3 → throwError "Expected deck format v3"
    l → l
  parent ← obj .? "parent"
  mirror ← obj .? "mirror"
  cards ← traverse Card.decode =<< obj .? "cards"
  pure { parent, mirror, cards }

deckIndex ∷ DirPath → DeckId → FilePath
deckIndex path deckId =
  path </> Pathy.dir (deckIdToString deckId) </> Pathy.file "index"
