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

module SlamData.Notebook.Deck.DeckId
  ( DeckId(..)
  , runDeckId
  , stringToDeckId
  , deckIdToString
  ) where

import SlamData.Prelude

import Data.Argonaut (class DecodeJson, class EncodeJson, decodeJson, encodeJson)
import Data.Int as Int

newtype DeckId = DeckId Int

runDeckId ∷ DeckId → Int
runDeckId (DeckId i) = i

stringToDeckId ∷ String → Either String DeckId
stringToDeckId = maybe (Left "incorrect deck id") (Right ∘ DeckId) ∘ Int.fromString

deckIdToString ∷ DeckId → String
deckIdToString = show ∘ runDeckId

derive instance eqDeckId ∷ Eq DeckId
derive instance ordDeckId ∷ Ord DeckId

instance encodeJsonDeckId ∷ EncodeJson DeckId where
  encodeJson = encodeJson ∘ runDeckId

instance decodeJsonDeckId ∷ DecodeJson DeckId where
  decodeJson json = DeckId <$> decodeJson json
