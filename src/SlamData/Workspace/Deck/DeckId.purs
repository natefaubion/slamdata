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

module SlamData.Workspace.Deck.DeckId
  ( DeckId
  , stringToDeckId
  , deckIdToString
  , freshDeckId
  ) where

import SlamData.Prelude
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Random as Rand
import Data.String as S
import Data.String.Regex as Rx

import Data.Argonaut (class DecodeJson, class EncodeJson, decodeJson, encodeJson)
import Test.StrongCheck as SC
import Test.StrongCheck.Gen as Gen

newtype DeckId = DeckId String

instance arbitraryDeckId ∷ SC.Arbitrary DeckId where
  arbitrary =
    DeckId <$> do
      x1 ← genString 8
      x2 ← genString 4
      x3 ← genString 4
      x4 ← genString 4
      x5 ← genString 12
      pure $ x1 <> "-" <> x2 <> "-" <> x3 <> "-" <> x4 <> "-" <> x5

    where
      genString n =
        Gen.vectorOf n SC.arbitrary
          <#> S.fromCharArray

stringToDeckId ∷ String → Either String DeckId
stringToDeckId str =
  if Rx.test (Rx.regex "\\w{8}-\\w{4}-\\w{4}-\\w{4}-\\w{12}" Rx.noFlags) str
  then Right $ DeckId str
  else Left $ "Invalid DeckId: " <> str

deckIdToString ∷ DeckId → String
deckIdToString (DeckId x) = x

derive instance genericDeckId ∷ Generic DeckId
derive instance eqDeckId ∷ Eq DeckId
derive instance ordDeckId ∷ Ord DeckId

instance encodeJsonDeckId ∷ EncodeJson DeckId where
  encodeJson (DeckId x) = encodeJson x

instance decodeJsonDeckId ∷ DecodeJson DeckId where
  decodeJson = map DeckId ∘ decodeJson

foreign import _generateUUID ∷ ∀ eff. Eff (random ∷ Rand.RANDOM | eff) String

freshDeckId ∷ ∀ eff. Eff (random ∷ Rand.RANDOM | eff) DeckId
freshDeckId = DeckId <$> _generateUUID
