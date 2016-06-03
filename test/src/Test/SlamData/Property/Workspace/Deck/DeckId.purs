{-
Copyright 2015 SlamData, Inc.

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

module Test.SlamData.Property.Workspace.Deck.DeckId where

import Prelude

import Data.Argonaut (encodeJson, decodeJson)
import Data.Either (Either(..))

import SlamData.Workspace.Deck.DeckId (DeckId(..))

import Test.StrongCheck (QC, Result(..), class Arbitrary, arbitrary, quickCheck, (<?>))

newtype ArbDeckId = ArbDeckId DeckId

runArbDeckId :: ArbDeckId -> DeckId
runArbDeckId (ArbDeckId m) = m

instance arbitraryArbDeckId :: Arbitrary ArbDeckId where
  arbitrary = ArbDeckId <<< DeckId <$> arbitrary

check :: QC Unit
check = quickCheck $ runArbDeckId >>> \ci ->
  case decodeJson (encodeJson ci) of
    Left err -> Failed $ "Decode failed: " ++ err
    Right ci' -> ci == ci' <?> "DeckId failed to decode as encoded value"
