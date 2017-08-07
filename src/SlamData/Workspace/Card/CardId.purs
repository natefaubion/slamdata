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

module SlamData.Workspace.Card.CardId
  ( CardId
  , make
  , fromString
  , fromString'
  , toString
  , codec
  , unsafeFromInt
  ) where

import SlamData.Prelude

import Control.Monad.Eff.Class (class MonadEff)
import Control.Monad.Eff.Random as Random
import Data.Argonaut as J
import Data.Codec ((<~<))
import Data.Codec as C
import Data.Codec.Argonaut as CA
import Data.String as String
import Data.UUID.Random as UUID
import Test.StrongCheck.Arbitrary as SC
import Utils (replicate)

newtype CardId = CardId UUID.UUIDv4

make ∷ ∀ m eff. MonadEff (random ∷ Random.RANDOM | eff) m ⇒ m CardId
make = CardId <$> UUID.make

fromString ∷ String → Maybe CardId
fromString = map CardId <<< UUID.fromString

fromString' ∷ String → Either String CardId
fromString' s = case fromString s of
  Just d  → Right d
  Nothing → Left $ "Invalid CardId: " <> s

toString ∷ CardId → String
toString (CardId u) = UUID.toString u

derive instance eqCardId ∷ Eq CardId
derive instance ordCardId ∷ Ord CardId

instance showCardId ∷ Show CardId where
  show c = "CardId " <> toString c

instance arbitraryDeckId ∷ SC.Arbitrary CardId where
  arbitrary = CardId <$> SC.arbitrary

codec ∷ CA.JsonCodec CardId
codec = dimap (\(CardId uuid) → uuid) CardId UUID.codec <~< migrationCodec
  where
    migrationCodec ∷ CA.JsonCodec J.Json
    migrationCodec = C.basicCodec (\j → pure $ either (const j) (J.fromString ∘ toString ∘ unsafeFromInt) (C.decode CA.int j)) id

unsafeFromInt ∷ Int → CardId
unsafeFromInt i =
  let
    str = String.take 12 (show i)
    len = String.length str
    block = if len < 12 then replicate (12 - len) "0" <> str else str
  in
    unsafePartial
      $ fromJust
      $ fromString ("00000000-0000-4000-8000-" <> block)
