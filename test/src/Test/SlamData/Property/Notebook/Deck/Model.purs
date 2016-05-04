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

module Test.SlamData.Property.Notebook.Deck.Model
  ( ArbDeck
  , runArbDeck
  , check
  ) where

import SlamData.Prelude

import Data.Array (zipWith)
import Data.Map as M

import SlamData.Notebook.Deck.Model as Model

import Test.StrongCheck
  (QC, Result(..), class Arbitrary, arbitrary, quickCheck, (<?>))
import Test.SlamData.Property.Notebook.Card.Model (runArbCard, checkCardEquality)
import Test.SlamData.Property.Notebook.Card.CardId (runArbCardId)

newtype ArbDeck = ArbDeck Model.Deck

runArbDeck ∷ ArbDeck → Model.Deck
runArbDeck (ArbDeck m) = m

instance arbitraryArbNotebook ∷ Arbitrary ArbDeck where
  arbitrary = do
    cards ← map runArbCard <$> arbitrary
    dependencies ← M.fromList ∘ map (bimap runArbCardId runArbCardId) <$> arbitrary
    name ← arbitrary
    pure $ ArbDeck { name, cards, dependencies }

check ∷ QC Unit
check = quickCheck $ runArbDeck ⋙ \model →
  case Model.decode (Model.encode model) of
    Left err → Failed $ "Decode failed: " ⊕ err
    Right model' →
      fold
       [ model.dependencies ≡ model'.dependencies <?> "dependencies mismatch"
       , fold (zipWith checkCardEquality model.cards model'.cards)
       ]
