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

module SlamData.Workspace.Card.Eval.CardEvalT
  ( CardEvalInput
  , CardEvalT
  , runCardEvalT
  , runCardEvalT_
  , temporaryOutputResource
  ) where

import SlamData.Prelude

import Data.Path.Pathy ((</>))
import Data.Path.Pathy as Path

import Control.Monad.Error.Class as EC
import Control.Monad.Except.Trans as ET

import SlamData.Workspace.Card.CardId as CID
import SlamData.Workspace.Card.Port as Port
import SlamData.Workspace.Card.Port.VarMap as VM
import SlamData.Workspace.Deck.DeckId as DID

import Utils.Path (DirPath, FilePath)

type CardEvalInput =
  { path ∷ DirPath
  , input ∷ Maybe Port.Port
  , cardCoord ∷ DID.DeckId × CID.CardId
  , globalVarMap ∷ VM.VarMap
  }

type CardEvalTP m = ET.ExceptT String m

newtype CardEvalT m a = CardEvalT (CardEvalTP m a)

getCardEvalT ∷ ∀ m a. CardEvalT m a → CardEvalTP m a
getCardEvalT (CardEvalT m) = m

instance functorCardEvalT ∷ Functor m ⇒ Functor (CardEvalT m) where
  map f = getCardEvalT ⋙ map f ⋙ CardEvalT

instance applyCardEvalT ∷ Apply m ⇒ Apply (CardEvalT m) where
  apply (CardEvalT f) = getCardEvalT ⋙ apply f ⋙ CardEvalT

instance applicativeCardEvalT ∷ Applicative m ⇒ Applicative (CardEvalT m) where
  pure = pure ⋙ CardEvalT

instance bindCardEvalT ∷ Monad m ⇒ Bind (CardEvalT m) where
  bind (CardEvalT m) = (_ ⋙ getCardEvalT) ⋙ bind m ⋙ CardEvalT

instance monadCardEvalT ∷ Monad m ⇒ Monad (CardEvalT m)

instance monadTransCardEvalT ∷ MonadTrans CardEvalT where
  lift = lift ⋙ CardEvalT

instance monadErrorCardEvalT ∷ Monad m ⇒ EC.MonadError String (CardEvalT m) where
  throwError = EC.throwError ⋙ CardEvalT
  catchError (CardEvalT m) = CardEvalT ∘ EC.catchError m ∘ (getCardEvalT ∘ _)

runCardEvalT ∷ ∀ m. Functor m ⇒ CardEvalT m Port.Port → m Port.Port
runCardEvalT (CardEvalT m) =
  ET.runExceptT m <#> either Port.CardError id

runCardEvalT_ ∷ ∀ m. Functor m ⇒ CardEvalT m Unit → m Unit
runCardEvalT_ (CardEvalT m) =
  ET.runExceptT m <#> either (const unit) id

temporaryOutputResource ∷
  ∀ r
  . { path ∷ DirPath, cardCoord ∷ DID.DeckId × CID.CardId | r }
  → FilePath
temporaryOutputResource { path, cardCoord = deckId × cardId } =
  outputDirectory </> outputFile

  where
  outputRoot =
    if path ≡ Path.rootDir
      then Path.rootDir </> Path.dir ".tmp"
      else path

  outputDirectory =
    outputRoot </> Path.dir (DID.deckIdToString deckId)

  outputFile =
    Path.file $
      "out" ⊕ CID.cardIdToString cardId
