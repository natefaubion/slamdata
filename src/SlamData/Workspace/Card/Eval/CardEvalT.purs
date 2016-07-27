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
  , addSource
  , addCache
  , addSources
  , addCaches
  , additionalSources
  , runCardEvalT
  , runCardEvalT_
  , temporaryOutputResource
  ) where

import SlamData.Prelude

import Data.Map as Map
import Data.Path.Pathy ((</>))
import Data.Path.Pathy as Path
import Data.Set as Set

import Control.Monad.Error.Class as EC
import Control.Monad.Except.Trans as ET
import Control.Monad.Reader.Class as RC
import Control.Monad.RWS.Trans as RWS
import Control.Monad.State.Class as SC
import Control.Monad.Writer.Class as WC

import SlamData.Workspace.Card.CardId as CID
import SlamData.Workspace.Card.Model as CM
import SlamData.Workspace.Card.Port as Port
import SlamData.Workspace.Deck.DeckId as DID
import SlamData.Workspace.Deck.AdditionalSource (AdditionalSource(..))


import Utils.Path (DirPath, FilePath)

type CardEvalInput =
  { path ∷ DirPath
  , input ∷ Maybe Port.Port
  , cardCoord ∷ DID.DeckId × CID.CardId
  , urlVarMaps ∷ Map.Map DID.DeckId Port.URLVarMap
  }

type CardEvalTP m = ET.ExceptT String (RWS.RWST Port.Port (Set.Set AdditionalSource) CM.AnyCardModel m)

newtype CardEvalT m a = CardEvalT (CardEvalTP m a)

getCardEvalT ∷ ∀ m a. CardEvalT m a → CardEvalTP m a
getCardEvalT (CardEvalT m) = m

instance functorCardEvalT ∷ Functor m ⇒ Functor (CardEvalT m) where
  map f = getCardEvalT ⋙ map f ⋙ CardEvalT

instance applyCardEvalT ∷ Bind m ⇒ Apply (CardEvalT m) where
  apply (CardEvalT f) = getCardEvalT ⋙ apply f ⋙ CardEvalT

instance applicativeCardEvalT ∷ Monad m ⇒ Applicative (CardEvalT m) where
  pure = pure ⋙ CardEvalT

instance bindCardEvalT ∷ Monad m ⇒ Bind (CardEvalT m) where
  bind (CardEvalT m) = (_ ⋙ getCardEvalT) ⋙ bind m ⋙ CardEvalT

instance monadCardEvalT ∷ Monad m ⇒ Monad (CardEvalT m)

instance monadTransCardEvalT ∷ MonadTrans CardEvalT where
  lift = lift ⋙ lift ⋙ CardEvalT

instance monadReaderCardEvalT ∷ Monad m ⇒ RC.MonadReader Port.Port (CardEvalT m) where
  ask = CardEvalT RC.ask
  local f = getCardEvalT ⋙ RC.local f ⋙ CardEvalT

instance monadStateCardEvalT ∷ Monad m ⇒ SC.MonadState CM.AnyCardModel (CardEvalT m) where
  state = SC.state ⋙ CardEvalT

instance monadWriterCardEvalT ∷ Monad m ⇒ WC.MonadWriter (Set.Set AdditionalSource) (CardEvalT m) where
  writer = WC.writer ⋙ CardEvalT
  listen = getCardEvalT ⋙ WC.listen ⋙ CardEvalT
  pass = getCardEvalT ⋙ WC.pass ⋙ CardEvalT

instance monadErrorCardEvalT ∷ Monad m ⇒ EC.MonadError String (CardEvalT m) where
  throwError = EC.throwError ⋙ CardEvalT
  catchError (CardEvalT m) = CardEvalT ∘ EC.catchError m ∘ (getCardEvalT ∘ _)

-- TODO: Take previous model state
-- TODO: Take Port environment
runCardEvalT
  ∷ ∀ m
  . Functor m
  ⇒ CardEvalT m Port.Port
  → m (Port.Port × (Set.Set AdditionalSource))
runCardEvalT (CardEvalT m) =
  RWS.runRWST (ET.runExceptT m) Port.Blocked CM.ErrorCard <#> \(RWS.RWSResult _ r ms) →
    (either Port.CardError id r) × ms

-- TODO: Remove this function, it makes no sense.
runCardEvalT_
  ∷ ∀ m
  . Functor m
  ⇒ CardEvalT m Unit → m Unit
runCardEvalT_ (CardEvalT m) =
  RWS.runRWST (ET.runExceptT m) Port.Blocked CM.ErrorCard <#> \(RWS.RWSResult _ x _) → either (const unit) id x

addSource
  ∷ ∀ m
  . (WC.MonadWriter (Set.Set AdditionalSource) m)
  ⇒ FilePath
  → m Unit
addSource fp = WC.tell $ Set.singleton $ Source fp

addCache
  ∷ ∀ m
  . (WC.MonadWriter (Set.Set AdditionalSource) m)
  ⇒ FilePath
  → m Unit
addCache fp = WC.tell $ Set.singleton $ Cache fp

addSources
  ∷ ∀ m f
  . (Foldable f, WC.MonadWriter (Set.Set AdditionalSource) m)
  ⇒ f FilePath
  → m Unit
addSources fps = WC.tell $ foldMap (Set.singleton ∘ Source) fps

addCaches
  ∷ ∀ m f
  . (Foldable f, WC.MonadWriter (Set.Set AdditionalSource) m)
  ⇒ f FilePath
  → m Unit
addCaches fps = WC.tell $ foldMap (Set.singleton ∘ Cache) fps

additionalSources
  ∷ ∀ m f
  . (Foldable f, WC.MonadWriter (Set.Set AdditionalSource) m)
  ⇒ f AdditionalSource
  → m Unit
additionalSources = WC.tell ∘ foldMap Set.singleton

temporaryOutputResource ∷
  ∀ r
  . { path ∷ DirPath, cardCoord ∷ DID.DeckId × CID.CardId | r }
  → FilePath
temporaryOutputResource { path, cardCoord: deckId × cardId } =
  path
    </> Path.dir ".tmp"
    </> Path.dir (DID.deckIdToString deckId)
    </> Path.file ("out" ⊕ CID.cardIdToString cardId)
