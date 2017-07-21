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

module SlamData.Wiring.Cache
  ( Cache
  , make
  , make'
  , restore
  , get
  , alter
  , modify
  , put
  , remove
  , clear
  , snapshot
  , merge
  , mergeWith
  ) where

import SlamData.Prelude

import Control.Monad.Eff.Class (class MonadEff, liftEff)
import Control.Monad.Eff.Ref (Ref, REF, newRef, readRef, writeRef)
import Data.Map (Map)
import Data.Map as Map

newtype Cache k v = Cache (Ref (Map k v))

make
  ∷ ∀ eff m k v
  . MonadEff (ref ∷ REF | eff) m
  ⇒ Ord k
  ⇒ m (Cache k v)
make = make' Map.empty

make'
  ∷ ∀ eff m k v
  . MonadEff (ref ∷ REF | eff) m
  ⇒ Ord k
  ⇒ Map k v
  → m (Cache k v)
make' initial = liftEff do
  Cache <$> newRef initial

restore
  ∷ ∀ eff m k v
  . MonadEff (ref ∷ REF | eff) m
  ⇒ Ord k
  ⇒ Map k v
  → Cache k v
  → m Unit
restore m (Cache cache) = liftEff do
  writeRef cache m

get
  ∷ ∀ eff m k v
  . MonadEff (ref ∷ REF | eff) m
  ⇒ Ord k
  ⇒ k
  → Cache k v
  → m (Maybe v)
get key (Cache cache) = liftEff do
  Map.lookup key <$> readRef cache

alter
  ∷ ∀ eff m k v
  . MonadEff (ref ∷ REF | eff) m
  ⇒ Ord k
  ⇒ k
  → (Maybe v → Maybe v)
  → Cache k v
  → m Unit
alter key fn (Cache cache) = liftEff do
  writeRef cache ∘ Map.alter fn key =<< readRef cache

modify
  ∷ ∀ eff m k v
  . MonadEff (ref ∷ REF | eff) m
  ⇒ Ord k
  ⇒ k
  → (v → v)
  → Cache k v
  → m Unit
modify key fn (Cache cache) = liftEff do
  writeRef cache ∘ Map.update (Just ∘ fn) key =<< readRef cache

put
  ∷ ∀ eff m k v
  . MonadEff (ref ∷ REF | eff) m
  ⇒ Ord k
  ⇒ k
  → v
  → Cache k v
  → m Unit
put key val (Cache cache) = liftEff do
  writeRef cache ∘ Map.insert key val =<< readRef cache

remove
  ∷ ∀ eff m k v
  . MonadEff (ref ∷ REF | eff) m
  ⇒ Ord k
  ⇒ k
  → Cache k v
  → m (Maybe v)
remove key (Cache cache) = liftEff do
  vals ← readRef cache
  case Map.pop key vals of
    Just (Tuple val vals') → do
      writeRef cache vals'
      pure (Just val)
    Nothing →
      pure Nothing

clear
  ∷ ∀ eff m k v
  . MonadEff (ref ∷ REF | eff) m
  ⇒ Ord k
  ⇒ Cache k v
  → m Unit
clear = restore Map.empty

snapshot
  ∷ ∀ eff m k v
  . MonadEff (ref ∷ REF | eff) m
  ⇒ Cache k v
  → m (Map k v)
snapshot (Cache cache) = liftEff do
  readRef cache

merge
  ∷ ∀ eff m k v
  . MonadEff (ref ∷ REF | eff) m
  ⇒ Ord k
  ⇒ Map k v
  → Cache k v
  → m Unit
merge = mergeWith const

mergeWith
  ∷ ∀ eff m k v
  . MonadEff (ref ∷ REF | eff) m
  ⇒ Ord k
  ⇒ (v → v → v)
  → Map k v
  → Cache k v
  → m Unit
mergeWith f ks (Cache cache) = liftEff do
  writeRef cache ∘ Map.unionWith f ks =<< readRef cache
