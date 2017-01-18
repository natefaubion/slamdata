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
  , unCache
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

import Control.Monad.Aff.AVar (AVar, AVAR, makeVar', takeVar, putVar, modifyVar)
import Control.Monad.Aff.Class (class MonadAff, liftAff)

import Data.Map (Map)
import Data.Map as Map

newtype Cache k v = Cache (AVar (Map k v))

unCache ∷ ∀ k v. Cache k v → AVar (Map k v)
unCache (Cache m) = m

make
  ∷ ∀ eff m k v
  . (MonadAff (avar ∷ AVAR | eff) m, Ord k)
  ⇒ m (Cache k v)
make = liftAff (Cache <$> makeVar' mempty)

make'
  ∷ ∀ eff m k v
  . (MonadAff (avar ∷ AVAR | eff) m, Ord k)
  ⇒ Map k v
  → m (Cache k v)
make' = liftAff ∘ map Cache ∘ makeVar'

restore
  ∷ ∀ eff m k v
  . (MonadAff (avar ∷ AVAR | eff) m, Ord k)
  ⇒ Map k v
  → Cache k v
  → m Unit
restore m (Cache c) =
  liftAff (modifyVar (const m) c)

get
  ∷ ∀ eff m k v
  . (MonadAff (avar ∷ AVAR | eff) m, Ord k)
  ⇒ k
  → Cache k v
  → m (Maybe v)
get key (Cache cache) = liftAff do
  vals ← takeVar cache
  putVar cache vals
  pure (Map.lookup key vals)

alter
  ∷ ∀ eff m k v
  . (MonadAff (avar ∷ AVAR | eff) m, Ord k)
  ⇒ k
  → (Maybe v → m (Maybe v))
  → Cache k v
  → m Unit
alter key fn (Cache cache) = do
  vals ← liftAff $ takeVar cache
  let val = Map.lookup key vals
  val' ← fn val
  liftAff $ case val, val' of
    Nothing, Nothing → putVar cache vals
    Just _ , Nothing → putVar cache (Map.delete key vals)
    _      , Just v  → putVar cache (Map.insert key v vals)

modify
  ∷ ∀ eff m k v
  . (MonadAff (avar ∷ AVAR | eff) m, Ord k)
  ⇒ k
  → (v → v)
  → Cache k v
  → m Unit
modify key fn (Cache cache) = liftAff do
  modifyVar (Map.update (Just ∘ fn) key) cache

put
  ∷ ∀ eff m k v
  . (MonadAff (avar ∷ AVAR | eff) m, Ord k)
  ⇒ k
  → v
  → Cache k v
  → m Unit
put key val (Cache cache) = liftAff do
  modifyVar (Map.insert key val) cache

remove
  ∷ ∀ eff m k v
  . (MonadAff (avar ∷ AVAR | eff) m, Ord k)
  ⇒ k
  → Cache k v
  → m (Maybe v)
remove key (Cache cache) = liftAff do
  vals ← takeVar cache
  case Map.pop key vals of
    Just (Tuple val vals') →
      putVar cache vals'
        $> Just val
    Nothing →
      putVar cache vals
        $> Nothing

clear
  ∷ ∀ eff m k v
  . (MonadAff (avar ∷ AVAR | eff) m, Ord k)
  ⇒ Cache k v
  → m Unit
clear (Cache cache) = liftAff do
  modifyVar (const mempty) cache

snapshot
  ∷ ∀ eff m k v
  . MonadAff (avar ∷ AVAR | eff) m
  ⇒ Cache k v
  → m (Map k v)
snapshot (Cache cache) = liftAff do
  vals ← takeVar cache
  putVar cache vals
  pure vals

merge
  ∷ ∀ eff m k v
  . (MonadAff (avar ∷ AVAR | eff) m, Ord k)
  ⇒ Map k v
  → Cache k v
  → m Unit
merge = mergeWith const

mergeWith
  ∷ ∀ eff m k v
  . (MonadAff (avar ∷ AVAR | eff) m, Ord k)
  ⇒ (v → v → v)
  → Map k v
  → Cache k v
  → m Unit
mergeWith f ks (Cache cache) = liftAff do
  modifyVar (Map.unionWith f ks) cache
