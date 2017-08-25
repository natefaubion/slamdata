{-
Copyright 2017 SlamData, Inc.

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


module Test.Feature.Monad where

import Prelude

import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Exception (EXCEPTION, throw)
import Control.Monad.Reader.Class (ask)
import Data.Either (either)
import Data.Time.Duration (Milliseconds)
import Node.Buffer (BUFFER)
import Node.FS (FS)
import Node.Process (PROCESS)
import Platform (PLATFORM)
import Selenium.Combinators as Combinators
import Selenium.Monad (Selenium, attempt)

type FeatureEffects eff =
    ( platform ∷ PLATFORM
    , exception ∷ EXCEPTION
    , fs ∷ FS
    , process ∷ PROCESS
    , buffer ∷ BUFFER
    | eff)

type Feature eff o =
  Selenium (FeatureEffects eff) o

await' ∷ forall eff o. Milliseconds → String → Feature eff o Boolean → Feature eff o Unit
await' timeout msg check =
  attempt (Combinators.await timeout check)
    >>= either (const $ liftEff $ throw msg) (const $ pure unit)

-- | Same as `await'` but max wait time is setted to `config.selenium.waitTime`
await ∷ forall eff o. String → Feature eff o Boolean → Feature eff o Unit
await msg check =
  ask >>= \r → await' r.defaultTimeout msg check
