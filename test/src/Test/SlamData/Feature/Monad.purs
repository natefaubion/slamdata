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

module Test.SlamData.Feature.Monad where

import Prelude

import Control.Monad.Reader.Class (ask)

import Data.Time.Duration (Milliseconds)

import Selenium.Monad (later)

import Test.Feature.Log (warnMsg)
import Test.Feature.Monad (Feature)
import Test.SlamData.Feature.Config (Config)
import Test.SlamData.Feature.Effects (SlamFeatureEffects)

type SlamFeature = Feature (SlamFeatureEffects ()) (config :: Config)

data Connector = Couchbase | Mongo | Marklogic

getConnector ∷ SlamFeature Connector
getConnector = do
  config <- getConfig
  case config.database.host of
    "couchbase" → pure Couchbase
    "marklogic" → pure Marklogic
    _ → pure Mongo

isMarklogic :: SlamFeature  Boolean
isMarklogic = getConnector <#> case _ of
      Marklogic → true
      _ → false

isCouchbase :: SlamFeature  Boolean
isCouchbase = getConnector <#> case _ of
      Couchbase → true
      _ → false

getConfig :: SlamFeature Config
getConfig = _.config <$> ask

waitTime :: Milliseconds -> SlamFeature Unit
waitTime t = do
  warnMsg $ "Warning: Tests manually waited for " <> show t <> " milliseconds."
  later t $ pure unit
