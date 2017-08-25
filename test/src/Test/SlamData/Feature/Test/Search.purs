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

module Test.SlamData.Feature.Test.Search where

import Prelude

import Data.Maybe (Maybe(..))
import Test.Feature.Log (successMsg)
import Test.Feature.Scenario (KnownIssues, noIssues, scenario)
import Test.SlamData.Feature.Expectations as Expect
import Test.SlamData.Feature.Interactions as Interact
import Test.SlamData.Feature.Monad (Connector(..), SlamFeature, getConnector)


searchScenario
  ∷ String
  → KnownIssues
  → SlamFeature Unit
  → SlamFeature Unit
searchScenario scenarioName knownIssues implementation = do
  connector ← getConnector
  scenario
    { epic: "Search"
    , before: Interact.createWorkspaceInTestFolder "Search"
    , after: Interact.deleteFileInTestFolder "Search.slam"
        *> Interact.browseRootFolder
    , title: scenarioName
    , knownIssues
    , connector
    }
    implementation

selectFile ∷ SlamFeature Unit
selectFile = Interact.selectFileForLastOpenCard "/test-mount/testDb/zips"

test ∷ SlamFeature Unit
test = do
  connector ← getConnector
  searchScenario "Search for a city in zips"
    (noIssues {
        couchbase = Just "CB: https://github.com/quasar-analytics/quasar/issues/2341",
        marklogic = Just "ML: https://github.com/quasar-analytics/quasar/issues/2341"
        })
    do
    Interact.insertOpenCardInLastDeck
    selectFile
    Interact.accessNextCardInFirstDeck
    Interact.insertSearchCardInLastDeck
    Interact.provideSearchStringInLastSearchCard "springfield"
    Interact.accessNextCardInFirstDeck
    Interact.insertPivotCard
    Interact.addColumn "city"
    Interact.addColumn "pop"
    Interact.accessNextCardInLastDeck
    Interact.insertChartCardInLastDeck
    Expect.cellsInTableColumnInLastCardToContain 25 "city" "SPRINGFIELD"
    successMsg "** Successfully searched for a city in zips **"

  searchScenario "Search within results"
    (noIssues { couchbase = Just "CB: https://github.com/quasar-analytics/quasar/issues/2341"
              , marklogic = Just "ML: https://github.com/quasar-analytics/quasar/issues/2341"
              })
    do
    Interact.insertOpenCardInLastDeck
    selectFile
    Interact.accessNextCardInFirstDeck
    Interact.insertSearchCardInLastDeck
    Interact.provideSearchStringInLastSearchCard "springfield"
    Interact.accessNextCardInLastDeck
    Interact.insertSearchCardInLastDeck
    Interact.provideSearchStringInLastSearchCard "OR"
    Interact.accessNextCardInLastDeck
    Interact.insertPivotCard
    Interact.addColumn "city"
    Interact.addColumn "state"
    Interact.accessNextCardInLastDeck
    Interact.insertChartCardInLastDeck
    Expect.cellsInTableColumnInLastCardToContain 2 "city" "SPRINGFIELD"
    Expect.cellsInTableColumnInLastCardToContain 2 "state" "OR"
    successMsg "** Successfully searched within results **"

  searchScenario "Search with field names"
    (noIssues
      { mongo = Just "MD: https://github.com/slamdata/slamdata/issues/2096"
      , marklogic = Just "ML: https://github.com/quasar-analytics/quasar/issues/2341"
      , couchbase = Just "CB: https://github.com/quasar-analytics/quasar/issues/2341"
      })
    do
    Interact.insertOpenCardInLastDeck
    selectFile
    Interact.accessNextCardInFirstDeck
    Interact.insertSearchCardInLastDeck
    Interact.provideSearchStringInLastSearchCard
          "city:springfield state:or pop:>20000"
    Interact.accessNextCardInLastDeck
    Interact.insertPivotCard
    Interact.addColumn "city"
    Interact.addColumn "state"
    Interact.addColumn "pop"
    Interact.accessNextCardInLastDeck
    Interact.insertChartCardInLastDeck
    Expect.cellsInTableColumnInLastCardToContain 1 "city" "SPRINGFIELD"
    Expect.cellsInTableColumnInLastCardToContain 1 "state" "OR"
    -- couchbase returns slightly different results which needs to be fixed
    case connector of
      Marklogic → Expect.cellsInTableColumnInLastCardToBeGT 1 "pop" "30000"
      Mongo → Expect.cellsInTableColumnInLastCardToBeGT 1 "pop" "30000"
      Couchbase → Expect.cellsInTableColumnInLastCardToBeGT 1 "pop" "27500"
    successMsg "** Successfully searched with field names **"

  searchScenario "Suppress search results"
    (noIssues
      { mongo = Just "MD: https://github.com/slamdata/slamdata/issues/2096"
      , marklogic = Just "ML: https://github.com/quasar-analytics/quasar/issues/2341"
      , couchbase = Just "CB: results fluxuate need to look if same issue as marklogic"
      })
    do
    Interact.insertOpenCardInLastDeck
    selectFile
    Interact.accessNextCardInFirstDeck
    Interact.insertSearchCardInLastDeck
    Interact.provideSearchStringInLastSearchCard "city:portland -state:OR"
    Interact.accessNextCardInLastDeck
    Interact.insertPivotCard
    Interact.addColumn "city"
    Interact.addColumn "state"
    Expect.columnHeaderInSetupPivotTableCard "city"
    Expect.columnHeaderInSetupPivotTableCard "state"
    Interact.accessNextCardInLastDeck
    Interact.insertChartCardInLastDeck
    Expect.cellsInTableColumnInLastCardToContain 10 "city" "PORTLAND"
    Expect.cellsInTableColumnInLastCardToNotEq 10 "state" "OR"
    successMsg "** Successfully suppressed search results **"
