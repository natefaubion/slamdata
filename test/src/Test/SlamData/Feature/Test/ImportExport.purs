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

module Test.SlamData.Feature.Test.ImportExport where

import SlamData.Prelude

import Test.Feature.Log (successMsg)
import Test.Feature.Scenario (KnownIssues, noIssues, scenario)
import Test.SlamData.Feature.Expectations as Expect
import Test.SlamData.Feature.Interactions as Interact
import Test.SlamData.Feature.Monad (SlamFeature, getConfig, getConnector)


importExportScenario
  ∷ SlamFeature Unit
  → String
  → KnownIssues
  → SlamFeature Unit
  → SlamFeature Unit
importExportScenario after scenarioName knownIssues implementation = do
  connector ← getConnector
  scenario
    { epic: "Import Export"
    , before: (pure unit)
    , after: after
    , title: scenarioName
    , knownIssues
    , connector
    }
    implementation

afterUpload ∷ String → SlamFeature Unit
afterUpload s = Interact.deleteFileInTestFolder s

tests ∷ SlamFeature Unit
tests = do
  connector ← getConnector
  { whoami } ← getConfig

  importExportScenario (pure unit) "Export and import .csv file"
    (noIssues)
    do
    let file = "smallZips"
    let fileLoc = {user: whoami, name: file, ending: ".csv"}
    let fullFile = (file <> ".csv")
    Interact.browseTestFolder
    Interact.downloadFileAsCSV file
    Interact.browseTestFolder
    Interact.uploadFile $ Interact.fileLocation fileLoc
    Expect.file fullFile
    Interact.deleteDownloadedFile $ Interact.fileLocation fileLoc
    Interact.accessFile fullFile
    Expect.tableColumnsAre ["city", "loc", "pop", "state"]
    afterUpload fullFile
    successMsg "** Successfully exported and imported .csv file **"

  importExportScenario (pure unit) "Export and import .csv file as zip"
    (noIssues {
        couchbase = Just "CB: https://github.com/quasar-analytics/quasar/issues/2652",
        marklogic = Just "ML: https://github.com/quasar-analytics/quasar/issues/2652",
        mongo = Just "MD: https://github.com/quasar-analytics/quasar/issues/2652"
        })
    do
    let file = "smallZips"
    let fileLoc = {user: whoami, name: file, ending: ".zip"}
    let fullFile = (file <> ".zip")
    Interact.browseTestFolder
    Interact.downloadFileAsCSVZip file
    Interact.browseTestFolder
    Interact.uploadFile $ Interact.fileLocation fileLoc
    Expect.file fullFile
    Interact.deleteDownloadedFile $ Interact.fileLocation fileLoc
    Interact.accessFile fullFile
    Expect.tableColumnsAre ["city", "loc", "pop", "state"]
    afterUpload fullFile
    successMsg "** Successfully exported and imported .csv file as zip **"

  importExportScenario (pure unit) "Export and import .json file"
    (noIssues)
    do
    let file = "smallZips"
    let fileLoc = {user: whoami, name: file, ending: ".json"}
    let fullFile = (file <> ".json")
    Interact.browseTestFolder
    Interact.downloadFileAsJSON file
    Interact.browseTestFolder
    Interact.uploadFile $ Interact.fileLocation fileLoc
    Expect.file fullFile
    Interact.deleteDownloadedFile $ Interact.fileLocation fileLoc
    Interact.accessFile fullFile
    Expect.tableColumnsAre ["city", "loc", "pop", "state"]
    Interact.browseTestFolder
    afterUpload fullFile
    successMsg "** Successfully exported and imported .json file **"

  importExportScenario (pure unit) "Export and import .json file as zip"
    (noIssues {
        couchbase = Just "CB: https://github.com/quasar-analytics/quasar/issues/2652",
        marklogic = Just "ML: https://github.com/quasar-analytics/quasar/issues/2652",
        mongo = Just "MD: https://github.com/quasar-analytics/quasar/issues/2652"
        })
    do
    let file = "smallZips"
    let fileLoc = {user: whoami, name: file, ending: ".zip"}
    let fullFile = (file <> ".zip")
    Interact.browseTestFolder
    Interact.downloadFileAsJSONZip file
    Interact.browseTestFolder
    Interact.uploadFile $ Interact.fileLocation fileLoc
    Expect.file fullFile
    Interact.deleteDownloadedFile $ Interact.fileLocation fileLoc
    Interact.accessFile fullFile
    Expect.tableColumnsAre ["city", "loc", "pop", "state"]
    Interact.browseTestFolder
    afterUpload fullFile
    successMsg "** Successfully exported and imported .json file as zip **"
