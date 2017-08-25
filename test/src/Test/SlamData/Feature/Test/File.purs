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

module Test.SlamData.Feature.Test.File where

import SlamData.Prelude

import Test.Feature.Log (successMsg)
import Test.Feature.Scenario (KnownIssues, noIssues, scenario)
import Test.SlamData.Feature.Expectations as Expect
import Test.SlamData.Feature.Interactions as Interact
import Test.SlamData.Feature.Monad (SlamFeature, getConnector)

fileScenario
  ∷ SlamFeature Unit
   → String
   → KnownIssues
   → SlamFeature Unit
   → SlamFeature Unit
fileScenario after scenarioName knownIssues implementation = do
  connector <- getConnector
  scenario
    { epic: "File"
    , before: (pure unit)
    , after: after
    , title: scenarioName
    , knownIssues
    , connector
    }
    implementation

defaultAfterFile ∷ SlamFeature Unit
defaultAfterFile = Interact.browseRootFolder

afterRename ∷ SlamFeature Unit
afterRename = Interact.deleteFileInTestFolder "Ϡ⨁⟶≣ΜϞ"

afterMove ∷ SlamFeature Unit
afterMove = Interact.deleteFileInTestFolder "Medical data"

afterUpload ∷ SlamFeature Unit
afterUpload = Interact.deleteFileInTestFolder "array-wrapped.json"

afterCreateWorkspace ∷ SlamFeature Unit
afterCreateWorkspace = Interact.deleteFileInTestFolder "Untitled Workspace.slam"

afterAccessSharingUrl ∷ SlamFeature Unit
afterAccessSharingUrl =
  Interact.launchSlamData
    *> Interact.browseTestFolder
    *> Interact.deleteFile "Untitled Workspace.slam"

test ∷ SlamFeature Unit
test = do
  connector ← getConnector
  fileScenario afterRename "Rename a folder" noIssues do
    Interact.createFolder
    Interact.renameFile "Untitled Folder" "Patients"
    Expect.file "Patients"
    Interact.renameFile "Patients" "Пациенты# #"
    Expect.file "Пациенты# #"
    Interact.renameFile "Пациенты# #" "Ϡ⨁⟶≣ΜϞ"
    Expect.file "Ϡ⨁⟶≣ΜϞ"
    successMsg "** Successfully ran Renamed a folder test **"

  fileScenario afterMove "Move a folder" noIssues do
    Interact.createFolder
    Interact.renameFile "Untitled Folder" "Medical data"
    Interact.createFolder
    Interact.moveFile
      "Untitled Folder"
      "/test-mount/testDb/"
      "/test-mount/testDb/Medical data/"
    Interact.accessFile "Medical data"
    Expect.file "Untitled Folder"
    successMsg "** Successfully ran Moved a folder test **"

  fileScenario defaultAfterFile "Delete a folder" noIssues do
    Interact.createFolder
    Interact.deleteFile "Untitled Folder"
    Expect.noFile "Untitled Folder"
    Interact.showHiddenFiles
    Interact.accessFile ".trash"
    Expect.file "Untitled Folder"
    Interact.hideHiddenFiles
    Expect.noFile ".trash"
    successMsg "** Successfully deleted a folder **"

  fileScenario defaultAfterFile "Navigate back using breadcrumbs" noIssues do
    Interact.browseTestFolder
    Interact.accessBreadcrumb "test-mount"
    Interact.accessBreadcrumb "Home"
    Expect.file "test-mount"
    successMsg "** Successfully navigated back using breadcrumbs **"

  fileScenario defaultAfterFile "Search for a file"
    (noIssues { couchbase = Just "CB: https://github.com/quasar-analytics/quasar/issues/2396" })
    do
    Interact.browseTestFolder
    Interact.provideFileSearchString "smallZ"
    Expect.fileSearchString "+smallZ"
    Expect.file "/test-mount/testDb/smallZips"
    Expect.numberOfFiles 1
    successMsg "** Succesfully searched for a file **"

  fileScenario defaultAfterFile "Access sharing URL for a file" noIssues do
    Interact.browseTestFolder
    Interact.shareFile "smallZips"
    Interact.accessSharingUrl
    Expect.tableColumnsAre ["city", "loc", "pop", "state"]
    successMsg "** Successfully accessed sharing URL for a file **"
    -- TODO: figure out how to move downloaded by chrome files to specific folders
-- and omit prompt
--  fileScenario defaultAfterFile "Download file as CSV" [] do
--    Interact.browseTestFolder
--    Interact.downloadFileAsCSV "smallZips"
--    Expect.downloadedTextFileToMatchFile
--      "tmp/test/downloads"
--      "smallZips"
--      "test/smallZips.csv"
--    annotate "Successfully downloaded file as CSV"

--  fileScenario defaultAfterFile "Download file as JSON" [] do
--    Interact.browseTestFolder
--    Interact.downloadFileAsJSON "smallZips"
--    Expect.downloadedTextFileToMatchFile
--      "tmp/test/downloads"
--      "smallZips"
--      "test/smallZips.json"
--    annotate "Successfully downloaded file as JSON"
