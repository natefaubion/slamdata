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

module Test.SlamData.Feature.Interactions.ImportExport where

import SlamData.Prelude

import Control.Monad.Aff.Class (liftAff)
import Node.Platform (Platform(..))
import Node.Process (platform)
import Node.Rimraf (rimraf)
import Test.Feature as Feature
import Test.Feature.Log (annotate)
import Test.SlamData.Feature.Interactions.Cards as Cards
import Test.SlamData.Feature.Interactions.FileSystem as FileSystem
import Test.SlamData.Feature.Monad (SlamFeature)
import Test.SlamData.Feature.XPaths as XPaths
import Test.Utils (appendToCwd)
import XPath as XPath

deleteDownloadedFile ∷ String → SlamFeature Unit
deleteDownloadedFile filename =
  annotate ("Deleted " <> filename) do
   liftAff $ rimraf $ filename

downloadFileAsCSV ∷ String → SlamFeature Unit
downloadFileAsCSV fileName =
  annotate ("Downloaded " <> fileName) do
    FileSystem.selectFile fileName
    Feature.click $ XPath.anywhere $ XPaths.downloadFile fileName
    Feature.click $ XPath.anywhere $ XPaths.downloadButton

downloadFileAsCSVZip ∷ String → SlamFeature Unit
downloadFileAsCSVZip fileName =
  annotate ("Downloaded " <> fileName) do
    FileSystem.selectFile fileName
    Feature.click $ XPath.anywhere $ XPaths.downloadFile fileName
    Cards.checkField "Compress as .zip"
    Feature.click $ XPath.anywhere $ XPaths.downloadButton


downloadFileAsJSON ∷ String → SlamFeature Unit
downloadFileAsJSON fileName =
  annotate ("Downloaded " <> fileName) do
    FileSystem.selectFile fileName
    Feature.click $ XPath.anywhere $ XPaths.downloadFile fileName
    Feature.click $ XPath.anywhere $ XPath.anyWithText "JSON"
    Feature.click $ XPath.anywhere $ XPaths.downloadButton

downloadFileAsJSONZip ∷ String → SlamFeature Unit
downloadFileAsJSONZip fileName =
  annotate ("Downloaded " <> fileName) do
    FileSystem.selectFile fileName
    Feature.click $ XPath.anywhere $ XPaths.downloadFile fileName
    Feature.click $ XPath.anywhere $ XPath.anyWithText "JSON"
    Cards.checkField "Compress as .zip"
    Feature.click $ XPath.anywhere $ XPaths.downloadButton

fileLocation ∷ { user :: String, name :: String, ending :: String } → String
fileLocation {user, name, ending } = do
    case platform of
      Win32 → "\\Users\\" <> user <> "\\Downloads\\" <> name <> ending
      Darwin → "/Users/" <> user <> "/Downloads/" <> name <> ending
      _ → "/home/" <> user <> "/Downloads/" <> name <> ending

uploadFile ∷ String → SlamFeature Unit
uploadFile location =
  annotate ("Uploaded " <> location) do
    Feature.provideFileInputValue (XPath.anywhere $ XPaths.uploadFile) location

uploadLocalFile ∷ String → SlamFeature Unit
uploadLocalFile name =
  annotate ("Uploaded " <> name) do
    (Feature.provideFileInputValue (XPath.anywhere $ XPaths.uploadFile)
      <=< appendToCwd) name
