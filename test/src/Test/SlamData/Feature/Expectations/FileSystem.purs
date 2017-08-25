module Test.SlamData.Feature.Expectations.FileSystem where

import SlamData.Prelude

import Data.Map as Map
import Test.Feature (expectDownloadedTextFileToMatchFile, expectNotPresented, expectPresented, expectPresentedNotRepeatedly, expectPresentedWithProperties)
import Test.Feature.Log (annotate)
import Test.SlamData.Feature.Monad (SlamFeature)
import Test.SlamData.Feature.XPaths as XPaths
import XPath as XPath

downloadedTextFileToMatchFile ∷ String → String → String → SlamFeature Unit
downloadedTextFileToMatchFile = expectDownloadedTextFileToMatchFile

downloadedFileName ∷ String → SlamFeature Unit
downloadedFileName s = expectPresented $ XPath.anywhere $ XPath.withTextContaining s

file ∷ String → SlamFeature Unit
file name =
  annotate ("Found expexcted file " <> name)
    $ (expectPresented ∘ XPath.anywhere ∘ XPaths.selectFile) name

fileNotRepeatedly ∷ String → SlamFeature Unit
fileNotRepeatedly =
  expectPresentedNotRepeatedly ∘ XPath.anywhere ∘ XPaths.selectFile

fileSearchString ∷ String → SlamFeature Unit
fileSearchString string =
  annotate ("Found expexcted string " <> string)
    $ expectPresentedWithProperties
      (Map.singleton "value" $ Just string)
      (XPath.anywhere XPaths.fileSearchInput)

noFile ∷ String → SlamFeature Unit
noFile name =
  annotate (name <> " was expectedly not found")
    $ (expectNotPresented ∘ XPath.anywhere ∘ XPaths.selectFile) name

numberOfFiles ∷ Int → SlamFeature Unit
numberOfFiles i =
  annotate ("Expected the right number of files to be: " <> show i) do
    expectPresented $ XPath.index (XPath.anywhere $ XPaths.nthFile) i
    expectNotPresented $ XPath.index (XPath.anywhere $ XPaths.nthFile) $ i + 1
