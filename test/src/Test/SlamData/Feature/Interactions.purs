module Test.SlamData.Feature.Interactions
       ( module Cards
       , module Chart
       , module Decks
       , module FS
       , module HNG
       , module IE
       , launchSlamData
       )
       where

import Test.SlamData.Feature.Interactions.Cards
       ( addColumn
       , checkField
       , checkFieldInLastDeck
       , doSaveInLastCacheCard
       , expandNewCardMenu
       , filterNextActions
       , provideApiVariableBindingsForVariablesCard
       , provideFieldValueInLastDeck
       , provideMdInLastMdCard
       , provideQueryInLastQueryCard
       , provideSaveDestinationInLastCacheCard
       , provideSearchStringInLastSearchCard
       , pushRadioButtonInLastDeck
       , runQuery
       , selectFileForLastOpenCard
       , selectFromDropdownInLastDeck
       , selectInMillerColumns
       , trashActiveOrLastCard
       , uncheckFieldInLastDeck
       ) as Cards

import Test.SlamData.Feature.Interactions.Chart
       ( selectInChartBuilder
       , activateCategoryForChartBuilder
       , activateMeasureForChartBuilder
       , activateStackForChartBuilder
       ) as Chart

import Test.SlamData.Feature.Interactions.Deck
       ( accessNextCardInFirstDeck
       , accessNextCardInLastDeck
       , accessPreviousCardInLastDeck
       , accessPublishingUrl
       , accessWorkspaceWithModifiedURL
       , confirmDeckAction
       , filterDeckAndCardActions
       , flipDeck
       , insertBuildBarChartCard
       , insertCacheCardInLastDeck
       , insertChartCardInLastDeck
       , insertChartOptionsCardInLastDeck
       , insertDisplayMarkdownCardInLastDeck
       , insertMdCardInFirstDeck
       , insertMdCardInLastDeck
       , insertOpenCardInLastDeck
       , insertPivotCard
       , insertQueryCardInFirstDeck
       , insertQueryCardInLastDeck
       , insertSearchCardInLastDeck
       , insertTableCardInLastDeck
       , insertTroubleshootCardInLastDeck
       , insertVariablesCardInFirstDeck
       , insertVariablesCardInLastDeck
       , nameDeck
       , publishDeck
       , reopenCurrentWorkspace
       , setVarMapForDeck
       , shareDeck
       ) as Decks

import Test.SlamData.Feature.Interactions.FileSystem
       ( accessBreadcrumb
       , accessSharingUrl
       , accessFile
       , browseRootFolder
       , browseTestFolder
       , createFolder
       , createWorkspace
       , createWorkspaceInTestFolder
       , deleteFile
       , deleteFileInTestFolder
       , editWorkspace
       , hideHiddenFiles
       , mountTestDatabase
       , moveFile
       , moveFileWithClick
       , provideFileSearchString
       , renameFile
       , selectFile
       , setupCouchbase
       , setupMarklogic
       , shareFile
       , showHiddenFiles
       ) as FS

import Test.SlamData.Feature.Interactions.ImportExport
       ( deleteDownloadedFile
       , downloadFileAsCSV
       , downloadFileAsCSVZip
       , downloadFileAsJSON
       , downloadFileAsJSONZip
       , fileLocation
       , uploadFile
       , uploadLocalFile
       ) as IE

import Test.SlamData.Feature.Interactions.HintsAndGuides
       ( dismissHint
       , skipGuide
       ) as HNG

import SlamData.Prelude

import Selenium.Monad (get)
import Test.SlamData.Feature.Monad (SlamFeature, getConfig)

launchSlamData ∷ SlamFeature Unit
launchSlamData = get ∘ _.slamdataUrl =<< getConfig
