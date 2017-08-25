module Test.SlamData.Feature.Expectations
       ( module Cards
       , module Charts
       , module FS
       , module FPS
       , module Text
       , module WS
       )
       where

import Test.SlamData.Feature.Expectations.Cards
       ( cellsInTableColumnInLastCardToEq
       , cellsInTableColumnInLastCardToContain
       , cellsInTableColumnInLastCardToNotEq
       , cellsInTableColumnInLastCardToBeGT
       , cellsInTableColumnInLastCardToBeLT
       , cellsInTableColumnInLastCardToEqOneOf
       , cellsInTableColumnInLastCardToNotEqOneOf
       , cellsInTableColumnInLastCard
       , categoryEnabledInLastBuildChartCard
       , checkableFieldInLastMdCard
       , columnHeaderInSetupPivotTableCard
       , displayMarkdownCardPresented
       , dropdownInLastMdCard
       , fieldInLastMdCard
       , addColumnIsPresent
       , logoIsPresent
       , labelInLastMdCard
       , measureDisabledInLastChartCard
       , measureInLastChartCard
       , noTablesPresented
       , resourceOpenedInLastOpenCard
       , tableCardPresented
       , tableColumnsAre
       , textInDisplayMarkdownCard
       , troubleshootCardPresented
       ) as Cards

import Test.SlamData.Feature.Expectations.Charts
       (lastEChart) as Charts

import Test.SlamData.Feature.Expectations.FileSystem
       ( downloadedTextFileToMatchFile
       , downloadedFileName
       , file
       , fileNotRepeatedly
       , fileSearchString
       , noFile
       , numberOfFiles
       ) as FS

import Test.SlamData.Feature.Expectations.Flipside
       ( flipsideActionNotPresented
       , flipsideMenuPresented
       , flipsideMenuNotPresented
       , embedButtonPresented
       , onlyEmbedActionPresented
       , onlyPublishActionPresented
       , onlyTrashActionPresented
       , publishButtonPresented
       , shareButtonPresented
       , trashButtonPresented
       ) as FPS

import Test.SlamData.Feature.Expectations.Text
       ( text
       , textEventually) as Text

import Test.SlamData.Feature.Expectations.Workspace
       (workspaceName) as WS
