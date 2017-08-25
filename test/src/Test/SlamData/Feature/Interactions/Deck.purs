module Test.SlamData.Feature.Interactions.Deck where

import SlamData.Prelude

import Data.Argonaut (encodeJson)
import Data.StrMap as SM
import Data.Time.Duration (Milliseconds(..))
import Global (encodeURIComponent)
import Selenium.Monad (get, getCurrentUrl, refresh)
import Test.Feature as Feature
import Test.Feature.Log (annotate)
import Test.SlamData.Feature.Monad (SlamFeature, waitTime)
import Test.SlamData.Feature.XPaths as XPaths
import Utils (prettyJson)
import XPath as XPath

accessNextCardInLastDeck ∷ SlamFeature Unit
accessNextCardInLastDeck =
  Feature.dragAndDrop
    (XPath.last $ XPath.anywhere $ XPaths.enabledNextCardGripper)
    (XPath.last $ XPath.anywhere $ XPaths.previousCardGripper)

accessNextCardInFirstDeck ∷ SlamFeature Unit
accessNextCardInFirstDeck =
  annotate "Navigated to next card in deck"
    $ Feature.click $ XPath.anywhere $ XPaths.nextCardGripper

accessPreviousCardInLastDeck ∷ SlamFeature Unit
accessPreviousCardInLastDeck =
  annotate "Navigated to previous card in deck"
    $ Feature.dragAndDrop
        (XPath.last $ XPath.anywhere $ XPaths.enabledPreviousCardGripper)
        (XPath.last $ XPath.anywhere $ XPaths.nextCardGripper)

accessPublishingUrl ∷ SlamFeature Unit
accessPublishingUrl =
  annotate "Accessed Publish URL"
    $ Feature.accessUrlFromFieldValue $ XPath.anywhere XPaths.publishingUrl

accessWorkspaceWithModifiedURL ∷ (String → String) → SlamFeature Unit
accessWorkspaceWithModifiedURL modifier =
  annotate "Accessed Workspace with modified URL"
    $ getCurrentUrl >>= modifier >>> get

confirmDeckAction ∷ SlamFeature Unit
confirmDeckAction =
  annotate "Confirmed deck action"
    $ Feature.click $ XPath.anywhere $ XPath.anyWithExactText "Confirm"

filterDeckAndCardActions ∷ String → SlamFeature Unit
filterDeckAndCardActions value =
  annotate ("filtered flipside menu with " <> value)
    $ (Feature.provideFieldValue
       (XPath.anywhere $ XPath.anyWithExactAriaLabel "Filter deck and card actions")) value

flipDeck ∷ SlamFeature Unit
flipDeck =
  annotate "Clicked on Flip deck icon"
    $ Feature.click $ XPath.anywhere $ XPath.anyWithExactAriaLabel "Flip deck"

insertBuildBarChartCard ∷ SlamFeature Unit
insertBuildBarChartCard = do
  annotate "Insert setup visualization"
    $ Feature.click
    $ XPath.anywhere
    $ XPath.anyWithExactAriaLabel
        "Insert a Setup Visualization card"
  annotate "Drill down to visualization select"
    $ Feature.click
    $ XPath.anywhere
    $ XPath.anyWithExactAriaLabel
        "Select visualization type"
  annotate "Choose bar chart visualization"
    $ Feature.click
    $ XPath.anywhere
    $ XPath.anyWithExactAriaLabel
        "Set visualization type to Bar"

insertCacheCardInLastDeck ∷ SlamFeature Unit
insertCacheCardInLastDeck =
  Feature.click $ XPaths.followingLastPreviousCardGripper XPaths.insertCacheCard

insertChartCardInLastDeck ∷ SlamFeature Unit
insertChartCardInLastDeck =
  annotate "Inserted chart card"
    $ Feature.click $ XPaths.followingLastPreviousCardGripper XPaths.insertChartCard

insertChartOptionsCardInLastDeck ∷ SlamFeature Unit
insertChartOptionsCardInLastDeck =
  annotate "Inserted Chart Options card"
    $ Feature.click $ XPaths.followingLastPreviousCardGripper XPaths.insertChartOptionsCard

insertDisplayMarkdownCardInLastDeck ∷ SlamFeature Unit
insertDisplayMarkdownCardInLastDeck =
  annotate "Inserted Display Markdown card"
    $ Feature.click $ XPath.anywhere $ XPaths.insertDisplayMarkdownCard

insertMdCardInFirstDeck ∷ SlamFeature Unit
insertMdCardInFirstDeck =
  annotate "Inserted Setup Markdown card"
    $ Feature.click $ XPath.anywhere $ XPaths.insertMdCard

insertMdCardInLastDeck ∷ SlamFeature Unit
insertMdCardInLastDeck =
  Feature.click $ XPaths.followingLastPreviousCardGripper XPaths.insertMdCard

insertOpenCardInLastDeck ∷ SlamFeature Unit
insertOpenCardInLastDeck =
  annotate "Inserted Open card as first card in deck"
    $ Feature.click $ XPath.anywhere $ XPaths.insertOpenCard

insertPivotCard ∷ SlamFeature Unit
insertPivotCard = do
  annotate "Insert setup visualization"
    $ Feature.click
    $ XPath.anywhere
    $ XPath.anyWithExactAriaLabel
        "Insert a Setup Visualization card"
  annotate "Drill down to visualization select"
    $ Feature.click
    $ XPath.anywhere
    $ XPath.anyWithExactAriaLabel
        "Select visualization type"
  annotate "Choose Pivot Table visualization"
    $ Feature.click
    $ XPath.anywhere
    $ XPath.anyWithExactAriaLabel
        "Set visualization type to Pivot Table"

insertQueryCardInLastDeck ∷ SlamFeature Unit
insertQueryCardInLastDeck =
  annotate "Inserted Query card as last card"
    $ Feature.click $ XPaths.followingLastPreviousCardGripper XPaths.insertQueryCard

insertQueryCardInFirstDeck ∷ SlamFeature Unit
insertQueryCardInFirstDeck =
  annotate "Inserted Query Card as first card"
    $ Feature.click $ XPath.anywhere $ XPaths.insertQueryCard

insertSearchCardInLastDeck ∷ SlamFeature Unit
insertSearchCardInLastDeck =
  annotate "Inserted Search card"
    $ Feature.click $ XPaths.followingLastPreviousCardGripper XPaths.insertSearchCard

insertTableCardInLastDeck ∷ SlamFeature Unit
insertTableCardInLastDeck =
  Feature.click $ XPaths.followingLastPreviousCardGripper XPaths.insertTableCard

insertTroubleshootCardInLastDeck ∷ SlamFeature Unit
insertTroubleshootCardInLastDeck =
  annotate "Inserted Troubleshoot card"
    $ Feature.click $ XPaths.followingLastPreviousCardGripper XPaths.insertTroubleshootCard

insertVariablesCardInFirstDeck ∷ SlamFeature Unit
insertVariablesCardInFirstDeck =
  annotate "Inserted Variable card"
    $ Feature.click $ XPath.anywhere $ XPaths.insertVariablesCard

insertVariablesCardInLastDeck ∷ SlamFeature Unit
insertVariablesCardInLastDeck =
  Feature.click $ XPaths.followingLastPreviousCardGripper XPaths.insertVariablesCard

nameDeck ∷ String → SlamFeature Unit
nameDeck name = do
  flipDeck
  Feature.click $ XPath.anywhere XPaths.renameDeck
  Feature.provideFieldValue (XPath.anywhere $ XPath.nodeWithExactAriaLabel "input" "Deck name") name
  Feature.click $ XPath.anywhere $ XPath.anyWithExactText "Save"

publishDeck ∷ SlamFeature Unit
publishDeck =
  annotate "Clicked Publish Deck"
    $ Feature.click $ XPath.anywhere $ XPath.anyWithExactAriaLabel "Publish deck"

reopenCurrentWorkspace ∷ SlamFeature Unit
reopenCurrentWorkspace = waitTime (Milliseconds 2000.0) *> refresh

setVarMapForDeck ∷ String → SM.StrMap String → SlamFeature Unit
setVarMapForDeck deckName vm =
  annotate ("Set Variable map for the deck " <> deckName)
    $ accessWorkspaceWithModifiedURL \urlStr →
      let
        varsValue =
          encodeURIComponent
          $ prettyJson
          $ encodeJson
          $ SM.singleton deckName vm
      in
      urlStr ⊕ "/?vars=" ⊕ varsValue

shareDeck ∷ SlamFeature Unit
shareDeck =
  Feature.click $ XPath.anywhere $ XPath.anyWithExactAriaLabel "Share deck"
