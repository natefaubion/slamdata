module Test.SlamData.Feature.Test.FlipDeck where

import Data.Time.Duration (Milliseconds(..))
import Selenium.Monad (later, sequence)
import SlamData.Prelude hiding (sequence)
import Test.Feature.ActionSequence as Actions
import Test.Feature.Log (successMsg, warnMsg)
import Test.Feature.Scenario (KnownIssues, allIssue, scenario)
import Test.SlamData.Feature.Expectations as Expect
import Test.SlamData.Feature.Interactions as Interact
import Test.SlamData.Feature.Monad (SlamFeature, getConnector, isMarklogic)

flipDeckScenario ∷ String → KnownIssues → SlamFeature Unit → SlamFeature Unit
flipDeckScenario scenarioName knownIssues implementation = do
  connector ← getConnector
  scenario
    { epic: "Deck flipside"
    , before: Interact.createWorkspaceInTestFolder "Flipped deck"
    , after: Interact.deleteFileInTestFolder "Flipped deck.slam"
    , title: scenarioName
    , knownIssues
    , connector
    }
    implementation

mkDeckWithLastTable ∷ SlamFeature Unit
mkDeckWithLastTable = do
    Interact.insertQueryCardInFirstDeck
    Interact.provideQueryInLastQueryCard $
      "select measureOne, measureTwo from `/test-mount/testDb/flatViz`"
    Interact.runQuery
    Interact.accessNextCardInFirstDeck
    Interact.selectBuildChart
    Interact.insertPivotCard
    Interact.addColumn "measureOne"
    Interact.addColumn "measureTwo"
    Interact.accessNextCardInLastDeck
    Interact.insertChartCardInLastDeck
    Expect.tableColumnsAre ["measureOne", "measureTwo"]
    successMsg "Ok, can see both measures!"

test ∷ SlamFeature Unit
test = do
  isMarklogic' ← isMarklogic
  flipDeckScenario "Flip deck"
    (allIssue "ALL: MeasureTwo not showing on Travis!")
    do
    mkDeckWithLastTable
    Interact.flipDeck
    Expect.flipsideMenuPresented
    Interact.flipDeck
    Expect.flipsideMenuNotPresented
    Expect.tableColumnsAre ["measureOne", "measureTwo"]
    successMsg "Ok, 'flip deck' button works"

  -- Note: Trash button deletes last or active card
  flipDeckScenario "Trash last card"
    (allIssue "ALL: MeasureTwo not showing on Travis!")
    do
    mkDeckWithLastTable
    Interact.flipDeck
    Expect.flipsideMenuPresented
    Interact.trashActiveOrLastCard
    -- Note, user should see that last|active card has been deleted
    -- That's why we immediately flip deck after trashing
    Expect.flipsideMenuNotPresented
    Expect.noTablesPresented
    successMsg "Successfuly deleted last|active card"

  flipDeckScenario "Filter flipside buttons"
    (allIssue "ALL: MeasureTwo not showing on Travis!")
    do
    mkDeckWithLastTable
    Interact.flipDeck
    Expect.flipsideMenuPresented
    Interact.filterDeckAndCardActions "delete c"
    Expect.onlyTrashActionPresented
    sequence $ Actions.sendBackspaces 8
    Expect.flipsideMenuPresented
    sequence $ Actions.sendBackspaces 8
    Expect.flipsideMenuPresented
    Interact.filterDeckAndCardActions "emb"
    Expect.onlyEmbedActionPresented
    sequence $ Actions.sendBackspaces 5
    Expect.flipsideMenuPresented
    Interact.filterDeckAndCardActions "ub"
    Expect.onlyPublishActionPresented
    sequence $ Actions.sendBackspaces 5
    successMsg "Successfully filtered flipside actions"

  flipDeckScenario "Share deck"
    (allIssue "ALL: MeasureTwo not showing on Travis!")
    do
    Interact.insertMdCardInFirstDeck
    Interact.provideMdInLastMdCard "Quarterly"
    Interact.runQuery
    Interact.accessNextCardInFirstDeck
    Interact.insertDisplayMarkdownCardInLastDeck
    Expect.textInDisplayMarkdownCard "Quarterly"
    warnMsg "https://github.com/slamdata/slamdata/issues/1077, we don't know if workspace has been saved already"
    later (Milliseconds 1000.0) $ pure unit
    Interact.flipDeck
    Expect.flipsideMenuPresented
    Interact.publishDeck
    Interact.accessPublishingUrl
    Expect.textInDisplayMarkdownCard "Quarterly"
    Interact.launchSlamData
    successMsg "Successfully shared deck"
