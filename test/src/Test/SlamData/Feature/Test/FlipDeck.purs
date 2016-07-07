module Test.SlamData.Feature.Test.FlipDeck where

import SlamData.Prelude (Unit, ($), bind, unit, pure)

import Selenium.Monad (later, sequence)
import Test.Feature.Log (successMsg, warnMsg)
import Test.Feature.Scenario (scenario)
import Test.SlamData.Feature.Expectations as Expect
import Test.SlamData.Feature.Interactions as Interact
import Test.SlamData.Feature.Monad (SlamFeature)
import Test.Feature.ActionSequence as Actions

flipDeckScenario ∷ String → Array String → SlamFeature Unit → SlamFeature Unit
flipDeckScenario =
  scenario
    "Deck backside"
    (Interact.createWorkspaceInTestFolder "Flipped deck")
    (Interact.deleteFileInTestFolder "Untitled Workspace.slam")


mkTwoCardTestDeck ∷ SlamFeature Unit
mkTwoCardTestDeck = do
    Interact.insertQueryCardInLastDeck
    Interact.provideQueryInLastQueryCard
      "select measureOne from `/test-mount/testDb/flatViz`"
    Interact.accessNextCardInLastDeck
    Interact.insertTableCardInLastDeck
    Expect.tableColumnsAre ["measureOne"]

test ∷ SlamFeature Unit
test = do
  flipDeckScenario "Flip deck" [] do
    mkTwoCardTestDeck
    Interact.flipDeck
    Expect.backsideMenuPresented
    Interact.flipDeck
    Expect.backsideMenuNotPresented
    Expect.tableColumnsAre ["measureOne"]
    successMsg "Ok, 'flip deck' button works"

  -- Note: Trash button deletes last or active card
  flipDeckScenario "Trash last card" [] do
    mkTwoCardTestDeck
    Interact.flipDeck
    Expect.backsideMenuPresented
    Interact.trashActiveOrLastCard
    -- Note, user should see that last|active card has been deleted
    -- That's why we immediately flip deck after trashing
    Expect.backsideMenuNotPresented
    Expect.noTablesPresented
    successMsg "Successfuly deleted last|active card"

  flipDeckScenario "Share deck" [] do
    Interact.insertMdCardInLastDeck
    Interact.provideMdInLastMdCard "Quarterly"
    Interact.accessNextCardInLastDeck
    Interact.insertDisplayMarkdownCardInLastDeck
    Expect.textInDisplayMarkdownCard "Quarterly"
    warnMsg "SD-1538, we don't know if workspace has been saved already"
    later 1000 $ pure unit
    Interact.flipDeck
    Expect.backsideMenuPresented
    Interact.publishDeck
    Interact.accessPublishingUrl
    Expect.textInDisplayMarkdownCard "Quarterly"
    Interact.launchSlamData
    successMsg "Successfully shared deck"

  flipDeckScenario "Filter backside buttons" [] do
    mkTwoCardTestDeck
    Interact.flipDeck
    Expect.backsideMenuPresented
    Interact.filterDeckAndCardActions "delete c"
    Expect.onlyTrashActionPresented
    sequence $ Actions.sendBackspaces 8
    Expect.backsideMenuPresented
    sequence $ Actions.sendBackspaces 8
    Expect.backsideMenuPresented
    Interact.filterDeckAndCardActions "emb"
    Expect.onlyEmbedActionPresented
    sequence $ Actions.sendBackspaces 5
    Expect.backsideMenuPresented
    Interact.filterDeckAndCardActions "ub"
    Expect.onlyPublishActionPresented
    sequence $ Actions.sendBackspaces 5
    successMsg "Successfully filtered backside actions"
