module Test.SlamData.Feature.Interactions.Cards where

import SlamData.Prelude

import Data.Array as Arr
import Data.String as S
import Test.Feature as Feature
import Test.Feature.Log (annotate)
import Test.SlamData.Feature.Expectations.Cards as Expect
import Test.SlamData.Feature.Monad (SlamFeature)
import Test.SlamData.Feature.XPaths as XPaths
import Utils.Array (enumerate)
import XPath as XPath

type ApiVarName = String
type ApiVarType = String
type ApiVarValue = String

addColumn ∷ String → SlamFeature Unit
addColumn str =
  annotate ("Added column " <> str ) do
    Feature.click $ XPath.last $ XPath.anywhere $ XPath.anyWithExactAriaLabel "Add column"
    Feature.click $ XPath.last $ XPath.anywhere $ XPath.anyWithExactAriaLabel $ "Select " ⊕ str
    Feature.click $ XPath.last $ XPath.anywhere $ XPath.anyWithExactText "Confirm"
    Expect.addColumnIsPresent

checkField ∷ String → SlamFeature Unit
checkField labelText =
  annotate ("Checked field" <> labelText)
    $ Feature.check
      $ XPath.anywhere
      $ "input" `XPath.withLabelWithExactText` labelText

checkFieldInLastDeck ∷ String → SlamFeature Unit
checkFieldInLastDeck labelText =
  annotate ("Checked field" <> labelText)
    $ Feature.check
      $ XPaths.followingLastPreviousCardGripper
      $ "input" `XPath.withLabelWithExactText` labelText

doSaveInLastCacheCard ∷ SlamFeature Unit
doSaveInLastCacheCard =
  annotate "Saved Cache card"
    $ Feature.click (XPath.last $ XPath.anywhere XPaths.saveSubmitButton)

expandNewCardMenu ∷ SlamFeature Unit
expandNewCardMenu = Feature.click (XPath.anywhere XPaths.insertCard)

filterNextActions ∷ String → SlamFeature Unit
filterNextActions =
  Feature.provideFieldValue (XPath.anywhere $ XPath.anyWithExactAriaLabel "Filter next actions")

provideApiVariableBindingsForVariablesCard
  ∷ ApiVarName
  → ApiVarType
  → ApiVarValue
  → SlamFeature Unit
provideApiVariableBindingsForVariablesCard name ty val =
  annotate ("Provided the variables "
            <> name <> " "
            <> ty <> " "
            <> val <> " for the Variable card") do
    provideValueForVariablesCard
    provideTypeForVariablesCard
    provideDefaultValueForVariablesCard
    where
    provideValueForVariablesCard ∷ SlamFeature Unit
    provideValueForVariablesCard = do
      Feature.provideFieldValue
        (XPath.first $ XPath.anywhere $ XPaths.variablesCardVariableName)
        name
      Feature.pressEnter
    provideTypeForVariablesCard ∷ SlamFeature Unit
    provideTypeForVariablesCard = do
      Feature.selectFromDropdown
        (XPath.first $ XPath.anywhere $ XPaths.variablesCardVariableTypeFor name)
        ty
      Feature.pressEnter

    provideDefaultValueForVariablesCard ∷ SlamFeature Unit
    provideDefaultValueForVariablesCard = do
      Feature.provideFieldValue
        (XPath.first $ XPath.anywhere $ XPaths.variablesCardDefaultValueFor name)
        val
      Feature.pressEnter

provideFieldValueInLastDeck ∷ String → String → SlamFeature Unit
provideFieldValueInLastDeck labelText text =
  annotate ("Provided Field value "
            <> text <> " for label "
            <> labelText)
    $ (Feature.provideFieldValue
      $ XPaths.followingLastPreviousCardGripper
      $ "input" `XPath.withLabelWithExactText` labelText) text

provideMdInLastMdCard ∷ String → SlamFeature Unit
provideMdInLastMdCard query =
  annotate "Provided a query in Markdown card"
    $ (Feature.provideFieldValue
        $ XPath.last $ XPath.anywhere XPaths.aceEditor) query

provideQueryInLastQueryCard ∷ String → SlamFeature Unit
provideQueryInLastQueryCard query =
  annotate "Provided a query in Query card"
    $ (Feature.provideFieldValue $ XPath.last $ XPath.anywhere XPaths.aceEditor) query

provideSaveDestinationInLastCacheCard ∷ String → SlamFeature Unit
provideSaveDestinationInLastCacheCard destination =
  annotate ("Provided save location for Cache card of " <> destination)
    $ (Feature.provideFieldValue (XPath.last $ XPath.anywhere XPaths.saveDestinationInput)) destination

provideSearchStringInLastSearchCard ∷ String → SlamFeature Unit
provideSearchStringInLastSearchCard string =
  annotate ("Provided the search string value " <> string)
    $ (Feature.provideFieldValue $ XPath.last $ XPath.anywhere XPaths.searchStringInput) string

pushRadioButtonInLastDeck ∷ String → SlamFeature Unit
pushRadioButtonInLastDeck labelText =
  Feature.pushRadioButton
    $ XPaths.followingLastPreviousCardGripper
    $ "input" `XPath.withLabelWithExactText` labelText

runQuery ∷ SlamFeature Unit
runQuery =
  annotate "Ran the query"
    $ Feature.click $ XPath.last $ XPath.anywhere $ XPath.anyWithExactAriaLabel "Run query"

selectFileForLastOpenCard ∷ String → SlamFeature Unit
selectFileForLastOpenCard s =
  annotate ("Selected " <> s)
    $ selectInMillerColumns $ pure "Filesystem" <> S.split (S.Pattern "/") s

selectFromDropdownInLastDeck ∷ String → String → SlamFeature Unit
selectFromDropdownInLastDeck labelText value =
  annotate ("Selected dropdown value " <> value)
    $ (Feature.selectFromDropdown
      $ XPaths.followingLastPreviousCardGripper
      $ "select" `XPath.withLabelWithExactText` labelText) value

selectInMillerColumns ∷ Array String → SlamFeature Unit
selectInMillerColumns ps = do
  for_ (enumerate $ Arr.filter (\s → S.length s > 0) ps) \(ix × path) →
    Feature.click $ resourceXPath (ix + one) path
  where
  ariaLabel ∷ String → String
  ariaLabel rPath = "Select " ⊕ rPath

  resourceXPath ∷ Int → String → String
  resourceXPath ix rPath =
    XPath.last
      $ XPath.anywhere
      $ (XPath.nodeAtPosition ix $ XPath.anyWithExactAriaLabel "Column")
      ⊕ XPath.descendantString
      ⊕ (XPath.anyWithExactAriaLabel $ ariaLabel rPath)

trashActiveOrLastCard ∷ SlamFeature Unit
trashActiveOrLastCard =
  annotate "Trashed card"
    $ Feature.click $ XPath.anywhere $ XPath.anyWithExactAriaLabel "Delete card"

uncheckFieldInLastDeck ∷ String → SlamFeature Unit
uncheckFieldInLastDeck labelText =
  annotate ("Unchecked field" <> labelText)
    $ Feature.uncheck
      $ XPaths.followingLastPreviousCardGripper
      $ "input" `XPath.withLabelWithExactText` labelText
