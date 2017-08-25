module Test.SlamData.Feature.Interactions.Chart where

import SlamData.Prelude

import Test.Feature as Feature
import Test.Feature.Log (annotate)
import Test.SlamData.Feature.Interactions.Cards (selectInMillerColumns)
import Test.SlamData.Feature.Monad (SlamFeature)
import Test.SlamData.Feature.XPaths as XPaths
import XPath as XPath

selectInChartBuilder ∷ Array String → SlamFeature Unit
selectInChartBuilder ps =
  annotate "Selected in Chart Builder" do
    selectInMillerColumns ps
    Feature.click $ XPath.last $ XPath.anywhere $ XPath.anyWithExactText "Confirm"

activateCategoryForChartBuilder ∷ SlamFeature Unit
activateCategoryForChartBuilder =
  annotate "Activated Category for Chart Builder"
    $ Feature.click $ XPath.last $ XPath.anywhere $ XPaths.chartCategorySelector

activateMeasureForChartBuilder ∷ SlamFeature Unit
activateMeasureForChartBuilder =
  annotate "Activated Measure for Chart Builder"
    $ Feature.click $ XPath.last $ XPath.anywhere $ XPaths.chartMeasureSelector

activateStackForChartBuilder ∷ SlamFeature Unit
activateStackForChartBuilder =
  annotate "Activated Stack for Chart Builder"
    $ Feature.click $ XPath.last $ XPath.anywhere $ XPaths.chartStackSelector
