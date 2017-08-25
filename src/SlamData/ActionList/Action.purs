{-
Copyright 2016 SlamData, Inc.

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

module SlamData.ActionList.Action where

import SlamData.Prelude

import Data.Array as A
import Data.Foldable as F
import Data.Identity (Identity)
import Data.Int as Int
import Data.String.LineWrapping (WrappedLine, MeasuredWord)
import Data.String.LineWrapping as LineWrapping
import Halogen.HTML as HH
import Math as Math
import RectanglePacking (Dimensions, mostRatioFittingRectangle, goldenRatio)
import SlamData.Render.Icon as I
import Utils as Utils
import Utils.DOM as DOMUtils

type NameLine =
  { line ∷ String
  , width ∷ Number
  }

type ActionSizes =
  { dimensions ∷ Array Dimensions
  , leavesASpace ∷ Boolean
  }

type ButtonMetrics =
  { dimensions ∷ Dimensions
  , iconDimensions ∷ Dimensions
  , iconMarginPx ∷ Number
  , iconOnlyLeftPx ∷ Number
  , iconOnlyTopPx ∷ Number
  }

data Presentation
  = IconOnly
  | TextOnly
  | IconAndText


type InputAndActionR r =
  { icon ∷ Maybe I.IconHTML
  , description ∷ String
  | r }

type DoMixin a =
  ( action ∷ a, highlighted ∷ Boolean, disabled ∷ Boolean )

type DrillMixin a =
  ( children ∷ Array (Action a) )

type ActionR r =
  InputAndActionR (words ∷ Array MeasuredWord | r)

type DoR a =
  ActionR (DoMixin a)

type DrillR a =
  ActionR (DrillMixin a)

type MkR r =
  InputAndActionR (name ∷ String | r)

type MkDoR a =
  MkR (DoMixin a)

type MkDrillR a =
  MkR (DrillMixin a)

data Action a
  = Do (DoR a)
  | Drill (DrillR a)
  | GoBack

type ButtonConf a =
  { metrics ∷ ButtonMetrics
  , presentation ∷ Presentation
  , action ∷ Action a
  , lines ∷ Array String
  }

type ActionListConf a =
  { buttons ∷ Array (ButtonConf a)
  , leavesASpace ∷ Boolean
  , classes ∷ Array HH.ClassName
  }


mkDo ∷ ∀ a. MkDoR a → Action a
mkDo {name, icon, description, highlighted, disabled, action} =
  Do
    { words: wordify name
    , icon
    , description
    , highlighted
    , disabled
    , action
    }

mkDrill ∷ ∀ a. MkDrillR a → Action a
mkDrill {name, icon, description, children} =
  Drill
    { words: wordify name
    , icon
    , description
    , children
    }

wordify ∷ String → Array MeasuredWord
wordify =
  unwrap ∘ wordifyId
  where
  wordifyId ∷ String → Identity (Array MeasuredWord)
  wordifyId =
    LineWrapping.measuredWords (pure <<< flip DOMUtils.getTextWidthPure (font fontSizePx))

font ∷ Number → DOMUtils.Font
font n =
  DOMUtils.Font $ "normal " <> show n <> "px Ubuntu"

fontSizePx ∷ Number
fontSizePx =
  12.0

isIconOnly ∷ Presentation → Boolean
isIconOnly = case _ of
  IconOnly → true
  _ → false

isDo ∷ ∀ a. Action a → Boolean
isDo = case _ of
  Do _ → true
  _ → false

isDrill ∷ ∀ a. Action a → Boolean
isDrill = case _ of
  Drill _ → true
  _ → false

isHighlighted ∷ ∀ a. Action a → Boolean
isHighlighted = case _ of
  Do {highlighted} →
    highlighted
  Drill {children} →
    F.any isHighlighted children
  GoBack → true

isDisabled ∷ ∀ a. Action a → Boolean
isDisabled = case _ of
  Do {disabled} →
    disabled
  Drill {children} →
    F.all isDisabled children
  GoBack → false

searchFilters ∷ ∀ a. Action a → Array String
searchFilters = case _ of
  Do {words} →
    LineWrapping.printMeasuredWord <$> words
  Drill {words, children} →
    (LineWrapping.printMeasuredWord <$> words)
      ⊕ A.concat (map searchFilters children)
  GoBack →
    [ "go back" ]

pluckActionDescription ∷ ∀ a. Action a → String
pluckActionDescription = case _ of
  Do {description} →
    description
  Drill {description} →
    description
  GoBack →
    "Go back"

pluckActionIcon ∷ ∀ a. Action a → Maybe I.IconHTML
pluckActionIcon = case _ of
  Do {icon} →
    icon
  Drill {icon} →
    icon
  GoBack →
    Just $ I.IconHTML I.goBack

actionNameWords ∷ ∀ a. Action a → Array MeasuredWord
actionNameWords = case _ of
  Do {words} →
    words
  Drill {words} →
    words
  GoBack →
    wordify "Go back"

calculateLines ∷ Number → Array MeasuredWord → Array NameLine
calculateLines maxWidth =
  map toNameLine ∘ LineWrapping.wrappedLines { maxWidth, spaceWidth }
  where
  spaceWidth ∷ Number
  spaceWidth =
    DOMUtils.getTextWidthPure " " (font fontSizePx)

  toNameLine ∷ WrappedLine → NameLine
  toNameLine wrappedLine =
    { line: LineWrapping.printWrappedLine wrappedLine
    , width: LineWrapping.lineWidth spaceWidth wrappedLine
    }

pluckAction ∷ ∀ a. Action a → Maybe a
pluckAction = case _ of
  Do {action} →
    Just action
  _ →
    Nothing

pluckDrillActions ∷ ∀ a. Action a → Maybe (Array (Action a))
pluckDrillActions = case _ of
  Drill {children} →
    Just children
  _ →
    Nothing

-- Firefox doesn't seem to be able to handle pixel metrics with decimal
-- precisons higher than one. Without applying this function actionlists
-- of certain widths render with empty spaces and overflowing actions.
firefoxify ∷ Number → Number
firefoxify n =
  if Utils.isFirefox
     then decimalCrop 1 n
     else n

lineHeightPx ∷ Number
lineHeightPx =
  13.0

iconSizeRatio ∷ Number
iconSizeRatio =
  0.3

-- Buttons are rendered with mystery centering and padding.
-- This can cause text to overflow.
buttonPaddingHighEstimate ∷ Number
buttonPaddingHighEstimate =
  0.3

decimalCrop ∷ Int → Number → Number
decimalCrop i n =
  (Math.floor $ n * multiplier) / multiplier
  where
  multiplier = Math.pow 10.0 $ Int.toNumber i

defaultConf ∷ ∀ a. Dimensions → Array (Action a) → ActionListConf a
defaultConf boundingDimensions as =
  let
    len = A.length as

    firstTry = mostRatioFittingRectangle goldenRatio len boundingDimensions

    actionSize
      | firstTry.height ≠ boundingDimensions.height =
          { dimensions: firstTry, leavesASpace: false }
      | otherwise =
          let secondTry = mostRatioFittingRectangle goldenRatio (len + 1) boundingDimensions
          in if secondTry.height ≡ boundingDimensions.height
             then { dimensions: firstTry, leavesASpace: false }
             else { dimensions: secondTry, leavesASpace: true }

    dimensions =
      { width: actionSize.dimensions.width
      , height: actionSize.dimensions.height
      }

    iconDimensions =
      { width: dimensions.width * iconSizeRatio
      , height: dimensions.height * iconSizeRatio
      }

    iconOnlyLeftPx =
      dimensions.width / 2.0 - iconDimensions.width / 2.0
    iconOnlyTopPx =
      dimensions.height / 2.0 - iconDimensions.height / 2.0
    iconMarginPx =
      dimensions.height * 0.05

    metrics =
      { dimensions
      , iconDimensions
      , iconMarginPx
      , iconOnlyLeftPx
      , iconOnlyTopPx
      }


    mkButtonConf action =
      { action
      , metrics
      , lines: map _.line $ calculateLines dimensions.width $ actionNameWords action
      , presentation: IconAndText
      }

    buttonConfs = map mkButtonConf as

    maxNumberOfLines =
      fromMaybe zero $ F.maximum $ map (A.length ∘ _.lines) buttonConfs

    maxTextHeightPx =
      Int.toNumber maxNumberOfLines * lineHeightPx

    buttonPaddingEstimatePx ∷ Number
    buttonPaddingEstimatePx =
      dimensions.height * buttonPaddingHighEstimate

    textDoesNotFitWithIcon ∷ Boolean
    textDoesNotFitWithIcon =
      maxTextHeightPx
        + iconDimensions.height
        + iconMarginPx
        + buttonPaddingEstimatePx
        > dimensions.height

    textDoesNotFitOnItsOwn ∷ Boolean
    textDoesNotFitOnItsOwn =
      buttonPaddingEstimatePx + maxTextHeightPx > dimensions.height
      ∨ dimensions.width < 40.0


    -- Allows text which fits vertically but not horizontally.
    -- Styling truncates overflowing lines with ellipses.
    --
    -- E.g. where there is only room for two lines:
    -- "Show Chart" would be presented without an icon as
    --
    -- Sh...
    -- Ch...
    --
    -- But "Build pie chart" would be presented with only an icon.
    --
    -- The reasoning behind this is that presentation should be
    -- unform per action list but that the entire list shouldn't be
    -- reduced to only icons just because "Troubleshoot" would be
    -- truncated to "Troublesh...".
    presentation
      | textDoesNotFitOnItsOwn ∨ maxNumberOfLines ≡ 0 = IconOnly
      | textDoesNotFitWithIcon = TextOnly
      | otherwise = IconAndText

    buttons = buttonConfs <#> \conf → conf
      { presentation =
        if isNothing $ pluckActionIcon conf.action
          then TextOnly
          else presentation
      }

  in
    { buttons
    , leavesASpace: actionSize.leavesASpace
    , classes: [ HH.ClassName "with-filter" ]
    }
