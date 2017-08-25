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

module SlamData.Workspace.Card.Setups.Common.Positioning where

import SlamData.Prelude

import Data.Array as A
import Data.Int as Int

import ECharts.Monad (DSL)
import ECharts.Commands as E
import ECharts.Types as ET
import ECharts.Types.Phantom as ETP

type RadialPosition r =
  { x ∷ Maybe Number
  , y ∷ Maybe Number
  , radius ∷ Maybe Number
  | r
  }

type WithDonutRadius r =
  { radius ∷ Maybe {start ∷ Number, end ∷ Number }
  | r
  }

itemsInRow ∷ ∀ a. Array a → Int
itemsInRow arr =
  let
    len = A.length arr

    result
      | len < 2 = 1
      | len < 5 = 2
      | len < 10 = 3
      | otherwise = 4
  in
    result

rowCount ∷ ∀ a. Array a → Int
rowCount arr =
  Int.ceil $ Int.toNumber (A.length arr) / Int.toNumber (itemsInRow arr)

radialSpaceCoeff ∷ Number
radialSpaceCoeff = 0.85

rectangularSpaceCoeff ∷ Number
rectangularSpaceCoeff = 0.72

adjustRadialPositions
  ∷ ∀ r
  . Array (RadialPosition r)
  → Array (RadialPosition r)
adjustRadialPositions ps =
  let
    inRow ∷ Int
    inRow = itemsInRow ps

    numRows ∷ Int
    numRows = rowCount ps

    radiusDivisor ∷ Number
    radiusDivisor = Int.toNumber $ max inRow numRows

    radius ∷ Maybe Number
    radius = Just $ 65.0 / radiusDivisor

    setPositions
      ∷ Array (RadialPosition r)
      → Int
      → Int
      → Int
      → Array (RadialPosition r)
      → Array (RadialPosition r)
    setPositions acc colIx rowIx inThisRow arr = case A.uncons arr of
      Nothing → acc
      Just {head, tail} →
        let
          top ∷ Maybe Number
          top =
            Just $ 100.0 * (2.0 * Int.toNumber rowIx + 1.0) / (Int.toNumber numRows * 2.0)

          left ∷ Maybe Number
          left =
            Just $ 10.0 + 90.0 * (2.0 * Int.toNumber colIx + 1.0) / (2.0 * Int.toNumber inThisRow)


          toPush ∷ RadialPosition r
          toPush =
            head { x = left
                 , y = top
                 , radius = radius
                 }

          newAcc ∷ Array (RadialPosition r)
          newAcc = A.snoc acc toPush

          newColIx ∷ Int
          newColIx
            | one + colIx < inThisRow = colIx + one
            | otherwise = zero

          newRowIx ∷ Int
          newRowIx
            | newColIx ≡ zero = rowIx + one
            | otherwise = rowIx

          inNewRow ∷ Int
          inNewRow
            | A.length tail > inRow = inRow
            | newColIx ≠ zero = inRow
            | otherwise = A.length tail
        in
          setPositions newAcc newColIx newRowIx inNewRow tail
  in
    setPositions [] 0 0 inRow ps

adjustDonutRadiuses
  ∷ ∀ r
  . Array (WithDonutRadius r)
  → Array (WithDonutRadius r)
adjustDonutRadiuses arr =
  let
    len ∷ Number
    len = Int.toNumber $ A.length arr

    step ∷ Number
    step = 1.0 / len

    adjustRadius
      ∷ Array (WithDonutRadius r) → Number → Array (WithDonutRadius r) → Array (WithDonutRadius r)
    adjustRadius acc ix inp = case A.uncons inp of
      Nothing → acc
      Just {head, tail} →
        let
          toPush = head { radius = Just {start: ix * step, end: (ix + one) * step }}
          newAcc = A.snoc acc toPush
          newIx = ix + one
        in
          adjustRadius newAcc newIx tail
  in
    adjustRadius [] zero arr

radialTitles
  ∷ ∀ r i
  . Array (RadialPosition (name ∷ Maybe String |r))
  → String
  → DSL (title ∷ ETP.I|i)
radialTitles rposs namePrefix =
  let prefix
        | namePrefix ≡ "" = ""
        | otherwise = namePrefix ⊕ ": "
  in E.titles $ for_ rposs \{name, x, y} → E.title do
    for_ name \n → E.text $ prefix ⊕ n
    E.textStyle do
      E.fontFamily "Ubuntu, sans"
      E.fontSize 12
    for_ x \left →
      E.left $ ET.Percent $ left - 0.3
    for_ y \top →
      E.top $ ET.Percent $ top - (rowHeight * radialSpaceCoeff) / 2.0
    E.textCenter
    E.textBottom
  where
  numRows ∷ Int
  numRows = rowCount rposs

  rowHeight ∷ Number
  rowHeight = 100.0 / Int.toNumber numRows


type RectangularPosition r =
  { x ∷ Maybe Number
  , y ∷ Maybe Number
  , w ∷ Maybe Number
  , h ∷ Maybe Number
  , fontSize ∷ Maybe Int
  | r
  }

adjustRectangularPositions
  ∷ ∀ r
  . Array (RectangularPosition r)
  → Array (RectangularPosition r)
adjustRectangularPositions arr =
  let
    len ∷ Int
    len = A.length arr

    inRow ∷ Int
    inRow = itemsInRow arr

    numRows ∷ Int
    numRows = rowCount arr

    colWidth ∷ Number
    colWidth =
      100.0 / Int.toNumber inRow

    rowHeight ∷ Number
    rowHeight =
      100.0 / Int.toNumber numRows

    legendAndVMHeight ∷ Number
    legendAndVMHeight = 10.0

    titleHeight ∷ Number
    titleHeight = 3.0

    setPositions
      ∷ Array (RectangularPosition r)
      → Int
      → Int
      → Int
      → Array (RectangularPosition r)
      → Array (RectangularPosition r)
    setPositions acc colIx rowIx inThisRow arr' = case A.uncons arr' of
      Nothing → acc
      Just {head, tail} →
        let
          left ∷ Maybe Number
          left =
            Just
              $ 100.0 * (2.0 * Int.toNumber colIx + one) / (Int.toNumber inRow * 2.0)
              - (colWidth * rectangularSpaceCoeff) / 2.0

          top ∷ Maybe Number
          top =
            Just
              $ (100.0 - legendAndVMHeight - Int.toNumber numRows * titleHeight)
              * (2.0 * Int.toNumber rowIx + one) / (Int.toNumber numRows * 2.0)
              - (rowHeight * rectangularSpaceCoeff) / 2.0
              + Int.toNumber (rowIx + one) *  titleHeight

          fontSize ∷ Int
          fontSize
            | numRows < 5 = 12
            | numRows < 6 = 11
            | otherwise = 10

          toPush ∷ RectangularPosition r
          toPush =
            head { x = left
                 , y = top
                 , w = Just $ colWidth * rectangularSpaceCoeff
                 , h = Just $ rowHeight * rectangularSpaceCoeff
                 , fontSize = Just fontSize
                 }

          newAcc ∷ Array (RectangularPosition r)
          newAcc = A.snoc acc toPush

          newColIx ∷ Int
          newColIx
            | one + colIx < inThisRow = colIx + one
            | otherwise = zero

          newRowIx ∷ Int
          newRowIx
            | newColIx ≡ zero = rowIx + one
            | otherwise = rowIx

          inNewRow ∷ Int
          inNewRow
            | A.length tail > inRow = inRow
            | newColIx ≠ zero = inRow
            | otherwise = A.length tail
        in
          setPositions newAcc newColIx newRowIx inNewRow tail
  in
    setPositions [] 0 0 inRow arr


rectangularTitles
  ∷ ∀ r i f
  . Foldable f
  ⇒ f (RectangularPosition (name ∷ Maybe String |r))
  → String
  → DSL (title ∷ ETP.I|i)
rectangularTitles poss namePrefix =
  let prefix
        | namePrefix ≡ "" = ""
        | otherwise = namePrefix ⊕ ": "
  in E.titles $ for_ poss \{name, x, y, h, fontSize} → E.title do
    for_ name \n → E.text $ prefix ⊕ n
    traverse_ E.text name
    E.textStyle do
      E.fontFamily "Ubuntu, sans"
      traverse_ E.fontSize fontSize
    for_ y \top →
      E.top $ ET.Percent $ top - 5.0
    for_ x \left →
      E.left $ ET.Percent $ left + colWidth / 2.0 - 0.3
    E.textCenter
    E.textMiddle
  where
  arr = foldMap (A.singleton) poss

  numRows ∷ Int
  numRows = rowCount arr

  rowHeight ∷ Number
  rowHeight = 100.0 / Int.toNumber numRows

  inRow ∷ Int
  inRow = itemsInRow arr

  colWidth ∷ Number
  colWidth =
    rectangularSpaceCoeff * 100.0 / Int.toNumber inRow

rectangularGrids
  ∷ ∀ r f i
  . Foldable f
  ⇒ f (RectangularPosition r)
  → DSL (grid ∷ ETP.I|i)
rectangularGrids poss = E.grids $ for_ poss \{w, h, x, y} → E.grid do
  for_ x $ E.left ∘ ET.Percent
  for_ y $ E.top ∘ ET.Percent
  for_ w E.widthPct
  for_ h E.heightPct



cartesian ∷ ∀ i. DSL (left ∷ ETP.I, right ∷ ETP.I, top ∷ ETP.I, containLabel ∷ ETP.I|i)
cartesian = do
  E.left $ ET.Pixel 30
  E.right $ ET.Pixel 30
  E.top $ ET.Pixel 30
  E.containLabel true
