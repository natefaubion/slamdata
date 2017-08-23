{-
Copyright 2017 SlamData, Inc.

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
module SlamData.Workspace.Card.Setups.DisplayOptions.DateFormat.Preset where

import SlamData.Prelude

import Data.Formatter.DateTime as FDT
import Data.Lens as Lens
import Data.List ((:))
import Data.List as L
import Data.List.NonEmpty as NEL
import Data.List.Safe as SL
import Data.Map as M
import Data.Profunctor.Strong ((&&&))

data Preset
  = ISO8601
  | Short
  | ShortUS
  | Medium
  | Long
  | LongUS

derive instance eqPreset ∷ Eq Preset

presetFormat ∷ Preset → FDT.Formatter
presetFormat = case _ of
  ISO8601 →
    FDT.YearFull
      : FDT.Placeholder "-"
      : FDT.MonthTwoDigits
      : FDT.Placeholder "-"
      : FDT.DayOfMonthTwoDigits
      : L.Nil
  Short →
    FDT.DayOfMonthTwoDigits
      : FDT.Placeholder "/"
      : FDT.MonthTwoDigits
      : FDT.Placeholder "/"
      : FDT.YearFull
      : L.Nil
  Medium →
    FDT.DayOfMonth
      : FDT.Placeholder " "
      : FDT.MonthShort
      : FDT.Placeholder " "
      : FDT.YearFull
      : L.Nil
  Long →
    FDT.DayOfMonth
      : FDT.Placeholder " "
      : FDT.MonthFull
      : FDT.Placeholder " "
      : FDT.YearFull
      : L.Nil
  ShortUS →
    FDT.MonthTwoDigits
      : FDT.Placeholder "/"
      : FDT.DayOfMonthTwoDigits
      : FDT.Placeholder "/"
      : FDT.YearFull
      : L.Nil
  LongUS →
    FDT.MonthFull
      : FDT.Placeholder " "
      : FDT.DayOfMonth
      : FDT.Placeholder ", "
      : FDT.YearFull
      : L.Nil

preset ∷ Lens.Prism' String Preset
preset = Lens.prism' to from
  where
    to = case _ of
      ISO8601 → "International standard (YYYY-MM-DD)"
      Short → "DD/MM/YYYY"
      ShortUS → "MM/DD/YYYY"
      Medium → "D MMM YYYY"
      Long → "D MMMM YYYY"
      LongUS → "MMMM D, YYYY"
    from = case _ of
      "International standard (YYYY-MM-DD)" → Just ISO8601
      "DD/MM/YYYY" → Just Short
      "MM/DD/YYYY" → Just ShortUS
      "D MMM YYYY" → Just Medium
      "D MMMM YYYY" → Just Long
      "MMMM D, YYYY" → Just LongUS
      _ → Nothing

presets ∷ NEL.NonEmptyList Preset
presets = SL.toNEL
  $ ISO8601
  SL.: Short
  SL.: ShortUS
  SL.: Medium
  SL.: Long
  SL.: LongUS
  SL.: SL.nil

presetsByFormat ∷ M.Map FDT.Formatter Preset
presetsByFormat =
  M.fromFoldable $ map (presetFormat &&& id) presets

isDateElement ∷ FDT.FormatterCommand → Boolean
isDateElement = case _ of
  FDT.YearFull → true
  FDT.YearTwoDigits → true
  FDT.YearAbsolute → true
  FDT.MonthFull → true
  FDT.MonthShort → true
  FDT.MonthTwoDigits → true
  FDT.DayOfMonthTwoDigits → true
  FDT.DayOfMonth → true
  FDT.DayOfWeek → true
  _ → false

isPlaceholder ∷ FDT.FormatterCommand → Boolean
isPlaceholder = case _ of
  FDT.Placeholder _ → true
  _ → false
