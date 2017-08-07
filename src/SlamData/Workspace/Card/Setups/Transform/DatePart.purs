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

module SlamData.Workspace.Card.Setups.Transform.DatePart where

import SlamData.Prelude
import SqlSquared as Sql
import Data.List as L
import Data.Argonaut as J
import Test.StrongCheck.Arbitrary (class Arbitrary)
import Test.StrongCheck.Gen as Gen

data DatePart
  = DayOfWeek
  | Day
  | DayOfYear
  | DayOfYearISO
  | Month
  | Quarter
  | Year
  | Decade
  | Century
  | Millenium

data TimePart
  = Microseconds
  | Milliseconds
  | Second
  | Minute
  | Hour

type DateTimePart = Either DatePart TimePart

dateParts ∷ Array DatePart
dateParts =
  [ Millenium
  , Century
  , Decade
  , Quarter
  , Year
  , Month
  , DayOfYear
  , DayOfYearISO
  , Day
  , DayOfWeek
  ]

timeParts ∷ Array TimePart
timeParts =
  [ Hour
  , Minute
  , Second
  , Milliseconds
  , Microseconds
  ]

dateTimeParts ∷ Array DateTimePart
dateTimeParts = (Left <$> dateParts) <> (Right <$> timeParts)

printDate ∷ DatePart → String
printDate = case _ of
  DayOfWeek → "dow"
  Day → "day"
  DayOfYear → "doy"
  DayOfYearISO → "isodoy"
  Month → "month"
  Quarter → "quarter"
  Year → "year"
  Decade → "decade"
  Century → "century"
  Millenium → "millenium"

printTime ∷ TimePart → String
printTime = case _ of
  Microseconds → "microseconds"
  Milliseconds → "milliseconds"
  Second → "second"
  Minute → "minute"
  Hour → "hour"

transformDateSql ∷ DatePart → Sql.Sql → Sql.Sql
transformDateSql dp expr =
  Sql.invokeFunction "DATE_PART"
  $ L.fromFoldable
  [ Sql.string $ printDate dp, expr ]

transformTimeSql ∷ TimePart → Sql.Sql → Sql.Sql
transformTimeSql tp expr =
  Sql.invokeFunction "DATE_PART"
  $ L.fromFoldable
  [ Sql.string $ printTime tp, expr ]

applyDateTransform ∷ DatePart → Sql.Projection Sql.Sql → Sql.Projection Sql.Sql
applyDateTransform dp (Sql.Projection { alias, expr }) =
  Sql.Projection
    { alias
    , expr: transformDateSql dp expr
    }

applyTimeTransform ∷ TimePart → Sql.Projection Sql.Sql → Sql.Projection Sql.Sql
applyTimeTransform tp (Sql.Projection { alias, expr }) =
  Sql.Projection
    { alias
    , expr: transformTimeSql tp expr
    }


printDateTime ∷ DateTimePart → String
printDateTime = either printDate printTime

prettyPrintDate ∷ DatePart → String
prettyPrintDate = case _ of
  DayOfWeek → "Day of Week"
  Day → "Day of Month"
  DayOfYear → "Day of Year"
  DayOfYearISO → "Day of Year (ISO)"
  Month → "Month"
  Quarter → "Quarter"
  Year → "Year"
  Decade → "Decade"
  Century → "Century"
  Millenium → "Millenium"

prettyPrintTime ∷ TimePart → String
prettyPrintTime = case _ of
  Microseconds → "Microseconds"
  Milliseconds → "Milliseconds"
  Second → "Second"
  Minute → "Minute"
  Hour → "Hour"

prettyPrintDateTime ∷ DateTimePart → String
prettyPrintDateTime = either prettyPrintDate prettyPrintTime

dateFromString ∷ String → Maybe DatePart
dateFromString = case _ of
  "dow" → Just DayOfWeek
  "day" → Just Day
  "doy" → Just DayOfYear
  "isodoy" → Just DayOfYearISO
  "month" → Just Month
  "quarter" → Just Quarter
  "year" → Just Year
  "decade" → Just Decade
  "century" → Just Century
  "millenium" → Just Millenium
  _ → Nothing

timeFromString ∷ String → Maybe TimePart
timeFromString = case _ of
  "microseconds" → Just Microseconds
  "milliseconds" → Just Milliseconds
  "second" → Just Second
  "minute" → Just Minute
  "hour" → Just Hour
  _ → Nothing

encodeDate ∷ DatePart → J.Json
encodeDate = J.encodeJson <<< printDate

decodeDate ∷ J.Json → Either String DatePart
decodeDate json = do
  str ← J.decodeJson json
  case dateFromString str of
    Just a → Right a
    _ → Left ("Invalid date part: " <> str)

encodeTime ∷ TimePart → J.Json
encodeTime = J.encodeJson <<< printTime

decodeTime ∷ J.Json → Either String TimePart
decodeTime json = do
  str ← J.decodeJson json
  case timeFromString str of
    Just a → Right a
    _ → Left ("Invalid time part: " <> str)

derive instance eqDatePart ∷ Eq DatePart
derive instance ordDatePart ∷ Ord DatePart
instance encodeJsonDatePart ∷ J.EncodeJson DatePart where encodeJson = encodeDate
instance decodeJsonDatePart ∷ J.DecodeJson DatePart where decodeJson = decodeDate

derive instance eqTimePart ∷ Eq TimePart
derive instance ordTimePart ∷ Ord TimePart
instance encodeJsonTimePart ∷ J.EncodeJson TimePart where encodeJson = encodeTime
instance decodeJsonTimePart ∷ J.DecodeJson TimePart where decodeJson = decodeTime

instance arbitraryDatePart ∷ Arbitrary DatePart where
  arbitrary = Gen.chooseInt 1 10 <#> case _ of
    1 → DayOfWeek
    2 → Day
    3 → DayOfYear
    4 → DayOfYearISO
    5 → Month
    6 → Year
    7 → Decade
    8 → Quarter
    9 → Century
    _ → Millenium

instance arbitraryTimePart ∷ Arbitrary TimePart where
  arbitrary = Gen.chooseInt 1 5 <#> case _ of
    1 → Microseconds
    2 → Milliseconds
    3 → Second
    4 → Minute
    _ → Hour
