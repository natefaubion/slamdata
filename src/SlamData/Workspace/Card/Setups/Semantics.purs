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

module SlamData.Workspace.Card.Setups.Semantics where

import SlamData.Prelude

import Data.Argonaut (class DecodeJson, class EncodeJson, runJsonPrim, toPrims, JsonPrim, Json, JArray, JCursor, foldJson, cursorGet, jsonEmptyObject, (~>), (:=), (.?), decodeJson)
import Data.Array as A
import Data.Enum (fromEnum, toEnum)
import Data.Formatter.DateTime as Fd
import Data.Int as Int
import Data.List (List(..), catMaybes)
import Data.List as L
import Data.Map (Map, keys, update, lookup, fromFoldable)
import Data.String (take)
import Data.String.Regex (Regex, regex, match)
import Data.String.Regex.Flags as RXF
import Data.DateTime as DT

import SqlSquared (Sql)
import SqlSquared as Sql

import Test.StrongCheck.Arbitrary (class Arbitrary, arbitrary)
import Test.StrongCheck.Gen as Gen

import Utils (stringToNumber)

data Semantics
  = Value Number
  | Percent Number
  | Money Number String
  | Bool Boolean
  | Category String
  | Time DT.Time
  | Date DT.Date
  | DateTime DT.DateTime

derive instance eqSemantics ∷ Eq Semantics
derive instance ordSemantics ∷ Ord Semantics

instance encodeJsonChartSemantics ∷ EncodeJson Semantics where
  encodeJson = case _ of
    Value n →
      "semanticsType" := "value"
      ~> "value" := n
      ~> jsonEmptyObject
    Percent p →
      "semanticsType" := "percent"
      ~> "percent" := p
      ~> jsonEmptyObject
    Money n s →
      "semanticsType" := "money"
      ~> "value" := n
      ~> "currency" := s
      ~> jsonEmptyObject
    Bool b →
      "semanticsType" := "bool"
      ~> "value" := b
      ~> jsonEmptyObject
    Category c →
      "semanticsType" := "category"
      ~> "value" := c
      ~> jsonEmptyObject
    Time (DT.Time h m s ms) →
      "semanticsType" := "time"
      ~> "hour" := (fromEnum h)
      ~> "minute" := (fromEnum m)
      ~> "second" := (fromEnum s)
      ~> "millisecond" := (fromEnum ms)
      ~> jsonEmptyObject
    Date d →
      "semanticsType" := "date"
      ~> "year" := (fromEnum $ DT.year d)
      ~> "month" := (fromEnum $ DT.month d)
      ~> "day" := (fromEnum $ DT.day d)
      ~> jsonEmptyObject
    DateTime (DT.DateTime d (DT.Time h m s ms)) →
      "semanticsType" := "datetime"
      ~> "year" := (fromEnum $ DT.year d)
      ~> "month" := (fromEnum $ DT.month d)
      ~> "day" := (fromEnum $ DT.day d)
      ~> "hour" := (fromEnum h)
      ~> "minute" := (fromEnum m)
      ~> "second" := (fromEnum s)
      ~> "millisecond" := (fromEnum ms)
      ~> jsonEmptyObject

instance decodeJsonChartSemantics ∷ DecodeJson Semantics where
  decodeJson = decodeJson >=> \obj → do
    semType ← obj .? "semanticsType"
    case semType of
      "value" → decodeValue obj
      "percent" → decodePercent obj
      "money" → decodeMoney obj
      "bool" → decodeBool obj
      "category" → decodeCategory obj
      "time" → decodeTime obj
      "date" → decodeDate obj
      "datetime" → decodeDatetime obj
      _ → Left "Incorrect semanticsType"
    where
    decodeBool obj = do
      v ← obj .? "value"
      pure $ Bool v

    decodeValue obj = do
      v ← obj .? "value"
      pure $ Value v

    decodePercent obj = do
      p ← obj .? "percent"
      pure $ Percent p

    decodeMoney obj = do
      v ← obj .? "value"
      c ← obj .? "currency"
      pure $ Money v c

    decodeCategory obj = do
      v ← obj .? "value"
      pure $ Category v

    decodeTime' obj = do
      hour ← obj .? "hour"
      minute ← obj .? "minute"
      second ← obj .? "second"
      millisecond ← obj .? "millisecond"
      let
        mbTime =
          DT.Time
          <$> (toEnum hour)
          <*> (toEnum minute)
          <*> (toEnum second)
          <*> (toEnum millisecond)
      case mbTime of
        Nothing → Left "Incorrect time"
        Just t → pure t

    decodeTime obj = do
      t ← decodeTime' obj
      pure $ Time t

    decodeDate' obj = do
      day ← obj .? "day"
      month ← obj .? "month"
      year ← obj .? "year"
      let
        mbDate = do
          y ← toEnum year
          m ← toEnum month
          d ← toEnum day
          DT.exactDate y m d
      case mbDate of
        Nothing → Left "Incorrect date"
        Just d → pure d

    decodeDate obj = do
      d ← decodeDate' obj
      pure $ Date d

    decodeDatetime obj = do
      t ← decodeTime' obj
      d ← decodeDate' obj
      pure $ DateTime $ DT.DateTime d t

instance arbitraryChartSemantics ∷ Arbitrary Semantics where
  arbitrary = do
    ix ← Gen.chooseInt 0 7
    case ix of
      0 → do
        v ← arbitrary
        pure $ Value v
      1 → do
        v ← arbitrary
        pure $ Percent v
      2 → do
        v ← arbitrary
        c ← arbitrary
        pure $ Money v c
      3 → do
        v ← arbitrary
        pure $ Bool v
      4 → do
        v ← arbitrary
        pure $ Category v
      5 → do
        t ← genTime
        pure $ Time t
      6 → do
        d ← genDate
        pure $ Date d
      7 → do
        t ← genTime
        d ← genDate
        pure $ DateTime $ DT.DateTime d t
      _ → pure $ Value 0.0
    where
    genTime = do
      hour ← map (fromMaybe bottom ∘ toEnum) $ Gen.chooseInt 0 23
      minute ← map (fromMaybe bottom ∘ toEnum) $ Gen.chooseInt 0 59
      second ← map (fromMaybe bottom ∘ toEnum) $ Gen.chooseInt 0 59
      millisecond ← map (fromMaybe bottom ∘ toEnum)
                    $Gen.chooseInt 0 999
      pure $ DT.Time hour minute second millisecond
    genDate = do
      day ← map (fromMaybe bottom ∘ toEnum) $ Gen.chooseInt 0 31
      month ← map (fromMaybe bottom ∘ toEnum) $ Gen.chooseInt 0 12
      year ← map (fromMaybe bottom ∘ toEnum) $ Gen.chooseInt 0 9999
      pure $ DT.canonicalDate day month year

printTime ∷ DT.Time → String
printTime time =
  let
    hour = fromEnum $ DT.hour time
    minute = fromEnum $ DT.minute time
    second = fromEnum $ DT.second time
    millisecond = fromEnum $ DT.millisecond time
    show' n = (if n > 9 then "" else "0") ⊕ show n
  in
    show' hour ⊕ ":" ⊕ show' minute ⊕ ":" ⊕ show' second ⊕ "." ⊕ show millisecond
-- We _can_ user purescript-formatters here, but printing semantics is used
-- heavily in axis analysis, so, simple printing used here instead of Μ stuff
printDate ∷ DT.Date → String
printDate date =
  let
    year = fromEnum $ DT.year date
    month = fromEnum $ DT.month date
    day = fromEnum $ DT.day date
    show' n = (if n > 9 then "" else "0") ⊕ show n
  in
    show year ⊕ "-" ⊕ show' month ⊕ "-" ⊕ show' day

printSemantics ∷ Semantics → String
printSemantics (Value v) =
  case Int.fromNumber v of
    Nothing → show v
    Just i → show i
printSemantics (Percent v) = show v <> "%"
printSemantics (Money v m) = show v <> m
printSemantics (Category s) = s
printSemantics (Bool b) = show b
printSemantics (Time t) = printTime t
printSemantics (Date d) = printDate d
printSemantics (DateTime (DT.DateTime d t)) = printDate d ⊕ "T" ⊕ printTime t

semanticsToSql ∷ Semantics → Array Sql
semanticsToSql = case _ of
  Value v → [ Sql.num v ]
  Percent v → [ Sql.num $ v / 100.0, Sql.string $ show v <> "%" ]
  Money v m → [ Sql.string $ show v <> m ]
  Category s → [ Sql.string s ]
  Bool b → [ Sql.bool b ]
  Time t →
    foldMap
      (pure ∘ Sql.invokeFunction "DATE" ∘ pure ∘ Sql.string)
      $ Fd.formatDateTime "YYYY-MM-DD"
      $ DT.DateTime bottom t
  Date d →
    foldMap
      (pure ∘ Sql.invokeFunction "TIME" ∘ pure ∘ Sql.string)
      $ Fd.formatDateTime "HH:mm:ss"
      $ DT.DateTime d bottom
  DateTime dt →
    foldMap
      (pure ∘ Sql.invokeFunction "TIMESTAMP" ∘ pure ∘ Sql.string)
      $ Fd.formatDateTime "YYYY-MM-DDTHH:mm:ssZ" dt

semanticsToNumber ∷ Semantics → Maybe Number
semanticsToNumber (Value v) = pure v
semanticsToNumber (Money v _) = pure v
semanticsToNumber (Percent v) = pure v
semanticsToNumber (Bool b) = pure if b then one else zero
semanticsToNumber _ = Nothing

semanticsToTime ∷ Semantics → Maybe DT.Time
semanticsToTime (Time t) = pure t
semanticsToTime _ = Nothing

semanticsToDate ∷ Semantics → Maybe DT.Date
semanticsToDate (Date d) = pure d
semanticsToDate _ = Nothing

semanticsToDateTime ∷ Semantics → Maybe DT.DateTime
semanticsToDateTime (DateTime dt) = pure dt
semanticsToDateTime _ = Nothing

analyze ∷ JsonPrim → Maybe Semantics
analyze p =
  runJsonPrim p
    (const Nothing)
    (Just ∘ Bool)
    (Just ∘ Value)
    analyzeString

analyzeJson ∷ Json → Maybe Semantics
analyzeJson =
  foldJson
    (const Nothing)
    (pure ∘ Bool)
    (pure ∘ Value)
    analyzeString
    (const Nothing)
    (const Nothing)

analyzeString ∷ String → Maybe Semantics
analyzeString str =
  analyzeTime str
  <|> analyzeDatetime str
  <|> analyzeDate str
  <|> analyzeNumber str
  <|> analyzeMoney str
  <|> analyzePercent str
  <|> (Just $ Category str)

analyzeNumber ∷ String → Maybe Semantics
analyzeNumber s = do
  num ← stringToNumber s
  guard $ show num == s || show (Int.floor num) == s
  pure $ Value num

percentRegex ∷ Regex
percentRegex = unsafePartial fromRight $ regex """^(-?\d+(\.\d+)?)\%$""" RXF.noFlags

analyzePercent ∷ String → Maybe Semantics
analyzePercent input = do
  matches ← match percentRegex input
  maybeMatch ← matches A.!! 1
  num ← maybeMatch >>= stringToNumber
  pure $ Percent num

moneyRegex ∷ Regex
moneyRegex = unsafePartial fromRight $ regex rgxStr RXF.noFlags
  where
  rgxStr = "^" <> curSymbols <> """?(([0-9]{1,3},([0-9]{3},)*[0-9]{3}|[0-9]+)(.[0-9][0-9])?)$"""
  curSymbols ∷ String
  curSymbols = """[\$\u20A0-\u20CF\u00A2\u00A3\u00A4\u00A5\u058F\u060B\u09F2\u09F3\u09FB\u0AF1\u0BF9\u0E3F\u17DB\uA838\uFDFC\uFE69\uFF04\uFFE0\uFFE1\uFFE5\uFFE6]"""

analyzeMoney ∷ String → Maybe Semantics
analyzeMoney str = do
  matches ← match moneyRegex str
  maybeMatch ← matches A.!! 1
  num ← maybeMatch >>= stringToNumber
  currencySymbol ←
    let fstSymbol = take 1 str
    in if fstSymbol == ""
       then Nothing
       else pure fstSymbol
  pure $ Money num currencySymbol

datetimeRegex ∷ Regex
datetimeRegex = unsafePartial fromRight $ regex rgxStr RXF.noFlags
  where
  rgxStr = "^(-?(?:[1-9][0-9]*)?[0-9]{4})-(1[0-2]|0?[1-9])-(3[0-1]|0?[1-9]|[1-2][0-9])(T| )(2[0-3]|[0-1]?[0-9]):([0-5]?[0-9]):([0-5]?[0-9])(\\.[0-9]+)?(Z|[+-](?:2[0-3]|[0-1]?[0-9]):[0-5]?[0-9])?$"

timeRegex ∷ Regex
timeRegex = unsafePartial fromRight $ regex rgxStr RXF.noFlags
  where
  rgxStr = "^(2[0-3]|[0-1][0-9]):([0-5]?[0-9]):([0-5]?[0-9])?(\\.[0-9]+)?(Z|[+-](?:2[0-3]|[0-1]?[0-9]):[0-5]?[0-9])?$"

dateRegex ∷ Regex
dateRegex = unsafePartial fromRight $ regex rgxStr RXF.noFlags
  where
  rgxStr = "^(-?(?:[1-9][0-9]*)?[0-9]{4})-(1[0-2]|0?[1-9])-(3[0-1]|0?[1-9]|[1-2][0-9])$"

analyzeDate ∷ String → Maybe Semantics
analyzeDate str = do
  matches ← match dateRegex str
  year ← (join $ matches A.!! 1) >>= Int.fromString >>= toEnum
  month ← (join $ matches A.!! 2) >>= Int.fromString >>= toEnum
  day ← (join $ matches A.!! 3) >>= Int.fromString >>= toEnum
  date ← DT.exactDate year month day
  pure $ Date date

analyzeTime ∷ String → Maybe Semantics
analyzeTime str = do
  matches ← match timeRegex str
  hour ← (join $ matches A.!! 1) >>= Int.fromString >>= toEnum
  minute ← (join $ matches A.!! 2) >>= Int.fromString >>= toEnum
  let
    second = fromMaybe bottom $ (join $ matches A.!! 3) >>= Int.fromString >>= toEnum
    millisecond = fromMaybe bottom $ (join $ matches A.!! 4) >>= Int.fromString >>= toEnum
  pure $ Time $ DT.Time hour minute second millisecond

analyzeDatetime ∷ String → Maybe Semantics
analyzeDatetime str = do
  matches ← match datetimeRegex str
  year ← (join $ matches A.!! 1) >>= Int.fromString >>= toEnum
  month ← (join $ matches A.!! 2) >>= Int.fromString >>= toEnum
  day ← (join $ matches A.!! 3) >>= Int.fromString >>= toEnum
  hour ← (join $ matches A.!! 5) >>= Int.fromString >>= toEnum
  minute ← (join $ matches A.!! 6) >>= Int.fromString >>= toEnum
  let
    second = fromMaybe bottom $ (join $ matches A.!! 7) >>= Int.fromString >>= toEnum
    millisecond = fromMaybe bottom $  (join $ matches A.!! 8) >>= Int.fromString >>= toEnum
    time = DT.Time hour minute second millisecond

  date ← DT.exactDate year month day
  pure
    $ DateTime
    $ DT.DateTime date time

jsonToSemantics ∷ Json → Map JCursor Semantics
jsonToSemantics j =
  fromFoldable $ catMaybes $ map (traverse analyze) $ toPrims j

jarrayToSemantics ∷ JArray → Map JCursor (List (Maybe Semantics))
jarrayToSemantics arr = foldl foldFn initial mapArr
  where
  mapArr ∷ Array (Map JCursor Semantics)
  mapArr = map jsonToSemantics arr

  initial ∷ Map JCursor (List (Maybe Semantics))
  initial = fromFoldable $ map (flip Tuple Nil) ks

  ks ∷ List JCursor
  ks = L.nub $ foldMap keys mapArr

  foldFn
    ∷ Map JCursor (List (Maybe Semantics))
    → Map JCursor Semantics
    → Map JCursor (List (Maybe Semantics))
  foldFn acc m = foldl (insertOne m) acc ks

  insertOne
    ∷ Map JCursor Semantics
    → Map JCursor (List (Maybe Semantics))
    → JCursor
    → Map JCursor (List (Maybe Semantics))
  insertOne m acc k = update (pure ∘ Cons (lookup k m)) k acc

getMaybeString ∷ Json → JCursor → Maybe String
getMaybeString js cursor =
  map printSemantics
    $ analyzeJson
    =<< cursorGet cursor js

getValues ∷ Json → Maybe JCursor → Array Number
getValues js cursor =
  foldMap A.singleton
    $ semanticsToNumber
    =<< analyzeJson
    =<< flip cursorGet js
    =<< cursor

getSemantics ∷ Json → JCursor → Maybe Semantics
getSemantics js cursor =
  cursorGet cursor js >>= analyzeJson

maybeString ∷ Json → Maybe String
maybeString = map printSemantics ∘ analyzeJson

requiredString ∷ String → Json → String
requiredString str = fromMaybe str ∘ maybeString

maybeNumber ∷ Json → Maybe Number
maybeNumber = semanticsToNumber <=< analyzeJson

requiredNumber ∷ Number → Json → Number
requiredNumber num = fromMaybe num ∘ maybeNumber
