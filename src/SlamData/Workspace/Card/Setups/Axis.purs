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

module SlamData.Workspace.Card.Setups.Axis where

import SlamData.Prelude

import Data.Argonaut (JCursor(..), JObject, JArray, Json, insideOut, (:=), (.?), (~>), jsonEmptyObject)
import Data.Array as A
import Data.Foldable as F
import Data.List (List(..))
import Data.List as L
import Data.Map as M
import Data.Set as Set
import ECharts.Types as ET
import SlamData.Quasar.Query as QQ
import SlamData.Workspace.Card.Setups.Semantics as Sem
import SqlSquared as Sql
import Test.StrongCheck.Arbitrary (arbitrary)
import Test.StrongCheck.Data.Argonaut (ArbJCursor)
import Test.StrongCheck.Gen as Gen

data AxisType
  = Measure
  | Category
  | Time
  | Date
  | DateTime

derive instance eqAxisType ∷ Eq AxisType
derive instance ordAxisType ∷ Ord AxisType

compatible ∷ AxisType → Sem.Semantics → Boolean
compatible Measure = case _ of
  Sem.Value _ → true
  Sem.Bool _ → true
  Sem.Percent _ → true
  Sem.Money _ _ → true
  _ → false
compatible Category = case _ of
  Sem.Category _ → true
  _ → false
compatible Time = case _ of
  Sem.Time _ → true
  _ → false
compatible Date = case _ of
  Sem.Date _ → true
  _ → false
compatible DateTime = case _ of
  Sem.DateTime _ → true
  _ → false

printWithAxisType ∷ AxisType → Sem.Semantics → Maybe String
printWithAxisType atype sem
  | compatible atype sem = pure $ Sem.printSemantics sem
  | otherwise = Nothing

-- | Note that this function is used on stuff that has been already
-- | aggregated and analyzed, i.e. you take JArray, analyze it, extract axes
-- | and then you need to sort those axes
compareWithAxisType ∷ AxisType → String → String → Ordering
compareWithAxisType atype a b =
  let
    mbASem = Sem.analyzeString a
    mbBSem = Sem.analyzeString b

  in case atype of
    Measure →
      compare (mbASem >>= Sem.semanticsToNumber) (mbBSem >>= Sem.semanticsToNumber)
    Category →
      compare a b
    Time →
      compare (mbASem >>= Sem.semanticsToTime) (mbBSem >>= Sem.semanticsToTime)
    Date →
      compare (mbASem >>= Sem.semanticsToDate) (mbBSem >>= Sem.semanticsToDate)
    DateTime →
      compare (mbASem >>= Sem.semanticsToDateTime) (mbBSem >>= Sem.semanticsToDateTime)

type AxisTypeAnnotated a =
  { value ∷ a
  , time ∷ a
  , category ∷ a
  , date ∷ a
  , datetime ∷ a
  }

type Axes = AxisTypeAnnotated (Set.Set JCursor)

axisType ∷ JCursor → Axes → AxisType
axisType c axes
  | Set.member c axes.value = Measure
  | Set.member c axes.time = Time
  | Set.member c axes.date = Date
  | Set.member c axes.datetime = DateTime
  | otherwise = Category

genAxes ∷ Gen.Gen Axes
genAxes = do
  value ← map (Set.fromFoldable ∘ map unwrap) (arbitrary ∷ Gen.Gen (Array ArbJCursor))
  time ← map (Set.fromFoldable ∘ map unwrap) (arbitrary ∷ Gen.Gen (Array ArbJCursor))
  category ← map (Set.fromFoldable ∘ map unwrap) (arbitrary ∷ Gen.Gen (Array ArbJCursor))
  date ← map (Set.fromFoldable ∘ map unwrap) (arbitrary ∷ Gen.Gen (Array ArbJCursor))
  datetime ← map (Set.fromFoldable ∘ map unwrap) (arbitrary ∷ Gen.Gen (Array ArbJCursor))
  pure {value, time, category, date, datetime}

encodeAxes ∷ Axes → Json
encodeAxes axes =
  "value" := A.fromFoldable axes.value
  ~> "time" := A.fromFoldable axes.time
  ~> "category" := A.fromFoldable axes.category
  ~> "date" := A.fromFoldable axes.date
  ~> "datetime" := A.fromFoldable axes.datetime
  ~> jsonEmptyObject

decodeAxes ∷ JObject → String ⊹ Axes
decodeAxes js = do
  value ← map Set.fromFoldable ((js .? "value") ∷ String ⊹ Array JCursor)
  category ← map Set.fromFoldable ((js .? "category") ∷ String ⊹ Array JCursor)
  time ← map Set.fromFoldable ((js .? "time") ∷ String ⊹ Array JCursor)
  date ← map Set.fromFoldable ((js .? "date") <|> pure [])
  datetime ← map Set.fromFoldable ((js .? "datetime") <|> pure [])
  pure {value, category, time, date, datetime}

initialAxes ∷ Axes
initialAxes =
  { value: Set.empty
  , time: Set.empty
  , category: Set.empty
  , date: Set.empty
  , datetime: Set.empty
  }

eqAxes ∷ Axes → Axes → Boolean
eqAxes r1 r2 =
  r1.category ≡ r2.category
  ∧ r1.time ≡ r2.time
  ∧ r1.value ≡ r2.value
  ∧ r1.date ≡ r2.date
  ∧ r1.datetime ≡ r2.datetime

buildAxes ∷ JArray → Axes
buildAxes rs =
  rs
    # Sem.jarrayToSemantics
    # checkPairs
    # map checkSemantics
    # M.toUnfoldable
    # asList
    # foldl foldFn initialAxes
  where
  foldFn ∷ Axes → JCursor × Maybe AxisType → Axes
  foldFn acc (_ × Nothing) = acc
  foldFn acc (cursor × (Just at)) = case at of
    Measure → acc { value = Set.insert cursor acc.value }
    Category → acc { category = Set.insert cursor acc.category }
    Time → acc { time = Set.insert cursor acc.time }
    Date → acc { date = Set.insert cursor acc.date }
    DateTime → acc { datetime = Set.insert cursor acc.datetime }

checkSemantics ∷ List (Maybe Sem.Semantics) → Maybe AxisType
checkSemantics lst =
  result
  where
  result
    | semiLen < counts.value = Just Measure
    | semiLen < counts.category = Just Category
    | semiLen < counts.time = Just Time
    | semiLen < counts.datetime = Just DateTime
    | semiLen < counts.date = Just Date
    | otherwise = Nothing

  semiLen ∷ Int
  semiLen = L.length lst / 2

  counts ∷ AxisTypeAnnotated Int
  counts = foldl foldFn init lst

  init ∷ AxisTypeAnnotated Int
  init =
    { category: 0
    , time: 0
    , value: 0
    , date: 0
    , datetime: 0
    }

  foldFn ∷ AxisTypeAnnotated Int → Maybe Sem.Semantics → AxisTypeAnnotated Int
  foldFn acc Nothing = acc
  foldFn acc (Just a)
    | compatible Measure a =
        acc { value = acc.value + 1 }
    | compatible Category a =
        acc { category = acc.category + 1 }
    | compatible Time a =
        acc { time = acc.time + 1 }
    | compatible Date a =
        acc { date = acc.date +  1 }
    | compatible DateTime a =
        acc { datetime = acc.datetime + 1 }
    | otherwise =
        acc

checkPairs ∷ ∀ a. M.Map JCursor a → M.Map JCursor a
checkPairs m = case ks of
  Nil → m
  Cons _ Nil → m
  _ → foldl check m ks
  where
  ks ∷ List JCursor
  ks = M.keys m

  check ∷ M.Map JCursor a → JCursor → M.Map JCursor a
  check m' cursor
    | F.any (dependsOn cursor) ks = m'
    | otherwise = M.delete cursor m'

dependsOn ∷ JCursor → JCursor → Boolean
dependsOn a b = a ≠ b ∧
  dependsOn' (insideOut a) (insideOut b)
  where
  dependsOn' JCursorTop JCursorTop = true
  dependsOn' JCursorTop _ = true
  dependsOn' _ JCursorTop = true
  dependsOn' (JField _ c) (JField _ c') = dependsOn' c c'
  dependsOn' (JIndex _ c) (JIndex _ c') = dependsOn' c c'
  dependsOn' _ _ = false

type EChartsAxisConfiguration =
  { axisType ∷ ET.AxisType
  , interval ∷ Maybe Int
  }

axisConfiguration ∷ AxisType → EChartsAxisConfiguration
axisConfiguration = case _ of
  Measure → {axisType: ET.Value, interval: Nothing }
  Time → {axisType: ET.Category, interval: Just 0 }
  Date → {axisType: ET.Time, interval: Just 0 }
  DateTime → {axisType: ET.Time, interval: Just 0 }
  Category → {axisType: ET.Category, interval: Just 0 }

axesToFields ∷ Axes → L.List Sql.Sql
axesToFields ax = QQ.jcursorToSql Nothing <$> L.fromFoldable (ax.value <> ax.category)
