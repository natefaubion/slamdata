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

module SlamData.Workspace.Card.BuildChart.PivotTable.Eval
  ( eval
  , module PTM
  ) where

import Data.Array as Array
import Data.Path.Pathy as P
import Data.String as String
import Data.StrMap as SM

import SlamData.Prelude
import SlamData.Quasar.Class (class QuasarDSL, liftQuasar)
import SlamData.Quasar.Error as QE
import SlamData.Quasar.Query as QQ
import SlamData.Workspace.Card.BuildChart.Aggregation as Ag
import SlamData.Workspace.Card.BuildChart.PivotTable.Model as PTM
import SlamData.Workspace.Card.Eval.CardEvalT as CET
import SlamData.Workspace.Card.Port as Port
import Quasar.Advanced.QuasarAF as QF
import Quasar.Data (JSONMode(..))

eval
  ∷ ∀ m
  . (Monad m, QuasarDSL m)
  ⇒ PTM.Model
  → Port.TaggedResourcePort
  → CET.CardEvalT m Port.Port
eval Nothing _ =
  QE.throw "Please select axis to aggregate"
eval (Just options) tr | PTM.isSimple options = do
  axes ← CET.liftQ $ QQ.axes tr.resource 100
  pure $ Port.PivotTable { records: [], options, taggedResource: tr, axes }
eval (Just options@{ dimensions: [] }) tr = do
  let
    path = fromMaybe P.rootDir (P.parentDir tr.resource)
    cols =
      Array.mapWithIndex
        case _, _ of
          i, PTM.Column c →
            sqlAggregation c.valueAggregation ("row" <> show c.value) <> " AS _" <> show i
          i, PTM.Count →
            "COUNT(*) AS _" <> show i
        options.columns
    sql =
      QQ.templated tr.resource $ String.joinWith " "
        [ "SELECT " <> String.joinWith ", " cols
        , "FROM {{path}} AS row"
        ]
  axes ← CET.liftQ $ QQ.axes tr.resource 100
  records ← CET.liftQ $ liftQuasar $
    QF.readQuery Readable path sql SM.empty Nothing
  pure $ Port.PivotTable { records, options, taggedResource: tr, axes }
eval (Just options) tr = do
  let
    path = fromMaybe P.rootDir (P.parentDir tr.resource)
    dlen = Array.length dims
    groupBy =
      map (\value → "row" <> show value) options.dimensions
    dims =
      Array.mapWithIndex
        (\i value → "row" <> show value <> " AS _" <> show i)
        options.dimensions
    cols =
      Array.mapWithIndex
        case _, _ of
          i, PTM.Column c → sqlAggregation c.valueAggregation ("row" <> show c.value) <> " AS _" <> show (i + dlen)
          i, PTM.Count    → "COUNT(*) AS _" <> show (i + dlen)
        options.columns
    sql =
      QQ.templated tr.resource $ String.joinWith " "
        [ "SELECT " <> String.joinWith ", " (dims <> cols)
        , "FROM {{path}} AS row"
        , "GROUP BY " <> String.joinWith ", " groupBy
        , "ORDER BY " <> String.joinWith ", " groupBy
        ]
  axes ← CET.liftQ $ QQ.axes tr.resource 100
  records ← CET.liftQ $ liftQuasar $
    QF.readQuery Readable path sql SM.empty Nothing
  pure $ Port.PivotTable { records, options, taggedResource: tr, axes }

sqlAggregation ∷ Maybe Ag.Aggregation → String → String
sqlAggregation a b = case a of
  Just Ag.Minimum → "MIN(" <> b <> ")"
  Just Ag.Maximum → "MAX(" <> b <> ")"
  Just Ag.Average → "AVG(" <> b <> ")"
  Just Ag.Sum     → "SUM(" <> b <> ")"
  _               → "[" <> b <> " ...]"
