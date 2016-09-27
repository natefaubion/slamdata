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
eval (Just options) tr | PTM.isSimple options =
  pure $ Port.PivotTable { records: [], options, taggedResource: tr }
eval (Just options@{ dimensions: [] }) tr = do
  let
    path = fromMaybe P.rootDir (P.parentDir tr.resource)
    cols =
      Array.mapWithIndex
        (\i (PTM.Column c) → sqlAggregation c.valueAggregation ("row" <> show c.value) <> " AS _" <> show i)
        options.columns
    sql =
      QQ.templated tr.resource $ String.joinWith " "
        [ "SELECT " <> String.joinWith ", " cols
        , "FROM {{path}} AS row"
        ]
  records ← CET.liftQ $ liftQuasar $
    QF.readQuery Readable path sql SM.empty Nothing
  pure $ Port.PivotTable { records, options, taggedResource: tr }
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
        (\i (PTM.Column c) → sqlAggregation c.valueAggregation ("row" <> show c.value) <> " AS _" <> show (i + dlen))
        options.columns
    sql =
      QQ.templated tr.resource $ String.joinWith " "
        [ "SELECT " <> String.joinWith ", " (dims <> cols)
        , "FROM {{path}} AS row"
        , "GROUP BY " <> String.joinWith ", " groupBy
        , "ORDER BY " <> String.joinWith ", " groupBy
        ]
  records ← CET.liftQ $ liftQuasar $
    QF.readQuery Readable path sql SM.empty Nothing
  pure $ Port.PivotTable { records, options, taggedResource: tr }

sqlAggregation ∷ Maybe Ag.Aggregation → String → String
sqlAggregation a b = case a of
  Just Ag.Minimum → "MIN(" <> b <> ")"
  Just Ag.Maximum → "MAX(" <> b <> ")"
  Just Ag.Average → "AVG(" <> b <> ")"
  Just Ag.Sum     → "SUM(" <> b <> ")"
  _               → "[" <> b <> " ...]"