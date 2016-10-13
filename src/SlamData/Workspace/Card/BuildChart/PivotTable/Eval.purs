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
import SlamData.Quasar.Class (liftQuasar)
import SlamData.Quasar.Query as QQ
import SlamData.Workspace.Card.BuildChart.Aggregation as Ag
import SlamData.Workspace.Card.BuildChart.Axis (Axes)
import SlamData.Workspace.Card.BuildChart.PivotTable.Model as PTM
import SlamData.Workspace.Card.Eval.Machine (stateful)
import SlamData.Workspace.Card.Eval.Monad as CEM
import SlamData.Workspace.Card.Eval.Transition as CET
import SlamData.Workspace.Card.Port as Port
import Quasar.Advanced.QuasarAF as QF
import Quasar.Data (JSONMode(..))
import Utils.Path (FilePath)

type EvalState =
  { axes ∷ Axes
  , taggedResource ∷ Port.TaggedResourcePort
  , query ∷ String
  }

eval ∷ CEM.EvalMachine
eval = stateful go Nothing
  where
  go ∷ Maybe EvalState → CEM.EvalMachineInput → CEM.CardEval (Maybe EvalState × Port.Port)
  go state arg@(input × ev) = case ev, input.input of
    CET.BuildPivotTable options, Just (Port.TaggedResource tr) → do
      let
        sameResource =
          fromMaybe false (Port.eqTaggedResourcePort tr ∘ _.taggedResource <$> state)
      axes ←
        case state of
          Just { axes: ax } | sameResource → pure ax
          _ → CEM.liftQ (QQ.axes tr.resource 100)
      let
        path = fromMaybe P.rootDir (P.parentDir tr.resource)
        query = mkSql options tr.resource
        state' =
          { axes
          , taggedResource: tr
          , query
          }
      -- For queries that do not have aggeregations or group-bys, we defer the
      -- query to the renderer so that it can do server-side paging.
      records ←
        if PTM.isSimple options
          then pure []
          else CEM.liftQ (liftQuasar (QF.readQuery Readable path query SM.empty Nothing))
      let
        port =
          { axes
          , taggedResource: tr
          , records
          , options
          , query
          }
      pure (Just state' × Port.PivotTable port)
    _, _ →
      CEM.unexpectedInput arg

mkSql ∷ PTM.Model → FilePath → String
mkSql options resource =
  let
    isSimple =
      PTM.isSimple options
    dimLen =
      Array.length options.dimensions
    groupBy =
      map (\value → "row" <> show value) options.dimensions
    dims =
      Array.mapWithIndex
        (\i value → "row" <> show value <> " AS _" <> show i)
        options.dimensions
    cols =
      Array.mapWithIndex
        case _, _ of
          i, PTM.Count →
            "COUNT(*) AS _" <> show (i + dimLen)
          i, PTM.Column c | isSimple →
            "row" <> show c.value <> " AS _" <> show (i + dimLen)
          i, PTM.Column c →
            sqlAggregation c.valueAggregation ("row" <> show c.value)
              <> " AS _" <> show (i + dimLen)
        options.columns
    head =
      [ "SELECT " <> String.joinWith ", " (dims <> cols)
      , "FROM {{path}} AS row"
      ]
    tail =
      [ "GROUP BY " <> String.joinWith ", " groupBy
      , "ORDER BY " <> String.joinWith ", " groupBy
      ]
  in
    QQ.templated resource $
      String.joinWith " "
        if dimLen == 0
          then head
          else head <> tail

sqlAggregation ∷ Maybe Ag.Aggregation → String → String
sqlAggregation a b = case a of
  Just Ag.Minimum → "MIN(" <> b <> ")"
  Just Ag.Maximum → "MAX(" <> b <> ")"
  Just Ag.Average → "AVG(" <> b <> ")"
  Just Ag.Sum     → "SUM(" <> b <> ")"
  _               → "[" <> b <> " ...]"
