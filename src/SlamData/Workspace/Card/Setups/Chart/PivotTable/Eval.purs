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

module SlamData.Workspace.Card.Setups.Chart.PivotTable.Eval
  ( eval
  , module PTM
  ) where

import SlamData.Prelude

import Control.Monad.Aff.Class (class MonadAff)
import Control.Monad.State (class MonadState, get, put)
import Control.Monad.Writer (class MonadTell)
import Data.Argonaut as J
import Data.Array as Array
import Data.Lens ((.~), (?~), (<>~))
import Data.List ((:))
import Data.List as L
import Data.Map as Map
import Data.NonEmpty as NE
import Data.Set as Set
import SlamData.Effects (SlamDataEffects)
import SlamData.Quasar.Class (class ParQuasarDSL)
import SlamData.Quasar.Query as QQ
import SlamData.Workspace.Card.Error as CE
import SlamData.Workspace.Card.Eval.Common as CEC
import SlamData.Workspace.Card.Eval.Monad as CEM
import SlamData.Workspace.Card.Port as Port
import SlamData.Workspace.Card.Port.VarMap as VM
import SlamData.Workspace.Card.Setups.Chart.PivotTable.Error (PivotTableError(..), throwPivotTableError)
import SlamData.Workspace.Card.Setups.Chart.PivotTable.Model as PTM
import SlamData.Workspace.Card.Setups.Common.Eval (analyze)
import SlamData.Workspace.Card.Setups.Common.Sql (numFuncs)
import SlamData.Workspace.Card.Setups.Dimension as D
import SlamData.Workspace.Card.Setups.Transform as T
import SqlSquared (Sql)
import SqlSquared as Sql

eval
  ∷ ∀ m v
  . MonadAsk CEM.CardEnv m
  ⇒ MonadState CEM.CardState m
  ⇒ MonadTell CEM.CardLog m
  ⇒ MonadThrow (Variant (pivotTable ∷ PivotTableError, qerror ∷ CE.QError, resource ∷ CE.ResourceError | v)) m
  ⇒ MonadAff SlamDataEffects m
  ⇒ ParQuasarDSL m
  ⇒ PTM.Model
  → Port.Port
  → m Port.Out
eval options port = do
  CEM.CardEnv { varMap, cardId, path } ← ask
  var × resource ← CEM.extractResourcePair port
  state ← get
  _ × axes ← analyze resource =<< get
  put $ Just $ CEM.Analysis { axes, records: [], resource }
  when (Array.null options.columns) do
    throwPivotTableError PivotTableNoColumnSelectedError
  let
    port' × sql = mkSql options var
  resource' ←
    CEC.localEvalResource (Sql.Query numFuncs sql) varMap
      >>= pivotTableError PivotTableQuasarError
  pure
    $ Port.PivotTable port'
    × VM.insert cardId (VM.Var Port.defaultResourceVar) (VM.Resource resource') varMap

mkSql ∷ PTM.Model → VM.Var → Port.PivotTablePort × Sql
mkSql options (VM.Var vari) =
  let
    isSimple = PTM.isSimple options
    port = genPort isSimple options

    dimVals = L.fromFoldable $ map escapeDimension <$> port.dimensions
    colVals = L.fromFoldable $ map escapeColumn <$> port.columns

    dimProjections ∷ L.List (Sql.Projection Sql)
    dimProjections = dimVals <#> \(n × tr × prj) →
      groupByTransform tr prj # Sql.as (Sql.printIdent n)

    colProjections ∷ L.List (Sql.Projection Sql)
    colProjections = colVals <#> case _ of
      n × Nothing × val | isSimple → val # Sql.as (Sql.printIdent n)
      n × tr × val → columnTransform tr val # Sql.as (Sql.printIdent n)

    projectionExpr ∷ ∀ a. Sql.Projection a → a
    projectionExpr (Sql.Projection { expr }) = expr

    groupByFields ∷ L.List Sql
    groupByFields = dimVals <#> \(_ × tr × val) →
      projectionExpr $ maybe val (flip T.applyTransform val) tr


    groupBy ∷ Maybe (Sql.GroupBy Sql)
    groupBy = case groupByFields of
      L.Nil → Nothing
      xs → Just $ Sql.groupBy xs

    orderBy ∷ Maybe (Sql.OrderBy Sql)
    orderBy = case groupByFields of
      L.Nil → Nothing
      h:tl → Just $ Sql.OrderBy $ (Sql.ASC × h) NE.:| map (Sql.ASC × _) tl

    projections ∷ L.List (Sql.Projection Sql)
    projections = dimProjections <> colProjections

    sql ∷ Sql
    sql =
      Sql.buildSelect
        $ (Sql._projections <>~ projections)
        ∘ (Sql._relations ?~ Sql.VariRelation { vari, alias: Just "row" })
        ∘ (Sql._groupBy .~ groupBy)
        ∘ (Sql._orderBy .~ orderBy)
  in
    port × sql

genPort ∷ Boolean → PTM.Model → Port.PivotTablePort
genPort isSimpleQuery model =
  toPort
    (foldl go { names: Map.empty, taken: Set.empty }
      (map Left model.dimensions <> map Right model.columns))
  where
  toPort
    ∷ { taken ∷ Set.Set String
      , names ∷ Map.Map (Either PTM.GroupByDimension PTM.ColumnDimension) String
      }
    → { dimensions ∷ Array (Tuple String PTM.GroupByDimension)
      , columns ∷ Array (Tuple String PTM.ColumnDimension)
      , isSimpleQuery ∷ Boolean
      }
  toPort res =
    let
      dimensions = flip Array.mapMaybe model.dimensions \j → (_ × j) <$> Map.lookup (Left j) res.names
      columns = flip Array.mapMaybe model.columns \j → (_ × j) <$> Map.lookup (Right j) res.names
    in { dimensions, columns, isSimpleQuery }

  go
    ∷ { taken ∷ Set.Set String
      , names ∷ Map.Map (Either PTM.GroupByDimension PTM.ColumnDimension) String
      }
    → Either PTM.GroupByDimension PTM.ColumnDimension
    → { names ∷ Map.Map (Either PTM.GroupByDimension PTM.ColumnDimension) String
      , taken ∷ Set.Set String
      }
  go { names, taken } a
    | Map.member a names = { names, taken }
    | otherwise =
        let name = uniqueTag 1 taken (genName a)
        in
          { names: Map.insert a name names
          , taken: Set.insert name taken
          }

  genName ∷ Either PTM.GroupByDimension PTM.ColumnDimension → String
  genName = case _ of
    Left  (D.Dimension _ (D.Static _)) → "static"
    Left  (D.Dimension _ (D.Projection _ value)) → topName value
    Right (_ × D.Dimension _ (D.Static _)) → "static"
    Right (_ × D.Dimension _ (D.Projection (Just T.Count) PTM.All)) → "count"
    Right (_ × D.Dimension _ (D.Projection _ PTM.All)) → "all"
    Right (_ × D.Dimension _ (D.Projection _ (PTM.Column value))) → topName value

  uniqueTag ∷ Int → Set.Set String → String → String
  uniqueTag n taken a =
    let name = if n ≡ 1 then a else a <> "_" <> show n
    in if Set.member name taken then uniqueTag (n + 1) taken a else name

topName ∷ J.JCursor → String
topName = case _ of
  J.JField c J.JCursorTop → c
  J.JField c (J.JIndex i J.JCursorTop) → c <> "_" <> show i
  J.JIndex i J.JCursorTop → "_" <> show i
  J.JField _ cs → topName cs
  J.JIndex _ cs → topName cs
  J.JCursorTop → "value"

groupByTransform ∷ Maybe T.Transform → Sql.Projection Sql → Sql.Projection Sql
groupByTransform (Just tr) b = T.applyTransform tr b
groupByTransform Nothing b   = b

columnTransform ∷ Maybe T.Transform → Sql.Projection Sql → Sql.Projection Sql
columnTransform (Just tr) b = T.applyTransform tr b
columnTransform Nothing (Sql.Projection {alias, expr}) =
  Sql.Projection { alias, expr: Sql.unop Sql.UnshiftArray expr }

escapeDimension ∷ PTM.GroupByDimension → Maybe T.Transform × Sql.Projection Sql
escapeDimension = case _ of
  D.Dimension _ (D.Static str) → Nothing × (Sql.projection $ Sql.ident str)
  D.Dimension _ (D.Projection tr cur) →
    tr × (Sql.projection $ QQ.jcursorToSql (Just (Sql.Ident "row")) cur)

escapeColumn ∷ PTM.ColumnDimension → Maybe T.Transform × Sql.Projection Sql
escapeColumn = case _ of
  _ × D.Dimension _ (D.Static str) → Nothing × (Sql.projection $ Sql.ident str)
  _ × D.Dimension _ (D.Projection tr PTM.All) → tr × (Sql.projection $ Sql.ident "row")
  _ × D.Dimension _ (D.Projection tr (PTM.Column cur)) →
    tr × (Sql.projection $ QQ.jcursorToSql (Just (Sql.Ident "row")) cur)

pivotTableError ∷ ∀ e a m v. MonadThrow (Variant (pivotTable ∷ PivotTableError | v)) m ⇒ (e → PivotTableError) → Either e a → m a
pivotTableError f = either (throwPivotTableError ∘ f) pure
