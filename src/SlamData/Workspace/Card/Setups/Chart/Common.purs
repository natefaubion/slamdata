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

module SlamData.Workspace.Card.Setups.Chart.Common where

import SlamData.Prelude
import Data.Argonaut as J
import Data.Lens ((.~), (?~))
import Data.List as L
import Data.NonEmpty as NE
import SlamData.Quasar.Query as QQ
import SlamData.Workspace.Card.Port.VarMap as VM
import SlamData.Workspace.Card.Setups.Dimension as D
import SlamData.Workspace.Card.Setups.Transform as T
import SqlSquared as Sql

buildBasicSql
  ∷ ∀ p
  . (p → L.List (Sql.Projection Sql.Sql))
  → (p → Maybe (Sql.GroupBy Sql.Sql))
  → p
  → VM.Var
  → Sql.Sql
buildBasicSql buildProjections buildGroupBy r (VM.Var vari) =
  Sql.buildSelect
    $ ( Sql._projections .~ buildProjections r )
    ∘ ( Sql._relations ?~ Sql.VariRelation { vari, alias: Nothing })
    ∘ ( Sql._groupBy .~ groups )
    ∘ ( Sql._orderBy .~ orders )
  where
  groups = buildGroupBy r
  orders = groups >>= groupByToOrderBy

applyTransform ∷ ∀ a b. D.Dimension a b → Sql.Projection Sql.Sql → Sql.Projection Sql.Sql
applyTransform dim p = case dim of
  D.Dimension _ (D.Projection (Just t) _) → T.applyTransform t p
  _ → p

jcursorSql ∷ ∀ a. D.Dimension a J.JCursor → Sql.Sql
jcursorSql (D.Dimension _ cat) = case cat of
  D.Static str → Sql.string str
  D.Projection mba pr →
    let prj = QQ.jcursorToSql Nothing pr
    in case mba of
      Nothing → prj
      Just a → T.transformSql a prj

jcursorPrj ∷ ∀ a. D.Dimension a J.JCursor → Sql.Projection Sql.Sql
jcursorPrj = Sql.projection ∘ jcursorSql

nullPrj ∷ Sql.Projection Sql.Sql
nullPrj = Sql.projection Sql.null

groupBy ∷ L.List Sql.Sql → Maybe (Sql.GroupBy Sql.Sql)
groupBy keys
  | L.null keys = Nothing
  | otherwise   = Just $ Sql.GroupBy { keys, having: Nothing }

groupByToOrderBy ∷ Sql.GroupBy Sql.Sql → Maybe (Sql.OrderBy Sql.Sql)
groupByToOrderBy (Sql.GroupBy { keys }) =
  case Tuple Sql.ASC <$> keys of
    head L.: tail → Just $ Sql.OrderBy $ NE.NonEmpty head tail
    _ → Nothing
