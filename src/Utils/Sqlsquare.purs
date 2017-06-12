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

module Utils.SqlSquared where

import SlamData.Prelude
import Control.Monad.Writer (Writer, runWriter, execWriter, tell)
import Data.Json.Extended (EJsonF)
import Data.Lens ((.~))
import Data.Functor.Mu (roll)
import Data.Set as Set
import Matryoshka (cataM)
import SqlSquared as Sql
import Utils.Path (AnyFilePath)

tableRelation ∷ AnyFilePath → Maybe (Sql.Relation Sql.Sql)
tableRelation path =
  Just $ Sql.TableRelation { alias: Nothing, path }

all ∷ Sql.SelectR Sql.Sql → Sql.SelectR Sql.Sql
all =
  Sql._projections .~ (pure $ Sql.projection (Sql.splice Nothing))

asRel ∷ String → Sql.Relation Sql.Sql → Sql.Relation Sql.Sql
asRel a = case _ of
  Sql.TableRelation r → Sql.TableRelation r { alias = Just a }
  Sql.ExprRelation r → Sql.ExprRelation r { aliasName = a }
  Sql.JoinRelation r → Sql.JoinRelation r
  Sql.VariRelation r → Sql.VariRelation r { alias = Just a }

freeVariables ∷ Sql.SqlQuery → Set.Set String
freeVariables (Sql.Query decls expr) =
  foldMap forDecl decls <> forExpr Set.empty expr
  where
  forDecl (Sql.Import _) = Set.empty
  forDecl (Sql.FunctionDecl { args, body }) =
    forExpr (Set.fromFoldable args) body

  forExpr vars = execWriter ∘ cataM case _ of
    Sql.Vari vari
      | not (Set.member vari vars) → tell $ Set.singleton vari
    Sql.Select { relations: Just (Sql.VariRelation { vari }) }
      | not (Set.member vari vars) → tell $ Set.singleton vari
    _ → pure unit

substFreeVarsQuery
  ∷ (Sql.SqlF EJsonF Sql.Sql → Maybe Sql.Sql)
  → Sql.SqlQuery
  → Sql.SqlQuery × Set.Set String
substFreeVarsQuery f (Sql.Query decls expr) =
  runWriter $ Sql.Query
    <$> traverse goDecl decls
    <*> substFreeVarsExpr f Set.empty expr
  where
  goDecl = case _ of
    a@(Sql.Import _) → pure a
    Sql.FunctionDecl { ident, args, body } → do
      body' ← substFreeVarsExpr f (Set.fromFoldable args) body
      pure $ Sql.FunctionDecl { ident, args, body: body' }

substFreeVarsExpr
  ∷ (Sql.SqlF EJsonF Sql.Sql → Maybe Sql.Sql)
  → Set.Set String
  → Sql.Sql
  → Writer (Set.Set String) Sql.Sql
substFreeVarsExpr f scope =
  cataM \sql → case sql of
    Sql.Vari vari
      | not (Set.member vari scope) → go sql vari
    Sql.Select { relations: Just (Sql.VariRelation { vari }) }
      | not (Set.member vari scope) → go sql vari
    _ → pure $ roll sql
  where
  go sql vari = case f sql of
    Just sql' → pure sql'
    Nothing → tell (Set.singleton vari) $> roll sql

