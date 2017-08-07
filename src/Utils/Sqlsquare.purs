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

import Data.Lens ((.~))
import Data.Path.Pathy (unsandbox)
import Quasar.Types (FilePath)
import SqlSquared as Sql
import Utils.Path (AnyFilePath)

tableRelation ∷ AnyFilePath → Maybe (Sql.Relation Sql.Sql)
tableRelation path =
  Just $ Sql.TableRelation { alias: Nothing, path: bimap unsandbox unsandbox path }

variRelation ∷ ∀ a. String → Sql.Relation a
variRelation vari = Sql.VariRelation { alias: Nothing, vari }

all ∷ Sql.SelectR Sql.Sql → Sql.SelectR Sql.Sql
all =
  Sql._projections .~ (pure $ Sql.projection (Sql.splice Nothing))

asRel ∷ String → Sql.Relation Sql.Sql → Sql.Relation Sql.Sql
asRel a = case _ of
  Sql.TableRelation r → Sql.TableRelation r { alias = Just a }
  Sql.ExprRelation r → Sql.ExprRelation r { aliasName = a }
  Sql.JoinRelation r → Sql.JoinRelation r
  Sql.VariRelation r → Sql.VariRelation r { alias = Just a }

selectStar ∷ FilePath → Sql.Sql
selectStar path =
  Sql.select
    false
    [ Sql.projection (Sql.splice Nothing)
    ]
    ( Just $ Sql.TableRelation
        { alias: Nothing
        , path: Left $ unsandbox path
        })
    Nothing
    Nothing
    Nothing
