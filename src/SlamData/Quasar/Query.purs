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

module SlamData.Quasar.Query
  ( compile
  , compile'
  , queryEJson
  , queryEJsonVM
  , query
  , viewQuery
  , viewQuery'
  , mountModule
  , mountModule'
  , all
  , sample
  , count
  , fileQuery
  , allFields
  , fields
  , jcursorToSql
  , module Quasar.Error
  , module SlamData.Quasar.Class
  ) where

import SlamData.Prelude

import Data.Argonaut as JS
import Data.Array as Arr
import Data.HugeInt as HI
import Data.Int as Int
import Data.Json.Extended as EJS
import Data.Lens ((.~))
import Data.List as L
import Data.Path.Pathy as P
import Data.Set as Set
import Data.StrMap as SM
import Matryoshka (Coalgebra, ana, project, embed)
import Quasar.Advanced.QuasarAF as QF
import Quasar.Data.Json as QJ
import Quasar.Error (QError)
import Quasar.Mount as QM
import Quasar.Types (DirPath, FilePath, CompileResultR)
import SlamData.Quasar.Class (class QuasarDSL, liftQuasar)
import SqlSquared (SqlModule, printModule, SqlQuery, printQuery, Sql, print)
import SqlSquared as Sql
import Utils.SqlSquared (tableRelation)

-- | Compiles a query.
compile
  ∷ ∀ m
  . QuasarDSL m
  ⇒ DirPath
  → SqlQuery
  → SM.StrMap String
  → m (Either QError CompileResultR)
compile backendPath sql varMap =
  compile' backendPath (printQuery sql) varMap

compile'
  ∷ ∀ m
  . QuasarDSL m
  ⇒ DirPath
  → String
  → SM.StrMap String
  → m (Either QError CompileResultR)
compile' backendPath sql varMap =
  liftQuasar $ QF.compileQuery backendPath sql varMap

query
  ∷ ∀ m
  . QuasarDSL m
  ⇒ DirPath
  → SqlQuery
  → m (Either QError JS.JArray)
query path sql =
  liftQuasar $ QF.readQuery QJ.Readable path (printQuery sql) SM.empty Nothing

queryEJson
  ∷ ∀ m
  . QuasarDSL m
  ⇒ DirPath
  → SqlQuery
  → m (Either QError (Array EJS.EJson))
queryEJson path sql =
  liftQuasar $ QF.readQueryEJson path (printQuery sql) SM.empty Nothing

queryEJsonVM
  ∷ ∀ m
  . QuasarDSL m
  ⇒ DirPath
  → SqlQuery
  → SM.StrMap String
  → m (Either QError (Array EJS.EJson))
queryEJsonVM path sql vm =
  liftQuasar $ QF.readQueryEJson path (printQuery sql) vm Nothing

-- | Runs a query creating a view mount for the query.
viewQuery
  ∷ ∀ m
  . QuasarDSL m
  ⇒ Monad m
  ⇒ FilePath
  → SqlQuery
  → SM.StrMap String
  → m (Either QError Unit)
viewQuery dest sql vars = viewQuery' dest (printQuery sql) vars

-- | Runs a query creating a view mount for the query.
viewQuery'
  ∷ ∀ m
  . QuasarDSL m
  ⇒ Monad m
  ⇒ FilePath
  → String
  → SM.StrMap String
  → m (Either QError Unit)
viewQuery' dest sql vars = do
  _ ← liftQuasar $ QF.deleteMount (Right dest)
  liftQuasar $
    QF.updateMount (Right dest) (QM.ViewConfig
      { query: sql
      , vars
      })

mountModule
  ∷ ∀ m
  . QuasarDSL m
  ⇒ Monad m
  ⇒ DirPath
  → SqlModule
  → m (Either QError Unit)
mountModule dest sql = mountModule' dest (printModule sql)

mountModule'
  ∷ ∀ m
  . QuasarDSL m
  ⇒ Monad m
  ⇒ DirPath
  → String
  → m (Either QError Unit)
mountModule' dest sql = do
  _ ← liftQuasar $ QF.deleteMount (Left dest)
  liftQuasar $ QF.updateMount (Left dest) (QM.ModuleConfig { "module": sql })

fileQuery
  ∷ ∀ m
  . QuasarDSL m
  ⇒ DirPath
  → FilePath
  → SqlQuery
  → SM.StrMap String
  → m (Either QError FilePath)
fileQuery backendPath dest sql vars =
  liftQuasar $ map _.out <$>
    QF.writeQuery backendPath dest (printQuery sql) vars

all
  ∷ ∀ m
  . QuasarDSL m
  ⇒ FilePath
  → m (Either QError JS.JArray)
all file =
  liftQuasar $ QF.readFile QJ.Readable file Nothing

sample
  ∷ ∀ m
  . QuasarDSL m
  ⇒ FilePath
  → Int
  → Int
  → m (Either QError JS.JArray)
sample file offset limit =
  liftQuasar $ QF.readFile QJ.Readable file (Just { limit, offset })

count
  ∷ ∀ m
  . Monad m
  ⇒ QuasarDSL m
  ⇒ FilePath
  → m (Either QError Int)
count file = runExceptT do
  let
    backendPath = fromMaybe P.rootDir (P.parentDir file)
    sql =
      Sql.buildSelect
      $ (Sql._projections
         .~ (L.singleton
               $ Sql.projection
                   (Sql.invokeFunction "COUNT" $ L.singleton $ Sql.splice Nothing)
                   #  Sql.as "total"))
      ∘ (Sql._relations .~ tableRelation (Left file))
  result ← ExceptT $ liftQuasar $
    QF.readQuery QJ.Readable backendPath (print sql) SM.empty Nothing
  pure $ fromMaybe 0 (readTotal result)
  where
  readTotal ∷ JS.JArray → Maybe Int
  readTotal =
    Int.fromNumber
      <=< JS.toNumber
      <=< SM.lookup "total"
      <=< JS.toObject
      <=< Arr.head

data UnfoldableJC = JC JS.JCursor | S String | I Int

jcCoalgebra ∷ (∀ a. Maybe (Sql.SqlF EJS.EJsonF a)) → Coalgebra (Sql.SqlF EJS.EJsonF) UnfoldableJC
jcCoalgebra root = case _ of
  S s → Sql.Ident s
  I i → Sql.Literal (EJS.Integer (HI.fromInt i))
  JC cursor → case cursor of
    JS.JCursorTop → fromMaybe (Sql.Splice Nothing) root
    JS.JIndex i c → Sql.Binop { op: Sql.IndexDeref, lhs: JC c, rhs: I i }
    JS.JField f c → Sql.Binop { op: Sql.FieldDeref, lhs: JC c, rhs: S f }

removeTopSplice ∷ Sql → Sql
removeTopSplice = project ⋙ case _ of
  op@(Sql.Binop { lhs, rhs }) → case project lhs of
    Sql.Splice Nothing → rhs
    _ → embed op
  a → embed a

jcursorToSql ∷ (∀ a. Maybe (Sql.SqlF EJS.EJsonF a))  → JS.JCursor → Sql
jcursorToSql root = removeTopSplice ∘ ana (jcCoalgebra root) ∘ JC ∘ JS.insideOut

allFields ∷ JS.JArray → L.List Sql
allFields =
  map (jcursorToSql Nothing)
  ∘ L.fromFoldable
  ∘ foldMap (Set.fromFoldable ∘ map fst)
  ∘ map JS.toPrims

fields
  ∷ ∀ m
  . Monad m
  ⇒ QuasarDSL m
  ⇒ FilePath
  → m (QError ⊹ (L.List Sql))
fields file = runExceptT do
  jarr ← ExceptT $ sample file 0 100
  pure $ allFields jarr
