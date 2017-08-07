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

module SlamData.Workspace.Card.Eval.Process
  ( elaborate
  , elaborateQuery
  , processIdent
  , ImportBaseDir
  ) where

import SlamData.Prelude
import Control.Monad.RWS as RWS
import Control.Monad.State as State
import Control.Monad.State.Class (class MonadState)
import Control.Monad.Writer.Class (class MonadWriter)
import Data.Foldable as F
import Data.List as L
import Data.List.NonEmpty as NL
import Data.Map as Map
import Data.Path.Pathy as Path
import Data.Set as Set
import Data.String as String
import Matryoshka (cataM, embed)
import SlamData.Workspace.Card.CardId as CID
import SlamData.Workspace.Card.Port as Port
import SlamData.Workspace.Card.Port.VarMap as VM
import SqlSquared as Sql

-- | Process elaboration is a transformation from a SQL^2 Query to a
-- | SQL^2 Module based on a VarMap. The reason we do this is because a Query
-- | might contain references to other Resources which may not refer to a
-- | static path on disk, but to a Process/Function call. The goals of
-- | elaboration are to:
-- |
-- | * Determine if a Query should be a Module. A Query should be a Module if
-- |   it contains "true" parameterization. True parameterization occurs when
-- |   the query contains references a URL VarMap binding or another Process.
-- |   Even though a Query may reference something like `:resource`, it may not
-- |   be truly parameterized if that variable refers to a static path. In such
-- |   a case, the path is instead inlined and does not qualify as true
-- |   parameterization.
-- | * Dereference Processes as a SQL^2 import and a call to the specified
-- |   function. We also alias these calls in a local temporary binding.
-- | * Track variable hygiene. A card at the beginning of the stack may
-- |   reference a URL variable, but that variable may be shadowed by a
-- |   subsequent card. To the end user they have the same name, but we still
-- |   must pass in both references when executing the query.
-- | * Elaborate Function declarations in Query cards to take free variables
-- |   as arguments. When a Query is turned into a Module, the "index" function
-- |   must take all free variables and pass them to the functions since they
-- |   can't just simply be templated. TODO: Can this be elided if all variables
-- |   are renamed to be uniquely addressed?

type Elaborate m a
  = MonadReader RewriteEnv m
  ⇒ MonadState RewriteState m
  ⇒ MonadWriter RewriteLog m
  ⇒ a

type VarMapValues =
  NL.NonEmptyList (CID.CardId × VM.VarMapValue)

type RewriteLog = Set.Set String

type ImportBaseDir = Path.RelDir Path.Unsandboxed

type RewriteState =
  { fresh ∷ Int
  , decls ∷ L.List (Sql.SqlDeclF Sql.Sql)
  , bindings ∷ L.List (String × Sql.Sql)
  , aliases ∷ Map.Map (CID.CardId × String) String
  , freeVars ∷ Map.Map String (L.List String)
  }

type RewriteEnv =
  { path ∷ ImportBaseDir
  , varMap ∷ Port.VarMap
  , varScope ∷ Set.Set String
  , shouldAlias ∷ Boolean
  }

type RewriteM =
  RWS.RWS RewriteEnv RewriteLog RewriteState

initialEnv ∷ ImportBaseDir → VM.VarMap → RewriteEnv
initialEnv path varMap =
  { path
  , varMap
  , varScope: Set.empty
  , shouldAlias: true
  }

initialState ∷ RewriteState
initialState =
  { fresh: 0
  , decls: mempty
  , bindings: mempty
  , aliases: mempty
  , freeVars: mempty
  }

elaborate
  ∷ ImportBaseDir
  → ImportBaseDir
  → CID.CardId
  → VM.VarMap
  → Sql.SqlQuery
  → Path.FileName × Either Sql.SqlQuery Sql.SqlModule
elaborate viewPath modPath currentSource varMap query =
  case RWS.runRWS go (initialEnv path varMap) initialState of
    RWS.RWSResult st res _ →
      Path.FileName selfName × res

  where
  isProcess ∷ Boolean
  isProcess = checkIsProcess varMap query

  path ∷ ImportBaseDir
  path = if isProcess then modPath else viewPath

  selfName ∷ String
  selfName = processIdent currentSource

  go ∷ RewriteM (Either Sql.SqlQuery Sql.SqlModule)
  go = do
    Sql.Query decls sql ← elaborateQuery' query
    st ← RWS.get
    vars ← (processVars <<< _.varMap) =<< ask
    pure if isProcess
      then Right $ sqlModule
        (st.decls <> decls)
        selfName
        (uncurry VM.uniqueVarName ∘ snd <$> vars)
        st.bindings
        sql
      else Left $ Sql.Query decls sql

checkIsProcess ∷ VM.VarMap → Sql.SqlQuery → Boolean
checkIsProcess varMap (Sql.Query decls expr) =
  F.or (L.Cons (goExpr Set.empty expr) $ map goDecl decls)
  where
  goDecl = case _ of
    Sql.Import a → false
    Sql.FunctionDecl { ident, args, body } →
      goExpr (Set.fromFoldable args) body

  goExpr scope = flip State.execState false ∘ cataM case _ of
    Sql.Vari vari → checkVari scope vari
    Sql.Select { relations: Just (Sql.VariRelation { vari }) } → checkVari scope vari
    _ → pure unit

  checkVari scope vari
    | Set.member vari scope = pure unit
    | otherwise = for_ (VM.lookup (VM.Var vari) varMap) checkValue

  checkValue = case _ of
    VM.Expr _ → State.put true
    VM.Resource (VM.Process _ _ _) → State.put true
    VM.Union u → for_ u \ptr → for_ (VM.lookupValue ptr varMap) checkValue
    _ → pure unit

elaborateQuery
  ∷ ImportBaseDir
  → VM.VarMap
  → Sql.SqlQuery
  → Sql.SqlQuery
elaborateQuery path varMap query =
  case RWS.runRWS go (initialEnv path varMap) initialState of
    RWS.RWSResult _ res _ → res

  where
  go ∷ RewriteM Sql.SqlQuery
  go = do
    Sql.Query decls sql ← elaborateQuery' query
    st ← RWS.get
    pure $ Sql.Query
      (st.decls <> decls)
      (foldBindings sql st.bindings)

elaborateQuery' ∷ ∀ m. Elaborate m (Sql.SqlQuery → m Sql.SqlQuery)
elaborateQuery' (Sql.Query decls expr) =
  Sql.Query
    <$> traverse elaborateDecl decls
    <*> elaborateExpr expr

elaborateDecl ∷ ∀ m. Elaborate m (Sql.SqlDeclF Sql.Sql → m (Sql.SqlDeclF Sql.Sql))
elaborateDecl = case _ of
  Sql.Import a → pure $ Sql.Import a
  Sql.FunctionDecl { ident, args, body } → do
    body' × freeVars ←
      RWS.listen $ RWS.local
        (\env → env
          { varScope = foldr Set.insert env.varScope args
          , shouldAlias = false
          })
        (elaborateExpr body)
    let
      freeVars' = L.fromFoldable freeVars
    if L.null freeVars'
      then
        pure $ Sql.FunctionDecl { ident, args, body: body' }
      else do
        addFreeVars ident freeVars'
        pure $ Sql.FunctionDecl { ident, args: freeVars' <> args, body: body' }

elaborateExpr ∷ ∀ m. Elaborate m (Sql.Sql → m Sql.Sql)
elaborateExpr = cataM case _ of
  Sql.Vari vari → elaborateVar vari
  Sql.InvokeFunction { name, args } → substInvoke name args
  Sql.Select sel@({ relations: Just (Sql.VariRelation { vari, alias }) }) →
    substSelect sel vari alias
  sql → pure $ embed sql

  where
  substSelect ∷ Sql.SelectR Sql.Sql → String → Maybe String → m Sql.Sql
  substSelect sel vari alias = do
    subst ← elaborateVar vari
    { bindings } ← RWS.get
    relations ← case unwrap subst of
      Sql.Vari vari' →
        pure $ Just $ Sql.VariRelation { vari: vari', alias }
      Sql.Ident i | not (F.any (eq i ∘ fst) bindings), Just path ← parseTablePath i →
        pure $ Just $ Sql.TableRelation { path, alias }
      expr → do
        aliasName ← maybe tmpName pure alias
        pure $ Just $ Sql.ExprRelation { expr: embed expr, aliasName }
    pure $ embed $ Sql.Select sel { relations = relations }

  substInvoke ∷ String → L.List Sql.Sql → m Sql.Sql
  substInvoke name args = do
    st ← RWS.get
    { shouldAlias } ← ask
    args' ← case Map.lookup name st.freeVars of
      Just freeVars | shouldAlias → do
        freeVars' ← traverse elaborateVar freeVars
        pure (freeVars' <> args)
      Just freeVars → pure (map Sql.vari freeVars <> args)
      Nothing → pure args
    pure $ embed $ Sql.InvokeFunction { name, args: args' }

  parseTablePath ∷ String → Maybe (Either (Path.AbsFile Path.Unsandboxed) (Path.RelFile Path.Unsandboxed))
  parseTablePath str = Left <$> Path.parseAbsFile str <|> Right <$> Path.parseRelFile str

elaborateVar ∷ ∀ m. Elaborate m (String → m Sql.Sql)
elaborateVar vari = do
  { varMap, varScope } ← ask
  if Set.member vari varScope
    then pure $ Sql.vari vari
    else maybe (defaultSub vari) substPtr $ VM.lookupPtr (VM.Var vari) varMap

substPtr ∷ ∀ m. Elaborate m (VM.VarMapPtr → m Sql.Sql)
substPtr ptr@(VM.Var vari × source) = do
  { varMap } ← ask
  maybe (defaultSub vari) substValue $ VM.lookupValue ptr varMap

  where
  substValue ∷ VM.VarMapValue → m Sql.Sql
  substValue value = do
    RWS.tell $ Set.singleton vari
    case value of
      VM.Expr expr → substExpr expr
      VM.Resource res → substResource res
      VM.Union ptrs → substUnion ptr ptrs

  substExpr ∷ Sql.Sql → m Sql.Sql
  substExpr sql = do
    { varMap } ← ask
    case VM.lookupMark ptr varMap of
      Just mark → pure $ Sql.vari $ VM.uniqueVarName vari mark
      Nothing → defaultSub vari

  substResource ∷ VM.Resource → m Sql.Sql
  substResource = case _ of
    VM.Path filePath → pure $ sqlPath filePath
    VM.View filePath _ _ → sqlPath <$> basePath filePath
    VM.Process filePath _ localVarMap → do
      { shouldAlias } ← ask
      if shouldAlias
        then do
          filePath' ← basePath filePath
          substProcessVar source filePath' localVarMap vari
        else defaultSub vari

substUnion ∷ ∀ m. Elaborate m (VM.VarMapPtr → Array VM.VarMapPtr → m Sql.Sql)
substUnion ptr@(VM.Var vari × source) ptrs = do
  exprs ← traverse substPtr ptrs
  name ← addAlias source vari $ embed $ Sql.SetLiteral $ L.fromFoldable exprs
  pure $ Sql.ident name

substProcessVar ∷ ∀ m a c. Elaborate m (CID.CardId → Path.Path a Path.File c → VM.VarMap → String → m Sql.Sql)
substProcessVar source filePath localVarMap vari = do
  { aliases } ← RWS.get
  maybe genAlias (pure ∘ Sql.ident) $
    Map.lookup (source × vari) aliases

  where
  genAlias ∷ m Sql.Sql
  genAlias = case Path.peel filePath of
    Just (dir × Right fileName) → do
      addProcessImport dir
      expr ← invokeProcess fileName localVarMap
      Sql.ident <$> addAlias source vari expr
    _ → defaultSub vari

invokeProcess ∷ ∀ m. Elaborate m (Path.FileName → VM.VarMap → m Sql.Sql)
invokeProcess (Path.FileName fileName) localVarMap = do
  { aliases } ← RWS.get
  vars ← processVars localVarMap
  let
    args ∷ L.List Sql.Sql
    args = vars <#> \(source × vari × mark) →
      case Map.lookup (source × vari) aliases of
        Just name → Sql.ident name
        Nothing → Sql.vari $ VM.uniqueVarName vari mark
  pure $ embed $ Sql.InvokeFunction { name: fileName, args }

processVars ∷ ∀ m. Elaborate m (VM.VarMap → m (L.List (CID.CardId × String × Int)))
processVars localVarMap = do
  { varMap } ← ask
  pure $ L.mapMaybe
    (\{ source, name, value } → do
      guard $ VM.isDynamic value
      mark ← VM.lookupMark (VM.Var name × source) varMap
      pure (source × name × mark))
    (VM.expand localVarMap)

processIdent ∷ CID.CardId → String
processIdent cid =
  "card" <> String.replaceAll
    (String.Pattern "-")
    (String.Replacement "")
    (CID.toString cid)

sqlPath ∷ ∀ a b c. Path.Path a b c → Sql.Sql
sqlPath = embed ∘ Sql.Ident ∘ Path.unsafePrintPath

basePath ∷ ∀ m b c. Elaborate m (Path.Path Path.Rel b c → m (Path.Path Path.Rel b Path.Unsandboxed))
basePath p = do
  { path } ← ask
  pure (path Path.</> Path.unsandbox p)

sqlModule
  ∷ L.List (Sql.SqlDeclF Sql.Sql)
  → String
  → L.List String
  → L.List (String × Sql.Sql)
  → Sql.Sql
  → Sql.SqlModule
sqlModule decls name args bindings expr =
  Sql.Module
    $ L.snoc decls
    $ Sql.FunctionDecl { ident: name, args, body }
  where
  body = foldBindings expr bindings

foldBindings ∷ ∀ f. Foldable f ⇒ Sql.Sql → f (String × Sql.Sql) → Sql.Sql
foldBindings = foldl \in_ (ident × bindTo) → embed $ Sql.Let { ident, bindTo, in_ }

tmpName ∷ ∀ m. MonadState RewriteState m ⇒ m String
tmpName = RWS.state \st → "tmp__" <> show st.fresh × st { fresh = st.fresh + 1 }

addAlias ∷ ∀ m. MonadState RewriteState m ⇒ CID.CardId → String → Sql.Sql → m String
addAlias source old sql = do
  tmp ← tmpName
  RWS.modify \st → st
    { bindings = L.Cons (tmp × sql) st.bindings
    , aliases = Map.insert (source × old) tmp st.aliases
    }
  pure tmp

addProcessImport ∷ ∀ m a b c. MonadState RewriteState m ⇒ Path.Path a b c → m Unit
addProcessImport dir = RWS.modify \st → st { decls = L.Cons (Sql.Import (Path.unsafePrintPath dir)) st.decls }

addFreeVars ∷ ∀ m. MonadState RewriteState m ⇒ String → L.List String → m Unit
addFreeVars key value = RWS.modify \st → st { freeVars = Map.insert key value st.freeVars }

defaultSub ∷ ∀ m. Elaborate m (String → m Sql.Sql)
defaultSub vari = pure $ Sql.vari $ "Bad substitution: " <> vari
