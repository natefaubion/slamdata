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

module SlamData.Workspace.Card.Port.VarMap
  ( Var(..)
  , VarMap
  , URLVarMap
  , URLVarMapValue
  , urlVarMapValue
  , unURLVarMapValue
  , unURLVarMapF
  , Resource(..)
  , VarMapValue(..)
  , VarMapPtr
  , Mark
  , empty
  , variables
  , insert
  , member
  , lookup
  , lookup'
  , lookupPtr
  , lookupValue
  , lookupMark
  , toURLVarMap
  , uniqueVarName
  , expand
  , snapshot
  , varNames
  , fromFoldable
  , isResource
  , isDynamic
  , union
  , downloadUrl
  ) where

import SlamData.Prelude hiding (empty, expand)
import Data.Argonaut.Decode (class DecodeJson)
import Data.Argonaut.Encode (class EncodeJson)
import Data.Array as A
import Data.List as L
import Data.Map as Map
import Data.Path.Pathy as Path
import Data.StrMap as SM
import Data.URI as URI
import Quasar.Paths as Paths
import SlamData.Workspace.Card.CardId as CID
import SqlSquared (Sql)
import SqlSquared as Sql
import Test.StrongCheck.Arbitrary as SC
import Unsafe.Coerce (unsafeCoerce)
import Utils.Path as PU

newtype Var = Var String

derive newtype instance eqVar ∷ Eq Var
derive newtype instance ordVar ∷ Ord Var
derive newtype instance decodeVar ∷ DecodeJson Var
derive newtype instance encodeVar ∷ EncodeJson Var
derive newtype instance arbitraryVar ∷ SC.Arbitrary Var
derive instance newtypeVar :: Newtype Var _

data Resource
  = Path PU.FilePath
  | View PU.RelFilePath Sql.SqlQuery VarMap
  | Process PU.RelFilePath Sql.SqlModule VarMap

derive instance eqResource ∷ Eq Resource

instance showResource ∷ Show Resource where
  show (Path path) = "(Path " <> Path.printPath path <> ")"
  show (View path _ _) = "(View " <> Path.printPath path <> ")"
  show (Process path _ _) = "(Process " <> Path.printPath path <> ")"

data VarMapValue
  = Expr Sql
  | Resource Resource
  | Union (Array VarMapPtr)

derive instance eqVarMapValue ∷ Eq VarMapValue

type Mark = Int

type VarMapPtr = Var × CID.CardId

newtype VarMap = VarMap
  { heap ∷ Map.Map Var (Map.Map CID.CardId VarMapValue)
  , scope ∷ Map.Map Var VarMapPtr
  }

derive instance eqVarMap ∷ Eq VarMap

newtype URLVarMapValue = URLVarMapValue String

derive newtype instance decodeJsonURLVarMapValue ∷ DecodeJson URLVarMapValue
derive newtype instance encodeJsonURLVarMapValue ∷ EncodeJson URLVarMapValue
derive newtype instance eqURLVarMapValue ∷ Eq URLVarMapValue

urlVarMapValue ∷ VarMapValue → Maybe URLVarMapValue
urlVarMapValue = case _ of
  Expr sql → Just $ URLVarMapValue $ Sql.print sql
  _ → Nothing

unURLVarMapValue ∷ URLVarMapValue → String
unURLVarMapValue (URLVarMapValue s) = s

-- | A VarMap passed through the URL - the VarMapValues are left unparsed until
-- | they are unified with the Variables card for the deck so that values can
-- | be parsed according to their defined type.
type URLVarMap = SM.StrMap URLVarMapValue

empty ∷ VarMap
empty = VarMap { heap: Map.empty, scope: Map.empty }

isEmpty ∷ VarMap → Boolean
isEmpty (VarMap vm) = Map.isEmpty vm.heap

variables ∷ VarMap → L.List Var
variables (VarMap { scope }) = Map.keys scope

insert ∷ CID.CardId → Var → VarMapValue → VarMap → VarMap
insert cid var value (VarMap vm) =
  let
    ptr = var × cid
    scope' = Map.insert var (var × cid) vm.scope
    heap' = Map.alter alterFn var vm.heap
    alterFn = case _ of
      Nothing → Just (Map.singleton cid value)
      Just m  → Just (Map.insert cid value m)
  in
    VarMap { heap: heap', scope: scope' }

member ∷ Var → VarMap → Boolean
member var (VarMap vm) = Map.member var vm.scope

varNames ∷ VarMap → Array String
varNames (VarMap vm) = unwrap ∘ fst <$> Map.toUnfoldable vm.scope

lookup ∷ Var → VarMap → Maybe VarMapValue
lookup var vm = do
  ptr ← lookupPtr var vm
  lookupValue ptr vm

lookup' ∷ Var → VarMap → Maybe (CID.CardId × VarMapValue)
lookup' var vm = do
  ptr ← lookupPtr var vm
  ref ← lookupValue ptr vm
  pure (snd ptr × ref)

lookupPtr ∷ Var → VarMap → Maybe VarMapPtr
lookupPtr var (VarMap vm) = Map.lookup var vm.scope

lookupValue ∷ VarMapPtr → VarMap → Maybe VarMapValue
lookupValue ptr (VarMap vm) = do
  vals ← Map.lookup (fst ptr) vm.heap
  Map.lookup (snd ptr) vals

lookupMark ∷ VarMapPtr → VarMap → Maybe Int
lookupMark ptr (VarMap vm) = do
  vals ← asList ∘ Map.toUnfoldable <$> Map.lookup (fst ptr) vm.heap
  L.findIndex (eq (snd ptr) ∘ fst) vals

toURLVarMap ∷ VarMap → URLVarMap
toURLVarMap = SM.fromFoldable ∘ L.mapMaybe go ∘ expand
  where
  go { name, mark, value } =
    Tuple (uniqueVarName name mark) <$> urlVarMapValue value

unURLVarMapF ∷ ∀ f. Functor f => f URLVarMapValue → f String
unURLVarMapF = unsafeCoerce

uniqueVarName ∷ String → Int → String
uniqueVarName vari 0 = vari
uniqueVarName vari n = vari <> "__" <> show n

snapshot ∷ VarMap → Map.Map Var VarMapValue
snapshot (VarMap vm) = fromMaybe Map.empty $ traverse (flip lookupValue (VarMap vm)) vm.scope

expand
  ∷ VarMap
  → L.List
      { name ∷ String
      , mark ∷ Int
      , source ∷ CID.CardId
      , value ∷ VarMapValue
      }
expand (VarMap vm) = join $ uncurry go <$> Map.toUnfoldable (Map.toUnfoldable <$> vm.heap)
  where
  go (Var name) =
    L.mapWithIndex \mark (source × value) →
      { name, mark, source, value }

isResource ∷ VarMapValue → Boolean
isResource (Resource _) = true
isResource _ = false

isDynamic ∷ VarMapValue → Boolean
isDynamic (Expr sql) = true
isDynamic _ = false

fromFoldable ∷ ∀ f. Foldable f ⇒ f (CID.CardId × Var × VarMapValue) → VarMap
fromFoldable = foldl (\vm (cid × var × val) → insert cid var val vm) empty

union ∷ CID.CardId → String → VarMap → VarMap → VarMap
union source namespace childVarMap@(VarMap child) intoVarMap@(VarMap into) =
  let
    -- If we have a namespace, we should rename all the variables in the scope.
    -- They still point to the same references though.
    childScope =
      if namespace ≡ ""
        then child.scope # Map.toUnfoldable
        else child.scope
          # Map.toUnfoldable
          # asArray
          # map (\(Var k × v) → Var (namespace <> "." <> k) × v)

    intoHeap =
      Map.unionWith Map.union child.heap into.heap

    initVarMap =
      VarMap { heap: intoHeap, scope: into.scope }
  in
    -- When we go to union two VarMapsValues, we need to create a fresh binding
    -- for the card in the value map. Otherwise it can just point to the old
    -- value.
    foldl
      case _, _ of
        accVarMap, childVar × childPtr
          | Just intoPtr ← lookupPtr childVar intoVarMap
          , childPtr ≠ intoPtr
          , Just childVal ← lookupValue childPtr childVarMap
          , Just intoVal ← lookupValue intoPtr intoVarMap
          →
            let
              newVal = case childVal, intoVal of
                Union a1, Union a2 → Union (a1 <> a2)
                Union a1, _ → Union (A.snoc a1 intoPtr)
                _, Union a2 → Union (A.cons childPtr a2)
                _, _ → Union [ childPtr, intoPtr ]
            in
              insert source childVar newVal accVarMap
        accVarMap, childVar × childPtr
          | Just childVal ← lookupValue childPtr childVarMap
          → insert (snd childPtr) childVar childVal accVarMap
        accVarMap, _ → accVarMap
    initVarMap
    childScope

downloadUrl ∷ Maybe String → Path.AbsDir Path.Sandboxed → Resource → URI.AbsoluteURI
downloadUrl headers context r =
  URI.AbsoluteURI Nothing
    (URI.HierarchicalPart Nothing (Just (Right path)))
    (URI.Query <$> query')
  where
  path = Path.rootDir Path.</> case r of
    Path p → Paths.data_ Path.</> PU.absToRelative p
    View p _ _ → Paths.data_ Path.</> (PU.absToRelative context Path.</> PU.tmpDir Path.</> p)
    Process p _ _ → Paths.invoke Path.</> (PU.absToRelative context Path.</> PU.tmpDir Path.</> p)

  query = case r of
    Process _ _ vm
      | not (isEmpty vm)
      → Just (map (Just ∘ unURLVarMapValue) <$> SM.toUnfoldable (toURLVarMap vm))
    _ → Nothing

  query' =
    query <> (pure ∘ Tuple "request-headers" ∘ Just <$> headers)
