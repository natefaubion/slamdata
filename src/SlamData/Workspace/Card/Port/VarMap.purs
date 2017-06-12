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
  , Resource(..)
  , VarMapValue(..)
  , VarMapValues
  , empty
  , variables
  , insert
  , lookup
  , lookup'
  , toURLVarMap
  , uniqueVarName
  , snapshot
  , expand
  , markIndex
  , isResource
  , union
  ) where

import SlamData.Prelude
import Data.Array as A
import Data.List as L
import Data.List.NonEmpty as NL
import Data.Map as Map
import Data.StrMap as SM
import Data.Argonaut.Decode (class DecodeJson)
import Data.Argonaut.Encode (class EncodeJson)
import SlamData.Workspace.Card.CardId as CID
import SqlSquared (Sql)
import SqlSquared as Sql
import Test.StrongCheck.Arbitrary as SC
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

data VarMapValue
  = Expr Sql
  | Resource Resource
  | Union (Array VarMapValue)

derive instance eqVarMapValue ∷ Eq VarMapValue

instance semigroupVarMapValue ∷ Semigroup VarMapValue where
  append v1 v2 | v1 ≡ v2 = v1
  append (Union v1) (Union v2) = Union (v1 <> v2)
  append (Union v1) v2 = Union (A.snoc v1 v2)
  append v1 (Union v2) = Union (A.cons v1 v2)
  append v1 v2 = Union [ v1, v2 ]

type VarMap = SM.StrMap VarMapValues

type VarMapValues = NL.NonEmptyList (CID.CardId × VarMapValue)

-- | A VarMap passed through the URL - the VarMapValues are left unparsed until
-- | they are unified with the Variables card for the deck so that values can
-- | be parsed according to their defined type.
type URLVarMap = SM.StrMap String

empty ∷ VarMap
empty = SM.empty

variables ∷ VarMap → L.List Var
variables = L.fromFoldable ∘ map Var ∘ A.sort ∘ SM.keys

insert ∷ CID.CardId → Var → VarMapValue → VarMap → VarMap
insert cid (Var var) value = flip SM.alter var case _ of
  Just nl → Just $ NL.singleton (cid × value) <> nl
  Nothing → Just $ NL.singleton (cid × value)

lookup ∷ Var → VarMap → Maybe VarMapValue
lookup var = lookup' var >>> map snd

lookup' ∷ Var → VarMap → Maybe (CID.CardId × VarMapValue)
lookup' (Var var) = SM.lookup var >>> map NL.head

toURLVarMap ∷ VarMap → URLVarMap
toURLVarMap = SM.fromFoldable ∘ L.mapMaybe go ∘ expand
  where
  go { name, mark, value: Expr sql } =
    Just (uniqueVarName name mark × Sql.print sql)
  go _ = Nothing

uniqueVarName ∷ String → Int → String
uniqueVarName vari 0 = vari
uniqueVarName vari n = vari <> "__" <> show n

snapshot ∷ VarMap → SM.StrMap VarMapValue
snapshot = map (snd ∘ NL.head)

expand
  ∷ VarMap
  → L.List
      { name ∷ String
      , mark ∷ Int
      , source ∷ CID.CardId
      , value ∷ VarMapValue
      }
expand = uncurry go <=< SM.toUnfoldable
  where
  go name vals = do
    mark × source × value ←
      L.mapWithIndex Tuple
        $ L.reverse
        $ NL.toList vals
    pure { name, mark, source, value }

markIndex ∷ VarMap → Map.Map (CID.CardId × String) Int
markIndex = Map.fromFoldable ∘ map go ∘ expand
  where
  go { source, name, mark } = (source × name) × mark

isResource ∷ VarMapValue → Boolean
isResource (Resource _) = true
isResource _ = false

union ∷ CID.CardId → String → VarMap → VarMap → VarMap
union source namespace childMap intoMap = SM.fold foldFn intoMap childMap
  where
  foldFn ∷ VarMap → String → VarMapValues → VarMap
  foldFn varMap key values =
    SM.alter (Just ∘ unionFn values) (mungeKey key) varMap

  mungeKey ∷ String → String
  mungeKey
    | namespace ≡ "" = id
    | otherwise = \k → namespace <> "." <> k

  unionFn ∷ VarMapValues → Maybe VarMapValues → VarMapValues
  unionFn vs1 = maybe vs1 (unionValues vs1)

  unionValues ∷ VarMapValues → VarMapValues → VarMapValues
  unionValues vs1 vs2 =
    let
      { head: c1 × h1, tail: t1 } = NL.uncons vs1
      { head: c2 × h2, tail: t2 } = NL.uncons vs2
      c3 | c1 ≡ source = c1
         | c2 ≡ source = c2
         | otherwise = source
      h3 = h1 <> h2
      t3 = t1 <> t2
    in
      NL.appendFoldable (pure (c3 × h3)) t3
