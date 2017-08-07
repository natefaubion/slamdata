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

module SlamData.Workspace.Card.Setups.Common.Sql (numFuncs) where

import SlamData.Prelude

import SqlSquared as Sql

val ∷ Sql.Sql
val = Sql.vari "val"

sqlOne ∷ Sql.Sql
sqlOne = Sql.int 1

sqlZero ∷ Sql.Sql
sqlZero = Sql.int 0

modded ∷ Sql.Sql
modded = Sql.binop Sql.Mod val sqlOne


tens ∷ Sql.Sql
tens = Sql.binop Sql.Pow (Sql.num 10.0) (Sql.vari "n")

tensBody ∷ String → Sql.Sql
tensBody funcName =
  Sql.binop Sql.Div
  ( Sql.invokeFunction funcName $ pure $ Sql.binop Sql.Mult val tens )
  tens

type Decl = Sql.SqlDeclF Sql.Sql

floor1Func ∷ Decl
floor1Func = Sql.FunctionDecl
  { ident: "FLOOR1"
  , args: pure "val"
  , body:
    let
      elseCase = Sql.binop Sql.Minus val modded
      ltCase = Sql.binop Sql.Minus (Sql.pars elseCase) sqlOne
    in
     Sql.switch
     ( foldMap pure
       [ Sql.when
         ( Sql.binop Sql.Eq (Sql.pars modded) sqlZero )
         # Sql.then_ val
       , Sql.when
         ( Sql.binop Sql.Lt val sqlZero )
         # Sql.then_ ltCase
       ] )
     ( pure elseCase )
  }

round1Func ∷ Decl
round1Func = Sql.FunctionDecl
  { ident: "ROUND1"
  , args: pure "val"
  , body: Sql.invokeFunction "FLOOR1" $ pure
    $ Sql.binop Sql.Plus (Sql.num 0.5) val
  }

ceil1Func ∷ Decl
ceil1Func = Sql.FunctionDecl
  { ident: "CEIL1"
  , args: pure "val"
  , body: Sql.invokeFunction "FLOOR1" $ pure
    $ Sql.binop Sql.Plus sqlOne val
  }

floor ∷ Decl
floor = Sql.FunctionDecl
  { ident: "FLOOR"
  , args: foldMap pure [ "val", "n" ]
  , body: tensBody "FLOOR1"
  }

ceil ∷ Decl
ceil = Sql.FunctionDecl
  { ident: "CEIL"
  , args: foldMap pure [ "val", "n" ]
  , body: tensBody "CEIL1"
  }

round ∷ Decl
round = Sql.FunctionDecl
  { ident: "ROUND"
  , args: foldMap pure [ "val", "n" ]
  , body: tensBody "ROUND1"
  }

numFuncs
  ∷ ∀ f
  . Monoid (f Decl)
  ⇒ Applicative f
  ⇒ f Decl
numFuncs =
  foldMap pure [ round1Func, ceil1Func, floor1Func, round, ceil, floor ]
