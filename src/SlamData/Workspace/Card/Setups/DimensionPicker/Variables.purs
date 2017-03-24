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

module SlamData.Workspace.Card.Setups.DimensionPicker.Variables where

import SlamData.Prelude
import Control.Comonad.Cofree as CF
import Data.Argonaut (JCursor)
import Data.List as L
import SlamData.Workspace.Card.Port.VarMap as VM
import SlamData.Workspace.Card.Setups.Chart.PivotTable.Model (Column)

type Variables = Either VM.Var

type VariablesNode a = Either (Variables a) (Variables a)

type VariablesJCursorNode = VariablesNode JCursor

type VariablesColumnNode = VariablesNode Column

groupVariables
  ∷ ∀ a
  . L.List VM.Var
  → CF.Cofree L.List (Either a a)
  → CF.Cofree L.List (VariablesNode a)
groupVariables vars as = withVars
  where
  as' = bimap Right Right <$> as
  withVars
    | L.null vars = as'
    | otherwise =
        let
          head = CF.head as'
          tail = CF.tail as'
          tree =
            CF.mkCofree
              (Left (Left (VM.Var "Local variables")))
              (flip CF.mkCofree mempty ∘ Right ∘ Left <$> vars)
        in
          CF.mkCofree head (L.Cons tree tail)

flattenVariables ∷ ∀ a. VariablesNode a → Variables a
flattenVariables = either id id
