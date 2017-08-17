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
module SlamData.Workspace.Card.Setups.DisplayOptions.Common.Query where

import SlamData.Prelude

import Halogen as H
import Halogen.Component.Utils as HCU

data Query s a
  = Init a
  | Modify (s → s) a

type State s = { error ∷ Maybe String | s }

type DSL s a m = H.ComponentDSL s (Query s) (Maybe a) m

eval
  ∷ ∀ s a m
  . (State s → Either String a)
  → Query (State s)
  ~> DSL (State s) a m
eval toModel = case _ of
  Init next → do
    handleUpdate toModel =<< H.get
    pure next
  Modify f next → do
    handleUpdate toModel =<< HCU.modify f
    pure next

handleUpdate
  ∷ ∀ s a m
  . (State s → Either String a)
  → State s
  → DSL (State s) a m Unit
handleUpdate toModel st = case toModel st of
  Left err → do
    H.modify (_ { error = Just err })
    H.raise Nothing
  Right opts → do
    H.modify (_ { error = Nothing })
    H.raise (Just opts)

evalInfallible ∷ ∀ s a m. (s → a) → Query s ~> DSL s a m
evalInfallible toModel = case _ of
  Init next → do
    H.raise ∘ Just ∘ toModel =<< H.get
    pure next
  Modify f next → do
    H.raise ∘ Just ∘ toModel =<< HCU.modify f
    pure next
