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

module SlamData.Workspace.Card.BuildChart.Common.Eval
  ( records
  , type (>>)
  ) where

import SlamData.Prelude

import Data.Argonaut (JArray)
import Data.Map as M

import Quasar.Types (FilePath)

import SlamData.Quasar.Query as QQ
import SlamData.Workspace.Card.Eval.Monad as CEM

infixr 3 type M.Map as >>

records
  ∷ FilePath
  → CEM.CardEval JArray
records resource = do
  numRecords ←
    CEM.liftQ $ QQ.count resource

  when (numRecords > 10000)
    $ CEM.throw
    $ "The 10000 record limit for visualizations has been exceeded - the current dataset contains "
    ⊕ show numRecords
    ⊕ " records. "
    ⊕ "Please consider using a 'limit' or 'group by' clause in the query to reduce the result size."

  CEM.liftQ $ QQ.all resource
