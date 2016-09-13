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

module SlamData.Workspace.Card.PivotTable.Model
  ( Model
  , emptyModel
  , eqModel
  , genModel
  , encode
  , decode
  ) where

import SlamData.Prelude
import Data.Argonaut as J
import Data.Argonaut ((~>), (:=), (.?))
import Test.StrongCheck.Gen as Gen

type Model = {}

emptyModel ∷ Model
emptyModel = {}

eqModel ∷ Model → Model → Boolean
eqModel a b = false

genModel ∷ Gen.Gen Model
genModel = pure {}

encode ∷ Model → J.Json
encode m = J.jsonEmptyObject

decode ∷ J.Json → Either String Model
decode m = pure {}
