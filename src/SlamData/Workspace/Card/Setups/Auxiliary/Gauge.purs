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

module SlamData.Workspace.Card.Setups.Auxiliary.Gauge where

import SlamData.Prelude hiding (case_)

import Data.Argonaut ((:=), (.?), (~>))
import Data.Argonaut as J
import Data.Functor.Variant (FProxy, on, inj, case_)
import Halogen as H
import Halogen.HTML as HH
import SlamData.Workspace.Card.Setups.Auxiliary.Algebra as A
import SlamData.Workspace.Card.Setups.Auxiliary.Eval as Eval
import SlamData.Workspace.Card.Setups.Auxiliary.Piece (minValue, maxValue)
import SlamData.Workspace.Card.Setups.Auxiliary.Piece as P
import SlamData.Workspace.Card.Setups.Auxiliary.Proxy (_val, _reset)
import SlamData.Workspace.Card.Setups.Auxiliary.Render as Render
import SlamData.Workspace.Card.Setups.Auxiliary.Reset (ResetF, AuxComponent)
import Test.StrongCheck.Gen as Gen

type QueryR = ( val ∷ FProxy A.MinMaxF )
type State = { val ∷ P.MinMax }
type Query = ResetF State QueryR

initial ∷ State
initial = { val: { min: minValue, max: maxValue } }

gen ∷ Gen.Gen State
gen = do
  val ← P.genMinMax
  pure { val }

eq_ ∷ State → State → Boolean
eq_ r1 r2 =
  r1.val.min ≡ r2.val.min
  ∧ r1.val.max ≡ r2.val.max

encode ∷ State → J.Json
encode r =
  "configType" := "graph"
  ~> "minValue" := r.val.min
  ~> "maxValue" := r.val.max
  ~> J.jsonEmptyObject

decode ∷ J.Json → String ⊹ State
decode r = J.decodeJson r >>= \obj → do
  tag ← obj .? "configType"
  unless (tag ≡ "graph") $ Left "This is not a graph"
  minValue ← obj .? "minValue"
  maxValue ← obj .? "maxValue"
  pure { val: { min: minValue, max: maxValue } }

component ∷ ∀ m. AuxComponent QueryR State m
component = H.component
  { initialState: const initial
  , render: \state → HH.div_
    [ HH.hr_
    , Render.minMax _val state
    ]
  , eval: case_
    # on _val (Eval.minMax _val)
    # on _reset Eval.reset
  , receiver: map $ inj _reset ∘ H.action ∘ Tuple
  }
