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

module SlamData.Workspace.Card.Setups.Auxiliary.Graph where

import SlamData.Prelude hiding (case_)

import Data.Argonaut ((:=), (.?), (~>))
import Data.Argonaut as J
import Data.Functor.Variant (FProxy, on, inj, case_)

import Halogen as H
import Halogen.HTML as HH

import SlamData.Render.Common (row)
import SlamData.Workspace.Card.Setups.Auxiliary.Algebra as A
import SlamData.Workspace.Card.Setups.Auxiliary.Eval as Eval
import SlamData.Workspace.Card.Setups.Auxiliary.Piece (minSize, maxSize, circular)
import SlamData.Workspace.Card.Setups.Auxiliary.Piece as P
import SlamData.Workspace.Card.Setups.Auxiliary.Proxy (_size, _circular, _reset)
import SlamData.Workspace.Card.Setups.Auxiliary.Render as Render
import SlamData.Workspace.Card.Setups.Auxiliary.Reset (ResetF, AuxComponent)

import Test.StrongCheck.Arbitrary (arbitrary)
import Test.StrongCheck.Gen as Gen

type QueryR = ( size ∷ FProxy A.MinMaxF, circular ∷ FProxy A.ToggleF )
type State = { size ∷ P.MinMax, circular ∷ Boolean }
type Query = ResetF State QueryR

initial ∷ State
initial = { size: { min: minSize, max: maxSize }, circular }

gen ∷ Gen.Gen State
gen = do
  size ← P.genMinMax
  circular ← arbitrary
  pure { size, circular }

eq_ ∷ State → State → Boolean
eq_ r1 r2 =
  r1.size.min ≡ r2.size.min
  ∧ r1.size.max ≡ r2.size.max
  ∧ r1.circular ≡ r2.circular

encode ∷ State → J.Json
encode r =
  "configType" := "graph"
  ~> "minSize" := r.size.min
  ~> "maxSize" := r.size.max
  ~> "circular" := r.circular
  ~> J.jsonEmptyObject

decode ∷ J.Json → String ⊹ State
decode r = J.decodeJson r >>= \obj → do
  tag ← obj .? "configType"
  unless (tag ≡ "graph") $ Left "This is not a graph"
  minSize ← obj .? "minSize"
  maxSize ← obj .? "maxSize"
  circular ← obj .? "circular"
  pure { size: { min: minSize, max: maxSize }
       , circular
       }

component ∷ ∀ m. AuxComponent QueryR State m
component = H.component
  { initialState: const initial
  , render: \state → HH.div_
    [ HH.hr_
    , row [ Render.toggle _circular state ]
    , HH.hr_
    , Render.minMax _size state
    ]
  , eval: case_
    # on _circular (Eval.toggle _circular)
    # on _size (Eval.minMax _size)
    # on _reset Eval.reset
  , receiver: map $ inj _reset ∘ H.action ∘ Tuple
  }
