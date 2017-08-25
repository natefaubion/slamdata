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

module SlamData.Workspace.Card.Setups.Auxiliary.Bar where

import SlamData.Prelude hiding (case_)

import Data.Argonaut ((:=), (.?), (~>))
import Data.Argonaut as J
import Data.Functor.Variant (FProxy, on, inj, case_)

import Halogen as H
import Halogen.HTML as HH

import SlamData.Render.Common (row)
import SlamData.Workspace.Card.Setups.Auxiliary.Reset (ResetF, AuxComponent)
import SlamData.Workspace.Card.Setups.Auxiliary.Render as Render
import SlamData.Workspace.Card.Setups.Auxiliary.Eval as Eval
import SlamData.Workspace.Card.Setups.Auxiliary.Algebra as A
import SlamData.Workspace.Card.Setups.Auxiliary.Piece (axisLabelAngle)
import SlamData.Workspace.Card.Setups.Auxiliary.Proxy (_axisLabelAngle, _reset)

import Test.StrongCheck.Arbitrary (arbitrary)
import Test.StrongCheck.Gen as Gen

type State = { axisLabelAngle ∷ Number }
type QueryR = ( axisLabelAngle ∷ FProxy A.SetF )
type Query  = ResetF State QueryR

initial ∷ State
initial = { axisLabelAngle }

gen ∷ Gen.Gen State
gen = map { axisLabelAngle: _ } arbitrary

eq_ ∷ State → State → Boolean
eq_ r1 r2 =
  r1.axisLabelAngle ≡ r2.axisLabelAngle

encode ∷ State → J.Json
encode r =
  "configType" := "bar"
  ~> "axisLabelAngle" := r.axisLabelAngle
  ~> J.jsonEmptyObject

decode ∷ J.Json → String ⊹ State
decode r = J.decodeJson r >>= \obj → do
  tag ← obj .? "configType"
  unless (tag ≡ "bar") $ Left "This is not bar"
  axisLabelAngle ← obj .? "axisLabelAngle"
  pure { axisLabelAngle }

component ∷ ∀ m. AuxComponent QueryR State m
component = H.component
  { initialState: const initial
  , render: \state → HH.div_
    [ HH.hr_
    , row [ Render.number _axisLabelAngle state ]
    ]
  , eval: case_
    # on _axisLabelAngle (Eval.number _axisLabelAngle)
    # on _reset Eval.reset
  , receiver: map $ inj _reset ∘ H.action ∘ Tuple
  }
