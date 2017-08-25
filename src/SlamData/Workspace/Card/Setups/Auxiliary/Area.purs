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

module SlamData.Workspace.Card.Setups.Auxiliary.Area where

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
import SlamData.Workspace.Card.Setups.Auxiliary.Piece (isStacked, isSmooth, size, axisLabelAngle)
import SlamData.Workspace.Card.Setups.Auxiliary.Proxy (_isStacked, _isSmooth, _axisLabelAngle, _size, _reset)

import Test.StrongCheck.Arbitrary (arbitrary)
import Test.StrongCheck.Gen as Gen

type State =
  { isStacked ∷ Boolean
  , isSmooth ∷ Boolean
  , axisLabelAngle ∷ Number
  , size ∷ Number
  }

type QueryR =
  ( isStacked ∷ FProxy A.ToggleF
  , isSmooth ∷ FProxy A.ToggleF
  , axisLabelAngle ∷ FProxy A.SetF
  , size ∷ FProxy A.SetF
  )
type Query = ResetF State QueryR

initial ∷ State
initial =
 { isStacked
 , isSmooth
 , size
 , axisLabelAngle
 }

gen ∷ Gen.Gen State
gen = do
  isStacked ← arbitrary
  isSmooth ← arbitrary
  size ← arbitrary
  axisLabelAngle ← arbitrary
  pure { isStacked
       , isSmooth
       , size
       , axisLabelAngle
       }

eq_ ∷ State → State → Boolean
eq_ r1 r2 =
  r1.isStacked ≡ r2.isStacked
  ∧ r1.isSmooth ≡ r2.isSmooth
  ∧ r1.size ≡ r2.size
  ∧ r1.axisLabelAngle ≡ r2.axisLabelAngle

encode ∷ State → J.Json
encode r =
  "configType" := "area"
  ~> "isStacked" := r.isStacked
  ~> "isSmooth" := r.isSmooth
  ~> "size" := r.size
  ~> "axisLabelAngle" := r.axisLabelAngle
  ~> J.jsonEmptyObject


decode ∷ J.Json → String ⊹ State
decode r = J.decodeJson r >>= \obj → do
  tag ← obj .? "configType"
  unless (tag ≡ "area") $ Left "This is not area"
  isStacked ← obj .? "isStacked"
  isSmooth ← obj .? "isSmooth"
  size ← obj .? "size"
  axisLabelAngle ← obj .? "axisLabelAngle"
  pure { isStacked
       , isSmooth
       , size
       , axisLabelAngle
       }

component ∷ ∀ m. AuxComponent QueryR State m
component = H.component
  { initialState: const initial
  , render: \state → HH.div_
    [ HH.hr_
    , row [ Render.toggle _isStacked state
          , Render.toggle _isSmooth state
          ]
    , HH.hr_
    , row [ Render.number _axisLabelAngle state
          , Render.number _size state
          ]
    ]
  , eval: case_
    # on _isStacked (Eval.toggle _isStacked)
    # on _isSmooth (Eval.toggle _isSmooth)
    # on _axisLabelAngle (Eval.number _axisLabelAngle)
    # on _size (Eval.number _size)
    # on _reset Eval.reset
  , receiver: map $ inj _reset ∘ H.action ∘ Tuple
  }
