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

module SlamData.Workspace.Card.Setups.Auxiliary.Line where

import SlamData.Prelude hiding (case_)

import Data.Argonaut ((:=), (.?), (~>))
import Data.Argonaut as J
import Data.Functor.Variant (FProxy, on, inj, case_)

import Halogen as H
import Halogen.HTML as HH

import SlamData.Render.Common (row)
import SlamData.Workspace.Card.Setups.Auxiliary.Algebra as A
import SlamData.Workspace.Card.Setups.Auxiliary.Eval as Eval
import SlamData.Workspace.Card.Setups.Auxiliary.Piece (minSize, maxSize, optionalMarkers, axisLabelAngle)
import SlamData.Workspace.Card.Setups.Auxiliary.Piece as P
import SlamData.Workspace.Card.Setups.Auxiliary.Proxy (_reset, _optionalMarkers, _size, _axisLabelAngle)
import SlamData.Workspace.Card.Setups.Auxiliary.Render as Render
import SlamData.Workspace.Card.Setups.Auxiliary.Reset (ResetF, AuxComponent)

import Test.StrongCheck.Arbitrary (arbitrary)
import Test.StrongCheck.Gen as Gen

type QueryR =
  ( optionalMarkers ∷ FProxy A.ToggleF
  , size ∷ FProxy A.MinMaxF
  , axisLabelAngle ∷ FProxy A.SetF
  )
type State =
  { optionalMarkers ∷ Boolean
  , size ∷ P.MinMax
  , axisLabelAngle ∷ Number
  }
type Query = ResetF State QueryR

initial ∷ State
initial =
  { optionalMarkers
  , size: { min: minSize, max: maxSize }
  , axisLabelAngle
  }

gen ∷ Gen.Gen State
gen = do
  optionalMarkers ← arbitrary
  size ← P.genMinMax
  axisLabelAngle ← arbitrary
  pure { optionalMarkers
       , size
       , axisLabelAngle
       }

eq_ ∷ State → State → Boolean
eq_ r1 r2 =
  r1.optionalMarkers ≡ r2.optionalMarkers
  ∧ r1.size.min ≡ r2.size.min
  ∧ r1.size.max ≡ r2.size.max
  ∧ r1.axisLabelAngle ≡ r2.axisLabelAngle

encode ∷ State → J.Json
encode r =
  "configType" := "line"
  ~> "optionalMarkers" := r.optionalMarkers
  ~> "minSize" := r.size.min
  ~> "maxSize" := r.size.max
  ~> "axisLabelAngle" := r.axisLabelAngle
  ~> J.jsonEmptyObject

decode ∷ J.Json → String ⊹ State
decode r = J.decodeJson r >>= \obj → do
  tag ← obj .? "configType"
  unless (tag ≡ "line") $ Left "This is not a line"
  optionalMarkers ← obj .? "optionalMarkers"
  minSize ← obj .? "minSize"
  maxSize ← obj .? "maxSize"
  axisLabelAngle ← obj .? "axisLabelAngle"
  pure { optionalMarkers
       , axisLabelAngle
       , size: { min: minSize, max: maxSize }
       }

component ∷ ∀ m. AuxComponent QueryR State m
component = H.component
  { initialState: const initial
  , render: \state → HH.div_
    [ HH.hr_
    , row [ Render.toggle _optionalMarkers state ]
    , HH.hr_
    , Render.minMax _size state
    , HH.hr_
    , row [ Render.number _axisLabelAngle state ]
    ]
  , eval: case_
    # on _optionalMarkers (Eval.toggle _optionalMarkers)
    # on _size (Eval.minMax _size)
    # on _axisLabelAngle (Eval.number _axisLabelAngle)
    # on _reset Eval.reset
  , receiver: map $ inj _reset ∘ H.action ∘ Tuple
  }
