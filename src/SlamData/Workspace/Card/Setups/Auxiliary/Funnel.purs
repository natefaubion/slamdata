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

module SlamData.Workspace.Card.Setups.Auxiliary.Funnel where

import SlamData.Prelude hiding (case_)

import Data.Argonaut ((:=), (.?), (~>))
import Data.Argonaut as J
import Data.Functor.Variant (FProxy, on, inj, case_)

import Halogen as H
import Halogen.HTML as HH

import SlamData.Common.Sort (sortSelect, Sort)
import SlamData.Common.Align (alignSelect, Align)
import SlamData.Render.Common (row)
import SlamData.Workspace.Card.Setups.Auxiliary.Reset (ResetF, AuxComponent)
import SlamData.Workspace.Card.Setups.Auxiliary.Render as Render
import SlamData.Workspace.Card.Setups.Auxiliary.Eval as Eval
import SlamData.Workspace.Card.Setups.Auxiliary.Algebra as A
import SlamData.Workspace.Card.Setups.Auxiliary.Piece (order, align)
import SlamData.Workspace.Card.Setups.Auxiliary.Proxy (_order, _align, _reset)

import Test.StrongCheck.Arbitrary (arbitrary)
import Test.StrongCheck.Gen as Gen

type QueryR =
  ( order ∷ FProxy (A.ChooseF Sort)
  , align ∷ FProxy (A.ChooseF Align)
  )
type State = { order ∷ Sort, align ∷ Align }
type Query = ResetF State QueryR

initial ∷ State
initial = { order, align }

gen ∷ Gen.Gen State
gen = do
  order ← arbitrary
  align ← arbitrary
  pure { order, align }

eq_ ∷ State → State → Boolean
eq_ r1 r2 =
  r1.order ≡ r2.order
  ∧ r1.align ≡ r2.align

encode ∷ State → J.Json
encode r =
  "configType" := "funnel"
  ~> "order" := r.order
  ~> "align" := r.align
  ~> J.jsonEmptyObject

decode ∷ J.Json → String ⊹ State
decode r = J.decodeJson r >>= \obj → do
  tag ← obj .? "configType"
  unless (tag ≡ "funnel") $ Left "This is not a funnel"
  order ← obj .? "order"
  align ← obj .? "align"
  pure { order, align }

component ∷ ∀ m. AuxComponent QueryR State m
component = H.component
  { initialState: const initial
  , render: \state → HH.div_
    [ HH.hr_
    , row [ Render.choose _order sortSelect state
          , Render.choose _align alignSelect state
          ]
    ]
  , eval: case_
    # on _order (Eval.choose _order)
    # on _align (Eval.choose _align)
    # on _reset Eval.reset
  , receiver: map $ inj _reset ∘ H.action ∘ Tuple
  }
