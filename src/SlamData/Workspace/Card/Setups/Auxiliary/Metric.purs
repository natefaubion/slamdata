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

module SlamData.Workspace.Card.Setups.Auxiliary.Metric where

import SlamData.Prelude hiding (case_)

import Data.Argonaut ((:=), (.?), (~>))
import Data.Argonaut as J
import Data.Functor.Variant (FProxy, on, inj, case_)

import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP

import SlamData.Workspace.Card.Setups.Auxiliary.Algebra as A
import SlamData.Workspace.Card.Setups.Auxiliary.Eval as Eval
import SlamData.Workspace.Card.Setups.Auxiliary.Piece (formatter)
import SlamData.Workspace.Card.Setups.Auxiliary.Proxy (_reset, _formatter)
import SlamData.Workspace.Card.Setups.Auxiliary.Render as Render
import SlamData.Workspace.Card.Setups.Auxiliary.Reset (ResetF, AuxComponent)

import Test.StrongCheck.Arbitrary (arbitrary)
import Test.StrongCheck.Gen as Gen

type QueryR = ( formatter ∷ FProxy A.SetF )
type State = { formatter ∷ String }
type Query = ResetF State QueryR

initial ∷ State
initial = { formatter }

gen ∷ Gen.Gen State
gen = map { formatter: _ } arbitrary

eq_ ∷ State → State → Boolean
eq_ r1 r2 =
  r1.formatter ≡ r2.formatter

encode ∷ State → J.Json
encode r =
  "configType" := "metric"
  ~> "formatter" := r.formatter
  ~> J.jsonEmptyObject

decode ∷ J.Json → String ⊹ State
decode r = J.decodeJson r >>= \obj → do
  tag ← obj .? "configType"
  unless (tag ≡ "metric") $ Left "This is not a metric"
  -- Previously `formatter` was `Maybe String`
  -- there is no any point in that, because `Nothing` was rendered as ""
  -- and "" is handled as `Nothing`
  formatter ← (obj .? "formatter" <|> pure "")
  pure { formatter }

component ∷ ∀ m. AuxComponent QueryR State m
component = H.component
  { initialState: const initial
  , render: \state → HH.div_
    [ HH.hr_
    , Render.string _formatter state
    , HH.hr_
    , HH.div_
      [ HH.p_ [ HH.text "Value between \"{{\" and \"}}\" will be replaced by following rules" ]
      , HH.p_
        [ HH.strong_ [ HH.text "{{0}}"]
        , HH.text " rounds to the closest integer"
        ]
      , HH.p_
        [ HH.strong_ [ HH.text "{{0,0}}"]
        , HH.text " rounds to the closest integer and adds thousands delimiters"
        ]
      , HH.p_
        [ HH.strong_ [ HH.text "{{000}}" ]
        , HH.text " adds leading zeros to the value"
        ]
      , HH.p_
        [ HH.strong_ [ HH.text "{{0a}}" ]
        , HH.text " adds an abbreviation"
        ]
      , HH.p_
        [ HH.strong_ [ HH.text "{{0.000}}" ]
        , HH.text " leaves three numbers after dot or adds up to three trailing zeros"
        ]
      , HH.p_
        [ HH.a [ HP.href "https://github.com/slamdata/purescript-formatters" ]
            [ HH.text "Complete documentation"
            ]
        ]
      ]
    ]
  , eval: case_ # on _formatter (Eval.string _formatter) # on _reset Eval.reset
  , receiver: map $ inj _reset ∘ H.action ∘ Tuple
  }
