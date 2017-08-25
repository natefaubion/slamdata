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

module SlamData.Workspace.Card.Setups.Auxiliary.GeoHeatmap where

import SlamData.Prelude hiding (case_)

import Data.Argonaut ((:=), (.?), (~>))
import Data.Argonaut as J
import Data.Functor.Variant (FProxy, on, inj, case_)

import Halogen as H
import Halogen.HTML as HH

import SlamData.Render.Common (row)
import SlamData.Workspace.Card.Setups.Auxiliary.Algebra as A
import SlamData.Workspace.Card.Setups.Auxiliary.Eval as Eval
import SlamData.Workspace.Card.Setups.Auxiliary.Piece (osm)
import SlamData.Workspace.Card.Setups.Auxiliary.Piece as P
import SlamData.Workspace.Card.Setups.Auxiliary.Proxy (_reset, _osm)
import SlamData.Workspace.Card.Setups.Auxiliary.Render as Render
import SlamData.Workspace.Card.Setups.Auxiliary.Reset (ResetF, AuxComponent)

import Test.StrongCheck.Gen as Gen

type State = { osm ∷ P.OsmURI }
type QueryR = ( osm ∷ FProxy A.SetF )
type Query = ResetF State QueryR

initial ∷ State
initial =  { osm }

gen ∷ Gen.Gen State
gen = map { osm: _ } P.genOsm

encode ∷ State → J.Json
encode r =
  "configType" := "geo-heatmap"
  ~> P.encodeOsmURI r.osm

eq_ ∷ State → State → Boolean
eq_ r1 r2 =
  r1.osm.uri ≡ r2.osm.uri
  ∧ r1.osm.string ≡ r2.osm.string

decode ∷ J.Json → String ⊹ State
decode r = J.decodeJson r >>= \obj → do
  tag ← obj .? "configType"
  unless (tag ≡ "geo-heatmap") $ Left "This is not geo heatmap"
  osm ← P.decodeOsmURI r
  pure { osm }

component ∷ ∀ m. AuxComponent QueryR State m
component = H.component
  { initialState: const initial
  , render: \state → HH.div_ [ HH.hr_, row [ Render.osmURI _osm state ] ]
  , eval: case_
    # on _osm (Eval.osmURI _osm)
    # on _reset Eval.reset
  , receiver: map $ inj _reset ∘ H.action ∘ Tuple
  }
