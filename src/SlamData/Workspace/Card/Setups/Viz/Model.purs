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

module SlamData.Workspace.Card.Setups.Viz.Model where

import SlamData.Prelude

import Data.Codec.Argonaut as CA
import Data.ListMap as LM
import SlamData.Workspace.Card.CardType as CT
import SlamData.Workspace.Card.CardType.VizType as VT
import SlamData.Workspace.Card.Setups.DimensionMap.Projection as Pr
import SlamData.Workspace.Card.Setups.Auxiliary as Aux
import Test.StrongCheck.Gen as Gen

lm ∷ ∀ a. LM.Module VT.VizType a
lm = LM.openModule VT.eq_

type Model =
  { dimMaps ∷ LM.ListMap VT.VizType Pr.DimMap
  , vizType ∷ VT.VizType
  , auxes ∷ LM.ListMap VT.VizType Aux.State
  }

initial ∷ Model
initial =
  { dimMaps: lm.empty
  , vizType: CT.pie
  , auxes: lm.empty
  }

eq_ ∷ Model → Model → Boolean
eq_ r1 r2 =
  lm.eq_ eq r1.dimMaps r2.dimMaps
  ∧ lm.eq_ Aux.eq_ r1.auxes r2.auxes
  ∧ VT.eq_ r1.vizType r2.vizType

gen ∷ Gen.Gen Model
gen = do
  vizType ← genVT
  auxes ← LM.gen genVT Aux.gen
  dimMaps ← LM.gen genVT Pr.genDimMap
  pure { vizType, auxes, dimMaps }
  where
  genVT = Gen.allInArray VT.all

codec ∷ CA.JsonCodec Model
codec = CA.object "Setups.Viz.Model" $ CA.record
  # CA.recordProp (SProxy ∷ SProxy "vizType")
      VT.codec
  # CA.recordProp (SProxy ∷ SProxy "auxes")
      (LM.listMapCodec VT.codec Aux.codec)
  # CA.recordProp (SProxy ∷ SProxy "dimMaps")
      (LM.listMapCodec VT.codec Pr.dimMapCodec)
