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

module SlamData.Workspace.Card.Viz.Model where

import SlamData.Prelude

import Data.Argonaut ((.?))
import Data.Argonaut as J
import Data.Variant as V
import Data.Codec as C
import Data.Codec.Argonaut as CA
import Data.Codec.Argonaut.Variant as CAV

import SlamData.Workspace.Card.Viz.Renderer.PivotTable.Model as PM
import SlamData.Workspace.Card.Viz.Renderer.Select.Model as SM
import SlamData.Workspace.Card.Viz.Renderer.Input.Model as IM
import SlamData.Workspace.Card.Viz.Renderer.Geo.Model as GM

import Test.StrongCheck.Gen as Gen

type Model = Variant
  ( pivot ∷ PM.Model
  , select ∷ SM.Model
  , input ∷ IM.Model
  , geo ∷ GM.Model
  , static ∷ Unit
  , metric ∷ Unit
  , chart ∷ Unit
  )

initial ∷ Model
initial = chart unit

_pivot = SProxy ∷ SProxy "pivot"
_select = SProxy ∷ SProxy "select"
_input = SProxy ∷ SProxy "input"
_static = SProxy ∷ SProxy "static"
_metric = SProxy ∷ SProxy "metric"
_chart = SProxy ∷ SProxy "chart"
_geo = SProxy ∷ SProxy "geo"

pivot ∷ ∀ a r. a → Variant (pivot ∷ a|r)
pivot = V.inj _pivot

select ∷ ∀ a r. a → Variant (select ∷ a|r)
select = V.inj _select

input ∷ ∀ a r. a → Variant (input ∷ a|r)
input = V.inj _input

static ∷ ∀ a r. a → Variant (static ∷ a|r)
static = V.inj _static

metric ∷ ∀ a r. a → Variant (metric ∷ a|r)
metric = V.inj _metric

chart ∷ ∀ a r. a → Variant (chart ∷ a|r)
chart = V.inj _chart

geo ∷ ∀ a r. a → Variant (geo ∷ a|r)
geo = V.inj _geo

eq_ ∷ Model → Model → Boolean
eq_ r1 = V.default false
  # V.on _pivot (\r2 → V.on _pivot (PM.eq_ r2) ff r1)
  # V.on _select (\r2 → V.on _select (SM.eq_ r2) ff r1)
  # V.on _input (\r2 → V.on _input (IM.eq_ r2) ff r1)
  # V.on _static (V.on _static tt ff r1)
  # V.on _metric (V.on _metric tt ff r1)
  # V.on _chart (V.on _chart tt ff r1)
  # V.on _geo (\r2 → V.on _geo (GM.eq_ r2) ff r1)

gen ∷ Gen.Gen Model
gen = Gen.oneOf (pure $ V.inj _static unit)
  [ pure $ chart unit
  , pure $ metric unit
  , map pivot PM.gen
  , map select SM.gen
  , map input IM.gen
  , map geo GM.gen
  ]

empty ∷ Model
empty = V.inj _chart unit

actualCodec ∷ CA.JsonCodec Model
actualCodec = CAV.variant
  # CAV.variantCase _pivot (Right PM.codec)
  # CAV.variantCase _select (Right SM.codec)
  # CAV.variantCase _input (Right IM.codec)
  # CAV.variantCase _static (Left unit)
  # CAV.variantCase _metric (Left unit)
  # CAV.variantCase _chart (Left unit)
  # CAV.variantCase _geo (Right GM.codec)

legacyDecode ∷ String → J.Json → String ⊹ Model
legacyDecode str j = case str of
  "geo-chart" → map (V.inj _geo) $ GM.decode j
  "form-input" → J.decodeJson j >>= \obj → do
    modelType ← obj .? "modelType"
    case modelType of
      "textLike" → map (V.inj _input) $ lmap show $ C.decode IM.codec j
      "labeled" → map (V.inj _select) $ lmap show $ C.decode SM.codec j
      _ → pure $ V.inj _static unit
  "chart" → decodePivot j <|> decodeChart j
  _ → Left "unexpected viz model"
  where
  decodePivot json = do
    obj ← J.decodeJson json
    renderer ← obj .? "renderer"
    state ← obj .? "state"
    case renderer of
      "pivot" → map (V.inj _pivot) $ lmap show $ C.decode PM.codec state
      _ → Left "Not a valid chart renderer"

  -- Since evaluation takes place after loading all empty models
  -- work fine here.
  decodeChart _ = Right $ V.inj _chart unit

codec ∷ String → CA.JsonCodec Model
codec str = C.basicCodec dec $ C.encode actualCodec
  where
  dec j =
    ( C.decode actualCodec j )
    <|> ( lmap CA.TypeMismatch $ legacyDecode str j )
