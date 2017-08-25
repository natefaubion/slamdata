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

module SlamData.Workspace.Card.Setups.DimensionMap.Projection where

import SlamData.Prelude

import Data.Codec.Argonaut as CA
import Data.Codec.Argonaut.Common as CAC
import Data.Argonaut (JCursor, class EncodeJson, class DecodeJson)
import Data.Int as Int
import Data.Lens (wander, lens)
import Data.Lens.At (class At)
import Data.Lens.Index (class Index)
import Data.List as L
import Data.StrMap as SM
import Data.String.Gen as SG
import SlamData.Workspace.Card.Setups.Dimension as D
import SlamData.Workspace.Card.Setups.Transform.Aggregation as Ag
import SlamData.Workspace.Card.Setups.Transform as Tr
import Test.StrongCheck.Gen as Gen

newtype Projection = Projection
  { dimension ∷ JCursor → D.LabeledJCursor
  , label ∷ String
  , value ∷ String
  , select ∷ String
  , deselectable ∷ Boolean
  , labelless ∷ Boolean
  , transforms ∷ Maybe Tr.Transform → Array Tr.Transform
  , key ∷ String
  }

getDimension ∷ Projection → JCursor → D.LabeledJCursor
getDimension (Projection {dimension: d}) = d

getLabel ∷ Projection → String
getLabel (Projection {label}) = label

getValue ∷ Projection → String
getValue (Projection {value: v}) = v

getSelect ∷ Projection → String
getSelect (Projection {select}) = select

isDeselectable ∷ Projection → Boolean
isDeselectable (Projection {deselectable}) = deselectable

isLabelless ∷ Projection → Boolean
isLabelless (Projection {labelless}) = labelless

availableTransforms ∷ Projection → Maybe Tr.Transform → Array Tr.Transform
availableTransforms (Projection {transforms}) = transforms

newtype ProjectionMap a = ProjectionMap (SM.StrMap a)

derive instance newtypeProjectionMap ∷ Newtype (ProjectionMap a) _
derive newtype instance eqProjectionMap ∷ Eq a ⇒ Eq (ProjectionMap a)
derive newtype instance encodeProjectionMap ∷ EncodeJson a ⇒ EncodeJson (ProjectionMap a)
derive newtype instance decodeProjectionMap ∷ DecodeJson a ⇒ DecodeJson (ProjectionMap a)

projectionMapCodec ∷ ∀ a. CA.JsonCodec a → CA.JsonCodec (ProjectionMap a)
projectionMapCodec aCodec = _Newtype $ CAC.strMap aCodec

dimMapCodec ∷ CA.JsonCodec DimMap
dimMapCodec = projectionMapCodec D.codecLabeledJCursor

toStrMap ∷ ∀ a. ProjectionMap a → SM.StrMap a
toStrMap (ProjectionMap sm) = sm

type DimMap = ProjectionMap D.LabeledJCursor

genProjectionMap ∷ ∀ a. Gen.Gen a → Gen.Gen (ProjectionMap a)
genProjectionMap genV = map (ProjectionMap ∘ SM.fromFoldable) $ Gen.arrayOf pair
  where
  pair = Tuple <$> SG.genAsciiString <*> genV

genDimMap ∷ Gen.Gen DimMap
genDimMap = genProjectionMap D.genLabeledJCursor

empty ∷ ∀ a. ProjectionMap a
empty = ProjectionMap SM.empty

lookup ∷ ∀ a. Projection → ProjectionMap a → Maybe a
lookup (Projection {key}) (ProjectionMap sm) = SM.lookup key sm

member ∷ ∀ a. Projection → ProjectionMap a → Boolean
member (Projection {key}) (ProjectionMap sm) = SM.member key sm

insert ∷ ∀ a. Projection → a → ProjectionMap a → ProjectionMap a
insert (Projection {key}) a (ProjectionMap sm) = ProjectionMap $ SM.insert key a sm

delete ∷ ∀ a. Projection → ProjectionMap a → ProjectionMap a
delete (Projection {key}) (ProjectionMap sm) = ProjectionMap $ SM.delete key sm

filter ∷ ∀ a. (a → Boolean) → ProjectionMap a → ProjectionMap a
filter f (ProjectionMap sm) = ProjectionMap $ SM.filter f sm

type AxesComposer s =
  { guard ∷ Projection → DimMap → s → s
  , filter ∷ Projection → DimMap → s → s
  }

print ∷ Projection → String
print (Projection {key}) = key

eq_ ∷ Projection → Projection → Boolean
eq_ (Projection r1) (Projection r2) = r1.key ≡ r2.key

mbDimIx ∷ Projection → Maybe Int
mbDimIx = Int.fromString ∘ print

dims ∷ ∀ a. ProjectionMap a → L.List (Int × a)
dims (ProjectionMap sm) =
  SM.foldMap (\s a → foldMap (\k → pure $ k × a) $ Int.fromString s ) sm

dimension ∷ Projection
dimension = Projection
  { dimension: D.projection
  , label: "Dimension"
  , value: "Choose dimension"
  , select: "Choose dimension"
  , deselectable: true
  , labelless: false
  , transforms: Tr.categoricalTransforms
  , key: "dimension"
  }

high ∷ Projection
high = Projection
  { dimension: D.projectionWithAggregation $ Just Ag.Sum
  , label: "High"
  , value: "Choose high"
  , select: "Choose measure for high position"
  , deselectable: false
  , labelless: false
  , transforms: const Tr.aggregationTransforms
  , key: "high"
  }

low ∷ Projection
low = Projection
  { dimension: D.projectionWithAggregation $ Just Ag.Sum
  , label: "Low"
  , value: "Choose low"
  , select: "Choose measure for low position"
  , deselectable: false
  , labelless: false
  , transforms: const Tr.aggregationTransforms
  , key: "low"
  }

open ∷ Projection
open = Projection
  { dimension: D.projectionWithAggregation $ Just Ag.Sum
  , label: "Open"
  , value: "Choose open"
  , select: "Choose measure for open position"
  , deselectable: false
  , labelless: false
  , transforms: const Tr.aggregationTransforms
  , key: "open"
  }

close ∷ Projection
close = Projection
  { dimension: D.projectionWithAggregation $ Just Ag.Sum
  , label: "Close"
  , value: "Choose close"
  , select: "Choose measure for close position"
  , deselectable: false
  , labelless: false
  , transforms: const Tr.aggregationTransforms
  , key: "close"
  }

parallel ∷ Projection
parallel = Projection
  { dimension: D.projection
  , label: "Parallel"
  , value: "Choose parallel"
  , select: "Choose parallel"
  , deselectable: true
  , labelless: false
  , transforms: Tr.categoricalTransforms
  , key: "parallel"
  }

value ∷ Projection
value = Projection
  { dimension: D.projectionWithAggregation $ Just Ag.Sum
  , label: "Measure #1"
  , value: "Choose measure"
  , select: "Choose measure"
  , deselectable: false
  , labelless: false
  , transforms: const Tr.aggregationTransforms
  , key: "value"
  }

flatValue ∷ Projection
flatValue = Projection
  { dimension: D.projectionWithAggregation $ Just Ag.Sum
  , label: "Measure #1"
  , value: "Choose measure"
  , select: "Choose measure"
  , deselectable: false
  , labelless: false
  , transforms: const Tr.aggregationTransforms
  , key: "flatValue"
  }

series ∷ Projection
series = Projection
  { dimension: D.projection
  , label: "Series"
  , value: "Choose series"
  , select: "Choose series"
  , deselectable: true
  , labelless: false
  , transforms: Tr.categoricalTransforms
  , key: "series"
  }

category ∷ Projection
category = Projection
  { dimension: D.projection
  , label: "Category"
  , value: "Choose category"
  , select: "Choose category"
  , deselectable: true
  , labelless: false
  , transforms: Tr.categoricalTransforms
  , key: "category"
  }

stack ∷ Projection
stack = Projection
  { dimension: D.projection
  , label: "Stack"
  , value: "Choose stack"
  , select: "Choose stack"
  , deselectable: true
  , labelless: false
  , transforms: Tr.categoricalTransforms
  , key: "stack"
  }

source ∷ Projection
source = Projection
  { dimension: D.projection
  , label: "Source"
  , value: "Choose source"
  , select: "Choose source"
  , deselectable: true
  , labelless: false
  , transforms: Tr.categoricalTransforms
  , key: "source"
  }

target ∷ Projection
target = Projection
  { dimension: D.projection
  , label: "Target"
  , value: "Choose target"
  , select: "Choose target"
  , deselectable: true
  , labelless: false
  , transforms: Tr.categoricalTransforms
  , key: "target"
  }

abscissa ∷ Projection
abscissa = Projection
  { dimension: D.projection
  , label: "X-axis"
  , value: "Choose x-axis"
  , select: "Choose x-axis"
  , deselectable: true
  , labelless: false
  , transforms: Tr.categoricalTransforms
  , key: "abscissa"
  }

ordinate ∷ Projection
ordinate = Projection
  { dimension: D.projectionWithAggregation $ Just Ag.Sum
  , label: "Y-axis"
  , value: "Choose y-axis"
  , select: "Choose y-axis"
  , deselectable: false
  , labelless: false
  , transforms: const Tr.aggregationTransforms
  , key: "ordinate"
  }

secondValue ∷ Projection
secondValue = Projection
  { dimension: D.projectionWithAggregation $ Just Ag.Sum
  , label: "Measure #2"
  , value: "Choose measure"
  , select: "Choose measure"
  , deselectable: false
  , labelless: false
  , transforms: const Tr.aggregationTransforms
  , key: "secondValue"
  }

donut ∷ Projection
donut = Projection
  { dimension: D.projection
  , label: "Donut"
  , value: "Choose donut"
  , select: "Choose donut"
  , deselectable: true
  , labelless: false
  , transforms: Tr.categoricalTransforms
  , key: "donut"
  }

multiple ∷ Projection
multiple = Projection
  { dimension: D.projection
  , label: "Multiple"
  , value: "Choose multiple"
  , select: "Choose multiple"
  , deselectable: true
  , labelless: false
  , transforms: Tr.categoricalTransforms
  , key: "multiple"
  }

size ∷ Projection
size = Projection
  { dimension: D.projectionWithAggregation $ Just Ag.Sum
  , label: "Size"
  , value: "Choose size"
  , select: "Choose size"
  , deselectable: false
  , labelless: false
  , transforms: const Tr.aggregationTransforms
  , key: "size"
  }

color ∷ Projection
color = Projection
  { dimension: D.projection
  , label: "Color"
  , value: "Choose color"
  , select: "Choose color"
  , deselectable: true
  , labelless: false
  , transforms: Tr.categoricalTransforms
  , key: "color"
  }

scatterOrdinate ∷ Projection
scatterOrdinate = Projection
  { dimension: D.projectionWithAggregation $ Just Ag.Sum
  , label: "Y-axis"
  , value: "Choose y-axis"
  , select: "Choose y-axis"
  , deselectable: true
  , labelless: false
  , transforms: Tr.categoricalTransforms
  , key: "ordinate"
  }

scatterSize ∷ Projection
scatterSize = Projection
  { dimension: D.projectionWithAggregation $ Just Ag.Sum
  , label: "Size"
  , value: "Choose size"
  , select: "Choose size"
  , deselectable: true
  , labelless: false
  , transforms: const Tr.aggregationTransforms
  , key: "size"
  }

lat ∷ Projection
lat = Projection
  { dimension: D.projection
  , label: "Latitude"
  , value: "Choose latitude"
  , select: "Choose latitude"
  , deselectable: true
  , labelless: false
  , transforms: Tr.categoricalTransforms
  , key: "lat"
  }

lng ∷ Projection
lng = Projection
  { dimension: D.projection
  , label: "Longitude"
  , value: "Choose longitude"
  , select: "Choose longitude"
  , deselectable: true
  , labelless: false
  , transforms: Tr.categoricalTransforms
  , key: "lng"
  }

intensity ∷ Projection
intensity = Projection
  { dimension: D.projectionWithAggregation $ Just Ag.Sum
  , label: "Intensity"
  , value: "Choose intensity"
  , select: "Choose intensity"
  , deselectable: false
  , labelless: false
  , transforms: const Tr.aggregationTransforms
  , key: "intensity"
  }

formValue ∷ Projection
formValue = Projection
  { dimension: D.projection
  , label: "Form input value"
  , value: "Choose form input"
  , select: "Choose form input"
  , deselectable: false
  , labelless: false
  , transforms: const Tr.aggregationTransforms
  , key: "value"
  }

formLabel ∷ Projection
formLabel = Projection
  { dimension: D.projection
  , label: "Form label"
  , value: "Choose form label"
  , select: "Choose form label"
  , deselectable: true
  , labelless: true
  , transforms: Tr.categoricalTransforms
  , key: "label"
  }

formSelected ∷ Projection
formSelected = Projection
  { dimension: D.projection
  , label: "Selected values"
  , value: "Choose selected values"
  , select: "Choose selected values"
  , deselectable: true
  , labelless: true
  , transforms: Tr.categoricalTransforms
  , key: "selected"
  }

dimIx ∷ Int → Projection
dimIx ix = Projection
  { dimension: D.projectionWithAggregation $ Just Ag.Sum
  , label: "Measure #" <> show (ix + 1)
  , value: "Choose measure"
  , select: "Choose measure"
  , deselectable: false
  , labelless: false
  , transforms: Tr.categoricalTransforms
  , key: show ix
  }

-- | Some projections shouldn't be aggregated
isFlat ∷ Projection → Boolean
isFlat = print ⋙ case _ of
  "flatValue" → true
  "formValue" → true
  "formLabel" → true
  "formSelected" → true
  _ → false


instance indexProjectionMap ∷ Index (ProjectionMap a) Projection a where
  ix k = wander \coalg m →
    lookup k m # maybe (pure m) (coalg ⋙ map \v → insert k v m)
instance atProjectionMap ∷ At (ProjectionMap a) Projection a where
  at k = lens (lookup k) \m →
    maybe (delete k m) \v → insert k v m
