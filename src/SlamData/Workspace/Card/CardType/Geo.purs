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

module SlamData.Workspace.Card.CardType.Geo where

import SlamData.Prelude

import Data.Variant (inj, on)
import Halogen.HTML as H
import SlamData.Render.Icon as I

_geoMarker = SProxy ∷ SProxy "geoMarker"
_geoHeatmap = SProxy ∷ SProxy "geoHeatmap"

type GeoR r =
  ( geoMarker ∷ Unit
  , geoHeatmap ∷ Unit
  | r)

type Geo r = Variant (GeoR r)

all ∷ ∀ r. Array (Geo r)
all = [ geoMarker, geoHeatmap ]

geoMarker ∷ ∀ r. Variant (geoMarker ∷ Unit|r)
geoMarker = inj _geoMarker unit

geoHeatmap ∷ ∀ r. Variant (geoHeatmap ∷ Unit|r)
geoHeatmap = inj _geoHeatmap unit

print ∷ ∀ r. (Variant r → String) → Geo r → String
print cb = cb
  # on _geoMarker (const "geo-marker")
  # on _geoHeatmap (const "geo-heatmap")

encode ∷ ∀ r. (Variant r → String) → Geo r → String
encode cb = cb
  # on _geoMarker (const "geo-marker-setup")
  # on _geoHeatmap (const "geo-heatmap-setup")

icon ∷ ∀ r. (Variant r → I.IconHTML) → Geo r → I.IconHTML
icon cb = cb
  # on _geoMarker (const $ I.IconHTML I.cardsSetupGeoChartMarker)
  # on _geoHeatmap (const $ I.IconHTML I.cardsSetupGeoChartHeatmap)

name ∷ ∀ r. (Variant r → String) → Geo r → String
name cb = cb
  # on _geoMarker (const "Marker")
  # on _geoHeatmap (const "Heatmap")


-- `print` used to return `marker-geo-setup` and `heatmap-geo-setup`
parse ∷ ∀ r. String → String ⊹ Geo r
parse = case _ of
  "geo-marker" → Right geoMarker
  "geo-heatmap" → Right geoHeatmap
  "marker-geo" → Right geoMarker
  "heatmap-geo" → Right geoHeatmap
  ty → Left $ ty ⊕ " is unknown geo chart card type"

consumerInteractable ∷ ∀ r. (Variant r → Boolean) → Geo r → Boolean
consumerInteractable cb = cb
  # on _geoMarker ff
  # on _geoHeatmap ff

cardClasses ∷ ∀ r. (Variant r → Array H.ClassName) → Geo r → Array H.ClassName
cardClasses cb = cb
  # on _geoMarker clss
  # on _geoHeatmap clss
  where
  clss _ = [ H.ClassName "sd-card-chart-options" ]

contractToGeo ∷ ∀ r. Contractable r (GeoR ()) ⇒ Variant r → Maybe (Geo ())
contractToGeo = contract
