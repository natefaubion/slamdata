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

module SlamData.Workspace.Card.Setups.Auxiliary.Proxy where

import Data.Symbol (SProxy(..), class IsSymbol)

class IsSymbol s ⇐ HasLabel s where
  label ∷ SProxy s → String

instance isSmoothHasLabel ∷ HasLabel "isSmooth" where
  label _ = "Smooth"
instance isStackedHasLabel ∷ HasLabel "isStacked" where
  label _ = "Stacked"
instance sizeHasLabel ∷ HasLabel "size" where
  label _ = "Size"
instance valHasLabel ∷ HasLabel "val" where
  label _ = "Value"
instance axisLabelAngleHasLabel ∷ HasLabel "axisLabelAngle" where
  label _ = "Axis label angle"
instance isColorSchemeReversedHasLabel ∷ HasLabel "isColorSchemeReversed" where
  label _ = "Reversed color scheme"
instance optionalMarkersHasLabel ∷ HasLabel "optionalMarkers" where
  label _ = "Optional Markers"
instance orderHasLabel ∷ HasLabel "order" where
  label _ = "Order"
instance alignHasLabel ∷ HasLabel "align" where
  label _ = "Align"
instance formatterHasLabel ∷ HasLabel "formatter" where
  label _ = "Value formatter"
instance circularHasLabel ∷ HasLabel "circular" where
  label _ = "Circular layout"
instance colorSchemeHasLabel ∷ HasLabel "colorScheme" where
  label _ = "Color scheme"

_size = SProxy ∷ SProxy "size"
_string = SProxy ∷ SProxy "string"
_uri = SProxy ∷ SProxy "uri"
_min = SProxy ∷ SProxy "min"
_max = SProxy ∷ SProxy "max"
_isSmooth = SProxy ∷ SProxy "isSmooth"
_isStacked = SProxy ∷ SProxy "isStacked"
_osm = SProxy ∷ SProxy "osm"
_axisLabelAngle = SProxy ∷ SProxy "axisLabelAngle"
_val = SProxy ∷ SProxy "val"
_isColorSchemeReversed = SProxy ∷ SProxy "isColorSchemeReversed"
_colorScheme = SProxy ∷ SProxy "colorScheme"
_optionalMarkers = SProxy ∷ SProxy "optionalMarkers"
_order = SProxy ∷ SProxy "order"
_align = SProxy ∷ SProxy "align"
_circular = SProxy ∷ SProxy "circular"
_formatter = SProxy ∷ SProxy "formatter"
_reset = SProxy ∷ SProxy "reset"
