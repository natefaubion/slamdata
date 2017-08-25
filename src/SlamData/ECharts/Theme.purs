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

module SlamData.ECharts.Theme (defaultThemeColor) where

import SlamData.Prelude

import Control.Monad.Eff (Eff)
import DOM (DOM)
import DOM.HTML (window)
import Data.Foreign (toForeign)
import Data.Nullable (toMaybe)
import ECharts.Theme (Theme)
import Utils.DOM as DOM

defaultThemeColor ∷ ∀ eff. Eff (dom ∷ DOM | eff) (Maybe Theme)
defaultThemeColor =
  window
    >>= DOM.getHTMLTextColorString
    >>> map (toMaybe >>> map themeForColor)
  where
    themeForColor ∷ String → Theme
    themeForColor color = Right $ toForeign
      { color
      , textStyle: { color }
      , dataZoom: { textStyle: { color } }
      , graph: { textStyle: { color } }
      , guage: { title: { textStyle: { color } } }
      , legend: { textStyle: { color } }
      , timeline:
          { lineStyle: { color }
          , itemStyle: { normal: { color } }
          , label: { normal: { color } }
          , controlStyle: { normal: { borderColor: color, color } }
          }
      , title: { textStyle: { color } }
      , visualMap: { textStyle: { color } }
      }
