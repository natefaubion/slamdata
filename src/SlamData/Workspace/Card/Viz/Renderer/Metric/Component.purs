{-
Copyright 2016 SlamData, Inc.

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

module SlamData.Workspace.Card.Viz.Renderer.Metric.Component where

import SlamData.Prelude

import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP

import SlamData.Monad (Slam)
import SlamData.Workspace.Card.Port (MetricPort)
import SlamData.Workspace.LevelOfDetails (LevelOfDetails(..))

import Utils.DOM (fitText)

type Dimensions = { width ∷ Int, height ∷ Int }

type State =
  { width ∷ Int
  , height ∷ Int
  , label ∷ Maybe String
  , value ∷ String
  , valueHeight ∷ Int
  , labelHeight ∷ Int
  }

initialState ∷ Dimensions → State
initialState { width, height } =
  { width
  , height
  , label: Nothing
  , value: ""
  , valueHeight: 0
  , labelHeight: 0
  }

data Query a
  = SetMetric MetricPort a
  | SetDimensions Dimensions a
  | GetLOD (LevelOfDetails → a)

type DSL = H.ComponentDSL State Query Void Slam
type HTML = H.ComponentHTML Query

component ∷ H.Component HH.HTML Query Dimensions Void Slam
component =
  H.component
    { initialState
    , render
    , eval
    , receiver: HE.input SetDimensions
    }

render ∷ State → HTML
render state =
  HH.div
    [ HP.classes [ HH.ClassName "metric" ]
    , HP.ref refLabel
    ]
    [ HH.div
      [ HP.classes [ HH.ClassName "metric-value-and-label" ] ]
      $ [ HH.div
          [ HP.classes [ HH.ClassName "metric-value" ] ]
          [ HH.text state.value ] ]
          ⊕ foldMap renderLabel state.label
    ]
  where
  renderLabel ∷ String → Array HTML
  renderLabel str =
    [ HH.div
        [ HP.classes [ HH.ClassName "metric-label" ] ]
        [ HH.text str ]
    ]

eval ∷ Query ~> DSL
eval (SetMetric m next) = do
  H.modify _{label = m.label, value = m.value}
  adjustFontSizes
  pure next
eval (SetDimensions dims next) = do
  H.modify _{width = dims.width, height = dims.height}
  adjustFontSizes
  pure next
eval (GetLOD continue) = do
  state ← H.get
  pure $ continue $ if (state.labelHeight + state.valueHeight) > state.height then Low else High

availableFontSizes ∷ Array Int
availableFontSizes = [ 16, 24, 32, 48, 64, 96, 128, 160, 200 ]

adjustFontSizes ∷ DSL Unit
adjustFontSizes = do
  H.getHTMLElementRef refLabel >>= traverse_ \el →
    H.liftEff $ fitText availableFontSizes el

refLabel ∷ H.RefLabel
refLabel = H.RefLabel "container"
