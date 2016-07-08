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

module SlamData.Workspace.Card.Search.Component
  ( searchComponent
  , module SlamData.Workspace.Card.Search.Component.Query
  , module SlamData.Workspace.Card.Search.Component.State
  ) where

import SlamData.Prelude

import Data.Lens ((.~))

import Halogen as H
import Halogen.HTML.Events.Indexed as HE
import Halogen.HTML.Indexed as HH
import Halogen.HTML.Properties.Indexed as HP
import Halogen.HTML.Properties.Indexed.ARIA as ARIA
import Halogen.Themes.Bootstrap3 as B

import SlamData.Effects (Slam)
import SlamData.Render.Common (glyph)
import SlamData.Render.CSS as CSS
import SlamData.Workspace.Card.CardType as CT
import SlamData.Workspace.Card.Component as CC
import SlamData.Workspace.Card.Model as Card
import SlamData.Workspace.Card.Search.Component.Query (Query, SearchQuery(UpdateSearch))
import SlamData.Workspace.Card.Search.Component.State (State, _searchString, initialState)

type DSL = H.ComponentDSL State Query Slam
type HTML = H.ComponentHTML Query

searchComponent ∷ H.Component CC.CardStateP CC.CardQueryP Slam
searchComponent =
  CC.makeCardComponent
    { cardType: CT.Search
    , component: H.component { render, eval }
    , initialState: initialState
    , _State: CC._SearchState
    , _Query: CC.makeQueryPrism CC._SearchQuery
    }

render ∷ State → HTML
render state =
  HH.div
    [ HP.class_ CSS.form ]
    [ HH.input
        [ HP.inputType HP.InputText
        , HP.placeholder "Search string"
        , ARIA.label "Search string"
        , HE.onValueInput $ HE.input \str → right ∘ UpdateSearch str
        , HP.value state.searchString
        ]
    , HH.button
        [ HP.title "Clear search string"
        , ARIA.label "Clear search string"
        , HE.onClick $ HE.input_ (right ∘ UpdateSearch "")
        ]
        [ glyph B.glyphiconRemove ]
    ]

eval ∷ Natural Query DSL
eval = coproduct cardEval searchEval

cardEval ∷ Natural CC.CardEvalQuery DSL
cardEval = case _ of
  CC.EvalCard input output next →
    pure next
  CC.Activate next →
    pure next
  CC.Save k → do
    input ← H.gets _.searchString
    pure ∘ k $ Card.Search input
  CC.Load card next → do
    case card of
      Card.Search input → H.modify $ _searchString .~ input
      _ → pure unit
    pure next
  CC.SetDimensions _ next →
    pure next
  CC.ModelUpdated _ next →
    pure next
  CC.ZoomIn next →
    pure next

searchEval ∷ Natural SearchQuery DSL
searchEval (UpdateSearch str next) = do
  H.modify (_searchString .~ str)
  CC.raiseUpdatedC' CC.EvalModelUpdate
  pure next
