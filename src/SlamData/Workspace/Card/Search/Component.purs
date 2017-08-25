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
  ) where

import SlamData.Prelude

import Halogen as H
import Halogen.HTML.Events as HE
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Halogen.HTML.Properties.ARIA as ARIA

import SlamData.Render.Common as R
import SlamData.Render.ClassName as CN
import SlamData.Workspace.Card.CardType as CT
import SlamData.Workspace.Card.Component as CC
import SlamData.Workspace.Card.Model as Card
import SlamData.Workspace.Card.Search.Component.Query (Query(..))
import SlamData.Workspace.Card.Search.Component.State (State, initialState)
import SlamData.Workspace.LevelOfDetails as LOD

type DSL = CC.InnerCardDSL State Query
type HTML = CC.InnerCardHTML Query

searchComponent ∷ CC.CardOptions → CC.CardComponent
searchComponent =
  CC.makeCardComponent CT.search $ H.component
    { render
    , eval: coproduct cardEval searchEval
    , initialState: const initialState
    , receiver: const Nothing
    }

render ∷ State → HTML
render state =
  HH.div
    [ HP.class_ CN.form ]
    [ HH.input
        [ HP.type_ HP.InputText
        , HP.placeholder "Search string"
        , ARIA.label "Search string"
        , HE.onValueInput $ HE.input \str → right ∘ UpdateSearch str
        , HP.value state.searchString
        ]
    , HH.button
        [ HE.onClick $ HE.input_ (right ∘ UpdateSearch "")
        ]
        [ R.clearFieldIcon "Clear search string" ]
    ]

cardEval ∷ CC.CardEvalQuery ~> DSL
cardEval = case _ of
  CC.Activate next →
    pure next
  CC.Deactivate next →
    pure next
  CC.Save k → do
    input ← H.gets _.searchString
    pure ∘ k $ Card.Search input
  CC.Load card next → do
    case card of
      Card.Search input → H.modify (_ { searchString = input })
      _ → pure unit
    pure next
  CC.ReceiveInput _ _ next →
    pure next
  CC.ReceiveOutput _ _ next →
    pure next
  CC.ReceiveState _ next →
    pure next
  CC.ReceiveDimensions _ reply →
    pure $ reply LOD.High

searchEval ∷ Query ~> DSL
searchEval (UpdateSearch str next) = do
  H.modify (_ { searchString = str })
  H.raise CC.modelUpdate
  pure next
