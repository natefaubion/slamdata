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

module SlamData.Workspace.Card.Cache.Component
  ( cacheCardComponent
  , module SlamData.Workspace.Card.Cache.Component.State
  , module SlamData.Workspace.Card.Cache.Component.Query
  ) where

import SlamData.Prelude

import Data.Lens ((.~), (?~))
import Data.Path.Pathy as Path

import Halogen as H
import Halogen.HTML.Events.Indexed as HE
import Halogen.HTML.Indexed as HH
import Halogen.HTML.Properties.Indexed as HP
import Halogen.HTML.Properties.Indexed.ARIA as ARIA

import SlamData.Effects (Slam)
import SlamData.Render.CSS as CSS
import SlamData.Workspace.Card.Cache.Component.Query (QueryP, Query(..))
import SlamData.Workspace.Card.Cache.Component.State (State, _confirmedPath, _pathString, initialState)
import SlamData.Workspace.Card.CardType as CT
import SlamData.Workspace.Card.Component as CC
import SlamData.Workspace.Card.Model as Card
import SlamData.Workspace.Card.Port as Port

import Utils.Path as PU

type CacheHTML = H.ComponentHTML QueryP
type CacheDSL = H.ComponentDSL State QueryP Slam

cacheCardComponent ∷ CC.CardComponent
cacheCardComponent = CC.makeCardComponent
  { cardType: CT.Cache
  , component: H.component { render, eval }
  , initialState: initialState
  , _State: CC._CacheState
  , _Query: CC.makeQueryPrism CC._CacheQuery
  }

render ∷ State → CacheHTML
render state =
  HH.div
    [ HP.class_ CSS.form ]
    [ HH.input
        [ HP.inputType HP.InputText
        , HP.placeholder "Cache file destination"
        , ARIA.label "Cache file destination"
        , HE.onValueInput $ HE.input \s → right ∘ UpdatePathString s
        , HP.value $ fromMaybe "" state.pathString
        ]
    , HH.button
          [ HP.title "Confirm file destination"
          , ARIA.label "Confirm file destination"
          , HP.disabled $ isNothing $ PU.parseFilePath =<< state.pathString
          , HE.onClick (HE.input_ $ right ∘ ConfirmPathString)
          ]
          [ HH.text "Confirm" ]
    ]

eval ∷ QueryP ~> CacheDSL
eval = coproduct cardEval saveEval

cardEval ∷ CC.CardEvalQuery ~> CacheDSL
cardEval = case _ of
  CC.EvalCard info output next → do
    for_ output case _ of
      Port.TaggedResource { resource } →
        H.modify
          $ (_pathString ?~ Path.printPath resource)
          ∘ (_confirmedPath ?~ resource)
      _ → pure unit
    pure next
  CC.Activate next →
    pure next
  CC.Save k →
    k ∘ Card.Cache ∘ map Path.printPath <$> H.gets _.confirmedPath
  CC.Load card next → do
    case card of
      Card.Cache s →
        H.modify
          $ (_pathString .~ s)
          ∘ (_confirmedPath .~ (PU.parseFilePath =<< s))
      _ → pure unit
    pure next
  CC.SetDimensions _ next →
    pure next
  CC.ModelUpdated _ next →
    pure next
  CC.ZoomIn next →
    pure next

saveEval ∷ Query ~> CacheDSL
saveEval = case _ of
  UpdatePathString str next →
    H.modify (_pathString ?~ str) $> next
  ConfirmPathString next → do
    pathString ← H.gets _.pathString
    H.modify $ \st → st { confirmedPath = PU.parseFilePath =<< pathString }
    CC.raiseUpdatedC' CC.EvalModelUpdate
    pure next
