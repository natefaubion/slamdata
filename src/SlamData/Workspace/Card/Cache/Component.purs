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

import Halogen as H
import Halogen.HTML.Events as HE
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Halogen.HTML.Properties.ARIA as ARIA

import SlamData.Render.ClassName as CN
import SlamData.Workspace.Card.Cache.Component.Query (Query(..))
import SlamData.Workspace.Card.Cache.Component.State (State, _confirmedPath, _pathString, initialState)
import SlamData.Workspace.Card.CardType as CT
import SlamData.Workspace.Card.Component as CC
import SlamData.Workspace.Card.Model as Card
import SlamData.Workspace.Card.Port as Port
import SlamData.Workspace.LevelOfDetails (LevelOfDetails(..))

import Utils.Path as PU

type CacheDSL = CC.InnerCardDSL State Query
type CacheHTML = CC.InnerCardHTML Query

cacheCardComponent ∷ CC.CardOptions → CC.CardComponent
cacheCardComponent =
  CC.makeCardComponent CT.Cache $ H.component
    { render
    , eval: coproduct cardEval saveEval
    , initialState: const initialState
    , receiver: const Nothing
    }

render ∷ State → CacheHTML
render state =
  HH.div
    [ HP.class_ CN.form ]
    [ HH.input
        [ HP.type_ HP.InputText
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

cardEval ∷ CC.CardEvalQuery ~> CacheDSL
cardEval = case _ of
  CC.Activate next →
    pure next
  CC.Deactivate next →
    pure next
  CC.Save k →
    k ∘ Card.Cache ∘ map PU.printAnyFilePath <$> H.gets _.confirmedPath
  CC.Load card next → do
    case card of
      Card.Cache s →
        H.modify
          $ (_pathString .~ s)
          ∘ (_confirmedPath .~ (PU.parseAnyFilePath =<< s))
      _ → pure unit
    pure next
  CC.ReceiveInput _ _ next →
    pure next
  CC.ReceiveOutput _ varMap next → do
    for_ (Port.extractAnyFilePath varMap) \anyPath →
      H.modify
        $ (_pathString ?~ PU.printAnyFilePath anyPath)
        ∘ (_confirmedPath ?~ anyPath)
    pure next
  CC.ReceiveState input next →
    pure next
  CC.ReceiveDimensions _ reply →
    pure (reply High)

saveEval ∷ Query ~> CacheDSL
saveEval = case _ of
  UpdatePathString str next →
    H.modify (_pathString ?~ str) $> next
  ConfirmPathString next → do
    pathString ← H.gets _.pathString
    H.modify $ \st → st { confirmedPath = PU.parseAnyFilePath =<< pathString }
    H.raise CC.modelUpdate
    pure next
