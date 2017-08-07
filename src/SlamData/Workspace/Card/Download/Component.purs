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

module SlamData.Workspace.Card.Download.Component
  ( downloadComponent
  , module SlamData.Workspace.Card.Download.Component.Query
  ) where

import SlamData.Prelude

import Data.Lens ((^?))
import Data.URI as URI
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Halogen.HTML.Properties.ARIA as ARIA
import SlamData.Download.Model as D
import SlamData.Quasar (reqHeadersToJSON)
import SlamData.Quasar.Auth as API
import SlamData.Render.ClassName as CN
import SlamData.Render.CSS.New as CSS
import SlamData.Wiring as Wiring
import SlamData.Workspace.Card.CardType as CT
import SlamData.Workspace.Card.Component as CC
import SlamData.Workspace.Card.Download.Component.Query (Query)
import SlamData.Workspace.Card.Download.Component.State (State, initialState)
import SlamData.Workspace.Card.Model as Card
import SlamData.Workspace.Card.Port as Port
import SlamData.Workspace.Card.Port.VarMap as VM
import SlamData.Workspace.LevelOfDetails as LOD

import Utils.DOM (Font(Font), getTextWidthPure)

type HTML = CC.InnerCardHTML Query
type DSL = CC.InnerCardDSL State Query

downloadComponent ∷ CC.CardOptions → CC.CardComponent
downloadComponent =
  CC.makeCardComponent CT.Download $ H.component
    { render: render
    , eval: evalCard ⨁ (absurd ∘ unwrap)
    , initialState: const initialState
    , receiver: const Nothing
    }

render ∷ State → HTML
render state =
  HH.div_
    [ HH.a
        [ HP.classes
          [ CSS.formButton
          , CN.btn
          , CN.btnDefault
          ]
        , HP.href state.url
        , ARIA.label $ fullDownloadString state
        , HP.title $ fullDownloadString state
        ]
        [ HH.text $ buttonText state ]
    ]

buttonText ∷ State → String
buttonText state
  | state.levelOfDetails ≡ LOD.Low = "Download"
  | otherwise = fullDownloadString state

fullDownloadString ∷ State → String
fullDownloadString state = "Download " ⊕ state.fileName

evalCard ∷ CC.CardEvalQuery ~> DSL
evalCard = case _ of
  CC.Activate next →
    pure next
  CC.Deactivate next →
    pure next
  CC.Save k →
    pure (k Card.Download)
  CC.Load json next →
    pure next
  CC.ReceiveInput input _ next → do
    for_ (input ^? Port._DownloadOptions) handleDownloadPort
    pure next
  CC.ReceiveOutput _ _ next → do
    pure next
  CC.ReceiveState _ next → do
    pure next
  CC.ReceiveDimensions dims reply → do
    textWidth ← H.gets $ flip getTextWidthPure (Font "normal 14px Ubuntu") ∘ _.fileName
    let
      buttonPadding = 24.0
      cardPadding = 24.0
      grippersWidth = 48.0
    H.modify (_ { levelOfDetails =
        if dims.width < textWidth + buttonPadding + cardPadding + grippersWidth
        then LOD.Low
        else LOD.High })
    pure $ reply LOD.High

handleDownloadPort ∷ Port.DownloadPort → DSL Unit
handleDownloadPort opts = do
  { path } ← Wiring.expose
  let
    ext = D.extension opts.compress opts.options
    url hs = URI.printAbsoluteURI $ VM.downloadUrl (Just (headersPart hs)) path opts.resource
    headersPart hs =
      show
        $ reqHeadersToJSON
        $ append hs
        $ D.toHeaders opts opts.compress
        $ Just (opts.targetName ⊕ ext)
  hs ← H.lift API.authHeaders
  H.modify (_ { url = url hs, fileName = opts.targetName ⊕ ext })
