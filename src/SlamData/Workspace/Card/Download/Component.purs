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
  , module SlamData.Workspace.Card.Download.Component.State
  ) where

import SlamData.Prelude

import Data.Lens ((^?), (.~))
import Data.Lens as Lens
import Data.Path.Pathy (printPath)

import Halogen as H
import Halogen.HTML.Indexed as HH
import Halogen.HTML.Properties.Indexed as HP
import Halogen.HTML.Properties.Indexed.ARIA as ARIA

import Quasar.Paths as Paths

import SlamData.Download.Model as D
import SlamData.Effects (Slam)
import SlamData.Quasar (reqHeadersToJSON, encodeURI)
import SlamData.Quasar.Auth as API
import SlamData.Render.CSS.New as CSS
import SlamData.Workspace.Card.CardType as CT
import SlamData.Workspace.Card.Component as CC
import SlamData.Workspace.Card.Download.Component.Query (Query, QueryP)
import SlamData.Workspace.Card.Download.Component.State (State, _fileName, _levelOfDetails, _url, initialState)
import SlamData.Workspace.Card.Model as Card
import SlamData.Workspace.Card.Port as Port
import SlamData.Workspace.LevelOfDetails (LevelOfDetails(..))

import Utils.Path as UP
import Utils.DOM (getTextWidthPure)

type HTML = H.ComponentHTML QueryP
type DSL = H.ComponentDSL State QueryP Slam

downloadComponent ∷ CC.CardComponent
downloadComponent = CC.makeCardComponent
  { cardType: CT.Download
  , component: H.component { render, eval }
  , initialState: initialState
  , _State: CC._DownloadState
  , _Query: CC.makeQueryPrism CC._DownloadQuery
  }

render ∷ State → HTML
render state =
  HH.div_
    [ HH.a
        [ HP.class_ CSS.formButton
        , HP.href state.url
        , ARIA.label $ fullDownloadString state
        , HP.title $ fullDownloadString state
        ]
        [ HH.text $ buttonText state ]
    ]

buttonText ∷ State → String
buttonText state
  | state.levelOfDetails ≡ Low = "Download"
  | otherwise = fullDownloadString state

fullDownloadString ∷ State → String
fullDownloadString state = "Download " ⊕ state.fileName

eval ∷ QueryP ~> DSL
eval = coproduct cardEval (absurd ∘ getConst)

cardEval ∷ CC.CardEvalQuery ~> DSL
cardEval = case _ of
  CC.EvalCard info output next → do
    for_ (info.input ^? Lens._Just ∘ Port._DownloadOptions) handleDownloadPort
    pure next
  CC.Activate next →
    pure next
  CC.Save k →
    pure (k Card.Download)
  CC.Load json next →
    pure next
  CC.SetDimensions dims next → do
    textWidth ← H.gets $ flip getTextWidthPure "normal 14px Ubuntu" ∘ _.fileName
    let
      buttonPadding = 24.0
      cardPadding = 24.0
      grippersWidth = 48.0
    H.modify
      $ _levelOfDetails
      .~ if dims.width < textWidth + buttonPadding + cardPadding + grippersWidth
           then Low
           else High
    pure next
  CC.ModelUpdated _ next →
    pure next
  CC.ZoomIn next →
    pure next

handleDownloadPort ∷ Port.DownloadPort → DSL Unit
handleDownloadPort opts = do
  hs ← H.fromEff API.authHeaders
  H.modify $ _url .~ url hs
  let
    fileName = UP.getNameStr $ Right opts.resource
    ext | opts.compress = ".zip"
    ext | isRight opts.options = ".json"
    ext | otherwise = ".csv"
  H.modify $ _fileName .~ (fileName ⊕ ext)
  where

  url hs =
    (encodeURI (printPath Paths.data_ ⊕ printPath opts.resource))
    ⊕ headersPart hs

  headersPart hs =
    "?request-headers="
      ⊕ (Global.encodeURIComponent
           $ show
           $ reqHeadersToJSON
           $ append hs
           $ D.toHeaders opts)
