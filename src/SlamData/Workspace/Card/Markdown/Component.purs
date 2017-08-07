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

module SlamData.Workspace.Card.Markdown.Component
  ( markdownComponent
  , module SlamData.Workspace.Card.Markdown.Component.Query
  ) where

import SlamData.Prelude

import Data.Lens ((^?))
import Data.StrMap as SM
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import SlamData.Render.ClassName as CN
import SlamData.Wiring as Wiring
import SlamData.Workspace.Card.CardId as CID
import SlamData.Workspace.Card.CardType as CT
import SlamData.Workspace.Card.Component as CC
import SlamData.Workspace.Card.Eval.State as CES
import SlamData.Workspace.Card.Markdown.Component.Query (Query(..))
import SlamData.Workspace.Card.Markdown.Component.State (State, initialState)
import SlamData.Workspace.Card.Markdown.Model as MD
import SlamData.Workspace.Card.Model as Card
import SlamData.Workspace.Card.Port as Port
import SlamData.Workspace.Deck.DeckId as DID
import SlamData.Workspace.LevelOfDetails as LOD
import Text.Markdown.SlamDown.Halogen.Component as SD

type MarkdownQuery = SD.SlamDownQuery MD.MarkdownExpr
type HTML = CC.InnerCardParentHTML Query MarkdownQuery Unit
type DSL = CC.InnerCardParentDSL State Query MarkdownQuery Unit

markdownComponent ∷ CC.CardOptions → CC.CardComponent
markdownComponent options =
  CC.makeCardComponent CT.Markdown (H.lifecycleParentComponent
    { render: render uniqueCardId
    , eval: evalCard ⨁ evalComponent
    , initialState: const initialState
    , initializer: Just $ right $ H.action Init
    , finalizer: Nothing
    , receiver: const Nothing
    }) options
  where
  uniqueCardId =
    foldMap DID.toString options.cursor <> CID.toString options.cardId

render
  ∷ String
  → State
  → HTML
render formName st =
  case st.browserFeatures of
    Nothing → HH.div_ []
    Just browserFeatures → do
      HH.div
        [ HP.class_ CN.form ]
        [ HH.slot
            unit
            (SD.slamDownComponent { formName, browserFeatures })
            unit
            (HE.input (map right ∘ HandleMessage))
        ]

evalCard ∷ CC.CardEvalQuery ~> DSL
evalCard = case _ of
  CC.Activate next →
    pure next
  CC.Deactivate next →
    pure next
  CC.Save k → do
    state ← H.gets _.state
    pure ∘ k $ Card.Markdown state
  CC.Load card next → do
    case card of
      Card.Markdown state → void $ do
        H.modify (_ { state = state })
        H.query unit $ H.action (SD.PopulateForm state)
      _ → pure unit
    pure next
  CC.ReceiveInput input _ next → do
    for_ (input ^? Port._SlamDown) \sd → do
      void $ H.query unit $ H.action (SD.SetDocument sd)
    pure next
  CC.ReceiveOutput _ _ next →
    pure next
  CC.ReceiveState evalState next → do
    for_ (evalState ^? CES._SlamDown) \state → do
      H.modify _ { state = state }
      void $ H.query unit $ H.action (SD.PopulateForm state)
    pure next
  CC.ReceiveDimensions _ reply →
    pure $ reply LOD.High

evalComponent ∷ Query ~> DSL
evalComponent = case _ of
  Init next → do
    w ← Wiring.expose
    H.modify (_ { browserFeatures = Just w.browserFeatures })
    pure next
  HandleMessage _ next → do
    state ← fromMaybe SM.empty <$> H.query unit (H.request SD.GetFormState)
    H.modify (_ { state = state })
    H.raise CC.modelUpdate
    pure next
