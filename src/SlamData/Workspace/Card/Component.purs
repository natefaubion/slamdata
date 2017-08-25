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

module SlamData.Workspace.Card.Component
  ( CardComponent
  , InnerCardComponent
  , InnerCardDSL
  , InnerCardHTML
  , InnerCardParentDSL
  , InnerCardParentHTML
  , makeCardComponent
  , module SlamData.Workspace.Card.Common
  , module CQ
  , module CS
  , module EQ
  ) where

import SlamData.Prelude

import Data.Lens ((.~))
import Halogen as H
import Halogen.Component.Utils (busEventSource)
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.HTML.Properties.ARIA as ARIA
import SlamData.Monad (Slam)
import SlamData.Render.ClassName as CN
import SlamData.Render.Icon as I
import SlamData.Wiring as Wiring
import SlamData.Workspace.AccessType as AccessType
import SlamData.Workspace.Card.CardType as CT
import SlamData.Workspace.Card.Common (CardOptions)
import SlamData.Workspace.Card.Common.EvalQuery as EQ
import SlamData.Workspace.Card.Component.CSS as CSS
import SlamData.Workspace.Card.Component.Query as CQ
import SlamData.Workspace.Card.Component.State as CS
import SlamData.Workspace.Class (navigateToDeck)
import SlamData.Workspace.Eval.Card as Card
import SlamData.Workspace.Eval.Persistence as P
import SlamData.Workspace.LevelOfDetails (LevelOfDetails(..))

-- | Type synonym for the full type of a card component.
type CardComponent = H.Component HH.HTML CQ.CardQuery CQ.Input Void Slam
type CardDSL f = H.ParentDSL CS.CardState CQ.CardQuery (CQ.InnerCardQuery f) Unit Void Slam
type CardHTML f = H.ParentHTML CQ.CardQuery (CQ.InnerCardQuery f) Unit Slam

type InnerCardComponent f = H.Component HH.HTML (CQ.InnerCardQuery f) Unit EQ.CardEvalMessage Slam
type InnerCardDSL s f = H.ComponentDSL s (CQ.InnerCardQuery f) CQ.CardEvalMessage Slam
type InnerCardHTML f = H.ComponentHTML (CQ.InnerCardQuery f)
type InnerCardParentDSL s f g p = H.ParentDSL s (CQ.InnerCardQuery f) g p CQ.CardEvalMessage Slam
type InnerCardParentHTML f g p = H.ParentHTML (CQ.InnerCardQuery f) g p Slam

cardRef ∷ H.RefLabel
cardRef = H.RefLabel "card"

-- | Constructs a card component from a record with the necessary properties and
-- | a render function.
makeCardComponent
  ∷ ∀ f
  . CT.CardType
  → InnerCardComponent f
  → CardOptions
  → CardComponent
makeCardComponent cardType component options =
  H.lifecycleParentComponent
    { render
    , eval
    , initialState: CS.initialState
    , initializer: Just (H.action CQ.Initialize)
    , finalizer: Nothing
    , receiver: HE.input CQ.HandleInput
    }
  where
  displayCoord ∷ Card.DisplayCoord
  displayCoord = options.cursor × options.cardId

  render ∷ CS.CardState → CardHTML f
  render st =
    HH.div
      [ HP.classes $ [ CSS.deckCard ]
      , ARIA.label $ (CT.name cardType) ⊕ " card"
      , HP.ref cardRef
      ]
      $ fold [cardLabel, card]
    where
    icon ∷ CardHTML f
    icon = CT.icon cardType # I.unIconHTML

    cardLabel ∷ Array (CardHTML f)
    cardLabel
      | cardType ≡ CT.draftboard = [ ]
      | otherwise =
          [ HH.div
              [ HP.classes [CSS.cardHeader]
              ]
              [ HH.div
                  [ HP.class_ CSS.cardName ]
                  [ icon
                  , HH.p_ [ HH.text $ CT.name cardType ]
                  ]
              ]
          ]

    card ∷ Array (CardHTML f)
    card =
      case st.status, st.levelOfDetails of
        CS.Pending, _ →
          []
        _, High →
          [ HH.fieldset
              [ HP.classes $ CT.cardClasses cardType
              , HP.disabled disabled
              ]
              [ HH.slot unit component unit (HE.input CQ.HandleCardMessage) ]
          ]
        _, Low →
          [ HH.fieldset
              [ HP.classes $ CT.cardClasses cardType <> [ CN.hidden ]
              , HP.disabled disabled
              ]
              [ HH.slot unit component unit (HE.input CQ.HandleCardMessage) ]
          , HH.div
              [ HP.class_ (HH.ClassName "card-input-minimum-lod") ]
              [ HH.button
                  [ ARIA.label "Zoom or resize"
                  , HP.title "Zoom or resize"
                  , HE.onClick (HE.input_ CQ.ZoomIn)
                  ]
                  [ icon
                  , HH.text "Zoom or resize"
                  ]
              ]
          ]

    disabled ∷ Boolean
    disabled =
      (not $ CT.consumerInteractable cardType)
        ∧ (AccessType.isReadOnly st.accessType)

  eval ∷ CQ.CardQuery ~> CardDSL f
  eval = case _ of
    CQ.Initialize next → do
      st ← H.get
      when (st.status ≡ CS.Active) do
        initializeInnerCard
        queryInnerCard EQ.Activate
      H.modify ∘ (CS._accessType .~ _)
        =<< _.accessType <$> Wiring.expose
      pure next
    CQ.UpdateDimensions next → do
      st ← H.get
      when (st.status ≠ CS.Pending) updateDimensions
      pure next
    CQ.PreloadCard next → do
      st ← H.get
      when (st.status ≡ CS.Pending) do
        H.modify _ { status = CS.Initialized }
        initializeInnerCard
      pure next
    CQ.HandleInput input next → do
      st ← H.get
      case st.status of
        CS.Pending | input.active → do
          H.modify _ { status = CS.Active }
          initializeInnerCard
          queryInnerCard EQ.Activate
        CS.Initialized | input.active → do
          H.modify _ { status = CS.Active }
          queryInnerCard EQ.Activate
        CS.Inactive | input.active → do
          H.modify _ { status = CS.Active }
          queryInnerCard EQ.Activate
        CS.Active | not input.active → do
          H.modify _ { status = CS.Inactive }
          queryInnerCard EQ.Deactivate
        _ → pure unit
      when (st.width ≠ input.width || st.height ≠ input.height) do
        H.modify _ { width = input.width, height = input.height }
        updateDimensions
      pure next
    CQ.HandleEvalMessage msg next → do
      case msg of
        Card.Pending source (evalPort × varMap) → do
          queryInnerCard $ EQ.ReceiveInput evalPort varMap
        Card.Complete source (evalPort × varMap) → do
          queryInnerCard $ EQ.ReceiveOutput evalPort varMap
        Card.StateChange source evalState →
          queryInnerCard $ EQ.ReceiveState evalState
        Card.ModelChange source evalModel →
          when (source ≠ displayCoord) do
            queryInnerCard $ EQ.Load evalModel
      pure next
    CQ.HandleCardMessage msg next → do
      case msg of
        EQ.ModelUpdated (EQ.EvalModelUpdate evalOpts) → do
          model ← H.query unit (left (H.request EQ.Save))
          for_ model (H.lift ∘ P.publishCardChange evalOpts displayCoord)
        EQ.ModelUpdated (EQ.EvalStateUpdate fn) → do
          H.lift $ P.publishCardStateChange displayCoord fn
      pure next
    CQ.ZoomIn next → do
      H.lift $ navigateToDeck options.cursor
      pure next

  initializeInnerCard ∷ CardDSL f Unit
  initializeInnerCard = do
    cell ← H.lift $ P.getCard options.cardId
    for_ cell \{ bus, model, input, output, state } → do
      H.subscribe $ busEventSource (\msg → CQ.HandleEvalMessage msg H.Listening) bus
      H.modify _ { bus = Just bus, status = CS.Active }
      updateDimensions
      queryInnerCard $ EQ.Load model
      for_ input (queryInnerCard ∘ uncurry EQ.ReceiveInput)
      for_ state (queryInnerCard ∘ EQ.ReceiveState)
      for_ output (queryInnerCard ∘ uncurry EQ.ReceiveOutput)

  updateDimensions ∷ CardDSL f Unit
  updateDimensions = do
    { width, height, levelOfDetails } ← H.get
    mbLod ← H.query unit $ left $ H.request (EQ.ReceiveDimensions { width, height })
    for_ mbLod \lod → when (levelOfDetails ≠ lod) do
      H.modify _ { levelOfDetails = lod }

queryInnerCard ∷ ∀ f. H.Action EQ.CardEvalQuery → CardDSL f Unit
queryInnerCard q =
  void $ H.query unit (left (H.action q))
