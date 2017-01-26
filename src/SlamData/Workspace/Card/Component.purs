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
  , makeCardComponent
  , module SlamData.Workspace.Card.Component.Def
  , module SlamData.Workspace.Card.Common
  , module CQ
  , module CS
  , module EQ
  ) where

import SlamData.Prelude

import Control.Monad.Aff (later)
import Control.Monad.Aff.EventLoop (break')
import Control.Monad.Aff.Free (fromAff)

import Data.Foldable (elem)
import Data.Lens (Prism', (.~), review, preview, clonePrism)

import DOM.HTML.HTMLElement (getBoundingClientRect)

import Halogen as H
import Halogen.Component.Utils (subscribeToBus', liftH')
import Halogen.HTML.Indexed as HH
import Halogen.HTML.Properties.Indexed as HP
import Halogen.HTML.Properties.Indexed.ARIA as ARIA

import SlamData.Monad (Slam)
import SlamData.Workspace.Card.CardType (CardType(..), cardClasses, cardName, cardIconDarkSrc)
import SlamData.Workspace.Card.Common.EvalQuery as EQ
import SlamData.Workspace.Card.Common (CardOptions)
import SlamData.Workspace.Card.Component.CSS as CSS
import SlamData.Workspace.Card.Component.Def (CardDef, makeQueryPrism, makeQueryPrism')
import SlamData.Workspace.Card.Component.Query as CQ
import SlamData.Workspace.Card.Component.State as CS
import SlamData.Workspace.Eval.Card as Card
import SlamData.Workspace.Eval.Persistence as P

-- | Type synonym for the full type of a card component.
type CardComponent = H.Component CS.CardStateP CQ.CardQueryP Slam
type CardDSL = H.ParentDSL CS.CardState CS.AnyCardState CQ.CardQuery CQ.InnerCardQuery Slam Unit
type CardHTML = H.ParentHTML CS.AnyCardState CQ.CardQuery CQ.InnerCardQuery Slam Unit

-- | Card component factory
makeCardComponent
  ∷ ∀ s f
  . CardDef s f ()
  → CardComponent
makeCardComponent def = makeCardComponentPart def render
  where
  render
    ∷ H.Component CS.AnyCardState CQ.InnerCardQuery Slam
    → CS.AnyCardState
    → CS.CardState
    → CardHTML
  render component initialState st =
    HH.div
      [ HP.classes $ [ CSS.deckCard ]
      , ARIA.label $ (cardName def.cardType) ⊕ " card"
      , HP.ref (H.action ∘ CQ.SetElement)
      ]
      $ fold [cardLabel, card]
    where
    cardLabel ∷ Array CardHTML
    cardLabel
      | def.cardType `elem` [ Draftboard ] = []
      | otherwise =
          [ HH.div
              [ HP.classes [CSS.cardHeader]
              ]
              [ HH.div
                  [ HP.class_ CSS.cardName ]
                  [ HH.img [ HP.src $ cardIconDarkSrc def.cardType ]
                  , HH.p_ [ HH.text $ cardName def.cardType ]
                  ]
              ]
          ]
    card ∷ Array CardHTML
    card =
      if st.pending
        then []
        else
          [ HH.div
              [ HP.classes $ cardClasses def.cardType ]
              [ HH.slot unit \_ → { component, initialState } ]
          ]

-- | Constructs a card component from a record with the necessary properties and
-- | a render function.
makeCardComponentPart
  ∷ ∀ s f r
  . CardDef s f r
  → (H.Component CS.AnyCardState CQ.InnerCardQuery Slam
     → CS.AnyCardState
     → CS.CardState
     → CardHTML)
  → CardComponent
makeCardComponentPart def render =
  H.lifecycleParentComponent
    { render: render component initialState
    , eval
    , peek: Just (coproduct peek (const (pure unit)) ∘ H.runChildF)
    , initializer: Just (H.action CQ.Initialize)
    , finalizer: Just (H.action CQ.Finalize)
    }
  where

  displayCoord ∷ Card.DisplayCoord
  displayCoord = def.options.cursor × def.options.cardId

  _State ∷ Prism' CS.AnyCardState s
  _State = clonePrism def._State

  _Query ∷ ∀ a. Prism' (CQ.InnerCardQuery a) (f a)
  _Query = clonePrism def._Query

  component ∷ H.Component CS.AnyCardState CQ.InnerCardQuery Slam
  component =
    H.transform
      (review _State) (preview _State)
      (review _Query) (preview _Query)
      def.component

  initialState ∷ CS.AnyCardState
  initialState = review _State def.initialState

  eval ∷ CQ.CardQuery ~> CardDSL
  eval = case _ of
    CQ.Initialize next → do
      initializeInnerCard
      pure next
    CQ.Finalize next → do
      H.gets _.breaker >>= traverse_ (fromAff ∘ break')
      pure next
    CQ.ActivateCard next →
      queryInnerCard EQ.Activate $> next
    CQ.DeactivateCard next →
      queryInnerCard EQ.Deactivate $> next
    CQ.SetElement el next →
      H.modify (CS._element .~ el) $> next
    CQ.UpdateDimensions next → do
      H.gets _.element >>= traverse_ \el -> do
        { width, height } ← H.fromEff (getBoundingClientRect el)
        unless (width ≡ zero ∧ height ≡ zero) do
          queryInnerCard $ EQ.ReceiveDimensions { width, height }
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

  initializeInnerCard ∷ CardDSL Unit
  initializeInnerCard = do
    cell ← liftH' $ P.getCard def.options.cardId
    for_ cell \{ bus, model, input, output, state } → do
      breaker ← subscribeToBus' (H.action ∘ CQ.HandleEvalMessage) bus
      H.modify _
        { breaker = Just breaker
        , bus = Just bus
        , pending = false
        }
      -- TODO: We need to defer these because apparently Halogen has bad
      -- ordering with regard to child initializers. This should be fixed
      -- in Halogen Next.
      H.fromAff $ later (pure unit)
      queryInnerCard $ EQ.Load model
      for_ input (queryInnerCard ∘ uncurry EQ.ReceiveInput)
      for_ state (queryInnerCard ∘ EQ.ReceiveState)
      for_ output (queryInnerCard ∘ uncurry EQ.ReceiveOutput)
      eval (CQ.UpdateDimensions unit)

  peek ∷ ∀ a. EQ.CardEvalQuery a → CardDSL Unit
  peek = case _ of
    EQ.ModelUpdated EQ.EvalModelUpdate _ → do
      model ← H.query unit (left (H.request EQ.Save))
      for_ model (liftH' ∘ P.publishCardChange displayCoord)
    EQ.ModelUpdated (EQ.EvalStateUpdate es) _ → do
      liftH' $ P.publishCardStateChange displayCoord es
    _ →
      pure unit

queryInnerCard ∷ H.Action EQ.CardEvalQuery → CardDSL Unit
queryInnerCard q =
  void $ H.query unit (left (H.action q))
