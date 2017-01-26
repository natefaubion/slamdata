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

module SlamData.Workspace.Card.Common.EvalQuery
  ( CardEvalQuery(..)
  , ModelUpdateType(..)
  , raiseUpdatedC
  , raiseUpdatedC'
  , raiseUpdatedP
  , raiseUpdatedP'
  ) where

import SlamData.Prelude

import Control.Monad.Aff.Free (class Affable)
import Control.Monad.Aff.AVar (AVAR)

import Halogen as H
import Halogen.Component.Utils (raise, raise')

import SlamData.Workspace.Card.Eval.Monad (EvalState)
import SlamData.Workspace.Card.Model (AnyCardModel)
import SlamData.Workspace.Card.Port as Port

-- | The query algebra shared by the inner parts of a card component.
-- |
-- | - `EvalCard` is a command sent from the deck when a card component needs
-- |   updating during the evaluation process. An input value from the previous
-- |   card in the deck is provided, along with the output from the card's model
-- |   evaluator. The card cannot return a new port value, it's eval is only
-- |   allowed to update the card component state.
-- |
-- | - `Activate` is sent when the card component becomes the active card.
-- |
-- | - `Save` requests the current model value for the card, used when
-- |   serialising model and when running the cards in a deck.
-- |
-- | - `Load` attempts to populate the card with a model value. This function
-- |   is partial, in the sense that the passed model should use the appropriate
-- |   constructor for the card type. The behaviour is unspecified when a
-- |   mismatched value is passed in.
-- |
-- | - `SetDimensions` is used to notify the card of the size of the deck upon
-- |   resize and initialisation.
-- |
-- | - `ModelUpdated` is a query the card sends to itself so that the deck can
-- |   peek the query and know that the deck needs saving/evaluating.
data CardEvalQuery a
  = Activate a
  | Deactivate a
  | Save (AnyCardModel → a)
  | Load AnyCardModel a
  | ReceiveState EvalState a
  | ReceiveInput Port.Port Port.DataMap a
  | ReceiveOutput Port.Port Port.DataMap a
  | ReceiveDimensions { width ∷ Number, height ∷ Number } a
  | ZoomIn a
  | ModelUpdated ModelUpdateType a

-- | This type is used to indicate whether a model update only affects the
-- | internal state of the card (and therefore only requires saving), or whether
-- | the model changes in a way that will affect the evaluated output of the
-- | card.
data ModelUpdateType
  = StateOnlyUpdate
  | EvalModelUpdate
  | EvalStateUpdate EvalState

-- | Raises a `ModelUpdateType` self-query for a card that is a standalone
-- | component.
raiseUpdatedC
  ∷ ∀ s g eff
  . (Affable (avar ∷ AVAR | eff) g, Functor g)
  ⇒ ModelUpdateType
  → H.ComponentDSL s CardEvalQuery g Unit
raiseUpdatedC updateType = raise $ H.action $ ModelUpdated updateType

-- | Raises a `ModelUpdateType` self-query for a card that is a standalone
-- | component with an expanded query algebra.
raiseUpdatedC'
  ∷ ∀ f s g eff
  . (Affable (avar ∷ AVAR | eff) g, Functor g)
  ⇒ ModelUpdateType
  → H.ComponentDSL s (CardEvalQuery ⨁ f) g Unit
raiseUpdatedC' updateType = raise $ left $ H.action $ ModelUpdated updateType

-- | Raises a `ModelUpdateType` self-query for a card that is a parent
-- | component.
raiseUpdatedP
  ∷ ∀ s s' f' p g eff
  . (Affable (avar ∷ AVAR | eff) g, Functor g)
  ⇒ ModelUpdateType
  → H.ParentDSL s s' CardEvalQuery f' g p Unit
raiseUpdatedP updateType = raise' $ H.action $ ModelUpdated updateType

-- | Raises a `ModelUpdateType` self-query for a card that is a parent
-- | component with an expanded query algebra.
raiseUpdatedP'
  ∷ ∀ s s' f f' p g eff
  . (Affable (avar ∷ AVAR | eff) g, Functor g)
  ⇒ ModelUpdateType
  → H.ParentDSL s s' (CardEvalQuery ⨁ f) f' g p Unit
raiseUpdatedP' updateType = raise' $ left $ H.action $ ModelUpdated updateType
