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
  , CardEvalMessage(..)
  , ModelUpdateType(..)
  , ModelUpdateOption(..)
  , modelUpdate
  , modelUpdateSilently
  , stateUpdate
  , shouldTriggerEval
  , stateAlter
  ) where

import SlamData.Prelude

import SlamData.Workspace.Card.Eval.Monad (EvalState)
import SlamData.Workspace.Card.Model (AnyCardModel)
import SlamData.Workspace.Card.Port as Port
import SlamData.Workspace.LevelOfDetails (LevelOfDetails)

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
data CardEvalQuery a
  = Activate a
  | Deactivate a
  | Save (AnyCardModel → a)
  | Load AnyCardModel a
  | ReceiveState EvalState a
  | ReceiveInput Port.Port Port.VarMap a
  | ReceiveOutput Port.Port Port.VarMap a
  | ReceiveDimensions { width ∷ Number, height ∷ Number } (LevelOfDetails → a)

data CardEvalMessage
  = ModelUpdated ModelUpdateType

-- | This type is used to indicate whether a model update only affects the
-- | internal state of the card (and therefore only requires saving), or whether
-- | the model changes in a way that will affect the evaluated output of the
-- | card.
data ModelUpdateType
  = EvalModelUpdate ModelUpdateOption
  | EvalStateUpdate (Maybe EvalState → Maybe EvalState)

data ModelUpdateOption
  = TriggerEval
  | Silent

modelUpdate ∷ CardEvalMessage
modelUpdate = ModelUpdated (EvalModelUpdate TriggerEval)

modelUpdateSilently ∷ CardEvalMessage
modelUpdateSilently = ModelUpdated (EvalModelUpdate Silent)

stateUpdate ∷ EvalState → CardEvalMessage
stateUpdate = ModelUpdated ∘ EvalStateUpdate ∘ const ∘ Just

stateAlter ∷ (Maybe EvalState → Maybe EvalState) → CardEvalMessage
stateAlter = ModelUpdated ∘ EvalStateUpdate

shouldTriggerEval ∷ ModelUpdateOption → Boolean
shouldTriggerEval = case _ of
  TriggerEval → true
  _ → false
