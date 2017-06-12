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

module SlamData.Workspace.Eval.Card
  ( EvalMessage(..)
  , Model
  , Cell
  , State
  , Id
  , Transition
  , DisplayCoord
  , toAll
  , module SlamData.Workspace.Card.CardId
  , module SlamData.Workspace.Card.Common.EvalQuery
  , module SlamData.Workspace.Card.Eval
  , module SlamData.Workspace.Card.Eval.Monad
  , module SlamData.Workspace.Card.Eval.State
  , module SlamData.Workspace.Card.Model
  , module SlamData.Workspace.Card.Port
  ) where

import SlamData.Prelude

import Control.Monad.Aff.Bus (BusRW)

import Data.List (List)
import Data.Set (Set)

import SlamData.Workspace.Card.CardId (CardId)
import SlamData.Workspace.Card.Common.EvalQuery (ModelUpdateOption(..), shouldTriggerEval)
import SlamData.Workspace.Card.Eval (Eval, runCard, modelToEval)
import SlamData.Workspace.Card.Eval.Monad (CardEnv(..), ChildOut, AdditionalSource(..))
import SlamData.Workspace.Card.Eval.State (initialEvalState, EvalState)
import SlamData.Workspace.Card.Model (AnyCardModel, modelCardType, cardModelOfType, childDeckIds)
import SlamData.Workspace.Card.Port (Port(..), Out, emptyOut, resourceOut)
import SlamData.Workspace.Eval.Deck as Deck

type State = EvalState

type Model = AnyCardModel

data EvalMessage
  = Pending DisplayCoord Out
  | Complete DisplayCoord Out
  | ModelChange DisplayCoord AnyCardModel
  | StateChange DisplayCoord State

type Cell =
  { bus ∷ BusRW EvalMessage
  , next ∷ Set (Either Deck.Id Id)
  , decks ∷ Set Deck.Id
  , model ∷ AnyCardModel
  , input ∷ Maybe Out
  , pending ∷ Maybe Out
  , output ∷ Maybe Out
  , state ∷ Maybe State
  , sources ∷ Set AdditionalSource
  , tick ∷ Maybe Int
  }

type Id = CardId

type Transition = Eval

type DisplayCoord = List Deck.Id × Id

toAll ∷ CardId → DisplayCoord
toAll = Tuple mempty
