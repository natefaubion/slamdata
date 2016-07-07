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

module SlamData.Workspace.Card.Next.Component.Query where

import SlamData.Prelude
import Data.Lens (TraversalP, wander)
import SlamData.Workspace.Card.Common.EvalQuery (CardEvalQuery)
import SlamData.Workspace.Card.CardType (CardType)
import SlamData.Workspace.Card.Port (Port)

data Query a
  = AddCard CardType a
  | PresentReason (Maybe Port) CardType a
  | UpdateFilter String a

_AddCardType :: forall a. TraversalP (Query a) CardType
_AddCardType =
  wander \f s → case s of
    AddCard cty next → flip AddCard next <$> f cty
    PresentReason io card next → pure s
    UpdateFilter filter next → pure s

_PresentReason :: forall a. TraversalP (Query a) (Tuple (Maybe Port) CardType)
_PresentReason =
  wander \f s → case s of
    AddCard cty next → pure s
    PresentReason io card next → (#) next <<< uncurry PresentReason <$> f (Tuple io card)
    UpdateFilter filter next → pure s

type QueryP = CardEvalQuery ⨁ Query
