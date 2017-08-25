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

module SlamData.Workspace.Card.Table.Component
  ( tableComponent
  , module SlamData.Workspace.Card.Table.Component.Query
  , module JTS
  ) where

import SlamData.Prelude

import Data.Argonaut as JSON
import Data.Int as Int
import Data.Lens ((.~), (^?))

import DOM.Classy.Event as DOM

import Halogen as H

import SlamData.Workspace.Card.CardType as CT
import SlamData.Workspace.Card.Component as CC
import SlamData.Workspace.Card.Eval.State as ES
import SlamData.Workspace.Card.Model as Card
import SlamData.Workspace.Card.Table.Component.Query (PageStep(..), Query(..))
import SlamData.Workspace.Card.Table.Component.Render (render)
import SlamData.Workspace.Card.Table.Component.State as JTS
import SlamData.Workspace.LevelOfDetails (LevelOfDetails(..))

type DSL = CC.InnerCardDSL JTS.State Query

tableComponent ∷ CC.CardOptions → CC.CardComponent
tableComponent =
  CC.makeCardComponent CT.table $ H.component
    { render
    , eval: evalCard ⨁ evalTable
    , initialState: const JTS.initialState
    , receiver: const Nothing
    }

-- | Evaluates generic card queries.
evalCard ∷ CC.CardEvalQuery ~> DSL
evalCard = case _ of
  CC.Activate next →
    pure next
  CC.Deactivate next →
    pure next
  CC.Save k →
    pure ∘ k =<< H.gets (Card.Table ∘ JTS.toModel)
  CC.Load card next → do
    case card of
      Card.Table model → H.put $ JTS.fromModel model
      _ → pure unit
    pure next
  CC.ReceiveInput _ _ next → do
    pure next
  CC.ReceiveOutput _ _ next →
    pure next
  CC.ReceiveState s next → do
    for_ (s ^? ES._Table) \t →
      H.modify
        if t.size == 0
          then _{ result = Nothing }
          else
            _{ result = Just
                 { json: JSON.fromArray t.result
                 , page: t.page
                 , pageSize: t.pageSize
                 }
             , size = Just t.size
             , isEnteringPageSize = false
             }
    pure next
  CC.ReceiveDimensions dims reply → do
    pure $ reply
      if dims.width < 360.0 ∨ dims.height < 240.0
      then Low
      else High

-- | Evaluates table-specific card queries.
evalTable ∷ Query ~> DSL
evalTable = case _ of
  StepPage step next → do
    H.modify (JTS.stepPage step)
    refresh
    pure next
  ChangePageSize pageSize next → do
    for_ (Int.fromString pageSize) (H.modify ∘ JTS.resizePage)
    refresh
    pure next
  StartEnterCustomPageSize next → do
    H.modify (JTS._isEnteringPageSize .~ true)
    pure next
  SetCustomPageSize size next → do
    H.modify (JTS.setPageSize size)
    refresh
    pure next
  SetCustomPage page next → do
    H.modify (JTS.setPage page)
    refresh
    pure next
  PreventDefault ev next → do
    H.liftEff $ DOM.preventDefault ev
    pure next

refresh ∷ DSL Unit
refresh =
  H.raise CC.modelUpdate
