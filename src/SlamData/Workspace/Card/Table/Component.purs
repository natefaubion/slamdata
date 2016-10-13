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

import Control.Monad.Except.Trans as ET

import Data.Argonaut.Core as JSON
import Data.Int as Int
import Data.Lens ((.~), (?~))

import Halogen as H

import SlamData.Monad (Slam)
import SlamData.Quasar.Error as QE
import SlamData.Quasar.Query as Quasar
import SlamData.Workspace.Card.CardType as CT
import SlamData.Workspace.Card.Component as CC
import SlamData.Workspace.Card.Model as Card
import SlamData.Workspace.Card.Port as Port
import SlamData.Workspace.Card.Table.Component.Query (QueryP, PageStep(..), Query(..))
import SlamData.Workspace.Card.Table.Component.Render (render)
import SlamData.Workspace.Card.Table.Component.State as JTS
import SlamData.Workspace.LevelOfDetails (LevelOfDetails(..))

type DSL = H.ComponentDSL JTS.State QueryP Slam

tableComponent ∷ H.Component CC.CardStateP CC.CardQueryP Slam
tableComponent =
  CC.makeCardComponent
    { cardType: CT.Table
    , component: H.component { render, eval: evalCard ⨁ evalTable }
    , initialState: JTS.initialState
    , _State: CC._TableState
    , _Query: CC.makeQueryPrism CC._TableQuery
    }

-- | Evaluates generic card queries.
evalCard ∷ CC.CardEvalQuery ~> DSL
evalCard = case _ of
  CC.EvalCard info output next → do
    for_ info.input $ void ∘ ET.runExceptT ∘ runTable
    pure next
  CC.Activate next →
    pure next
  CC.Deactivate next →
    pure next
  CC.Save k →
    pure ∘ k =<< H.gets (Card.Table ∘ JTS.toModel)
  CC.Load card next → do
    case card of
      Card.Table model → H.set $ JTS.fromModel model
      _ → pure unit
    pure next
  CC.SetDimensions dims next → do
    H.modify
      $ JTS._levelOfDetails
      .~ if dims.width < 336.0 ∨ dims.height < 240.0
           then Low
           else High
    pure next
  CC.ModelUpdated _ next →
    pure next
  CC.ZoomIn next →
    pure next

runTable
  ∷ Port.Port
  → ET.ExceptT QE.QError (H.ComponentDSL JTS.State QueryP Slam) Unit
runTable = case _ of
  Port.TaggedResource trp → updateTable trp
  _ → QE.throw "Expected a TaggedResource input"

updateTable
  ∷ Port.TaggedResourcePort
  → ET.ExceptT QE.QError (H.ComponentDSL JTS.State QueryP Slam) Unit
updateTable { resource, tag, varMap } = do
  oldInput ← lift $ H.gets _.input
  when (((oldInput <#> _.resource) ≠ pure resource) || ((oldInput >>= _.tag) ≠ tag))
    $ lift $ resetState

  size ←  ET.ExceptT $ Quasar.count resource

  lift $ H.modify $ JTS._input ?~ { resource, size, tag, varMap }
  p ← lift $ H.gets JTS.pendingPageInfo

  items ← ET.ExceptT $
    Quasar.sample resource ((p.page - 1) * p.pageSize) p.pageSize

  lift
    $ H.modify
    $ (JTS._isEnteringPageSize .~ false)
    ∘ (JTS._result ?~
         { json: JSON.fromArray items
         , page: p.page
         , pageSize: p.pageSize
         })

-- | Resets the state while preserving settings like page size.
resetState ∷ DSL Unit
resetState = H.modify (JTS._result .~ Nothing)

-- | Evaluates table-specific card queries.
evalTable ∷ Query ~> (H.ComponentDSL JTS.State QueryP Slam)
evalTable = case _ of
  StepPage step next → do
    H.modify (JTS.stepPage step)
    refresh
    pure next
  ChangePageSize pageSize next → do
    for_ (Int.fromString pageSize) (H.modify ∘ JTS.resizePage)
    refresh
    pure next
  StartEnterCustomPageSize next →
    H.modify (JTS._isEnteringPageSize .~ true) $> next
  SetCustomPageSize size next →
    H.modify (JTS.setPageSize size) $> next
  SetCustomPage page next →
    H.modify (JTS.setPage page) $> next
  Update next →
    refresh $> next

refresh ∷ DSL Unit
refresh = do
  input ← H.gets _.input
  for_ input \ {resource, tag, varMap} →
    void $ ET.runExceptT $ updateTable {resource, tag, varMap}
  CC.raiseUpdatedC' CC.StateOnlyUpdate
