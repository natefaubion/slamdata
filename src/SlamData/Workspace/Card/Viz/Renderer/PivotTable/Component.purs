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

module SlamData.Workspace.Card.Viz.Renderer.PivotTable.Component
  ( component
  , module Q
  ) where

import SlamData.Prelude

import Data.Int as Int
import DOM.Event.Event (preventDefault)
import Global (readFloat)
import Halogen as H
import Halogen.HTML as HH
import SlamData.Monad (Slam)
import SlamData.Workspace.Card.Viz.Renderer.PivotTable.Component.Query as Q
import SlamData.Workspace.Card.Viz.Renderer.PivotTable.Component.Render as R
import SlamData.Workspace.Card.Viz.Renderer.PivotTable.Component.State as S

type DSL = H.ComponentDSL S.State Q.Query Q.Message Slam

component ∷ H.Component HH.HTML Q.Query Unit Q.Message Slam
component =
  H.component
    { initialState: const S.initialState
    , render: R.render
    , eval
    , receiver: const Nothing
    }

eval ∷ Q.Query ~> DSL
eval = case _ of
  Q.Update es next → do
    H.modify _
      { buckets = Just es.buckets
      , pageIndex = es.pageIndex
      , pageCount = es.pageCount
      , pageSize = es.pageSize
      , loading = false
      , port = Just es.options
      }
    pure next
  Q.StepPage step next → do
    st ← H.get
    let
      pageIndex = clamp 0 (st.pageCount - 1) case step of
        Q.First → 0
        Q.Prev  → st.pageIndex - 1
        Q.Next  → st.pageIndex + 1
        Q.Last  → st.pageCount - 1
    H.raise $ Q.StateUpdated _ { pageIndex = pageIndex }
    H.modify _ { loading = true }
    pure next
  Q.SetCustomPage page next → do
    H.modify _ { customPage = Just page }
    pure next
  Q.UpdatePage ev next → do
    H.liftEff $ preventDefault ev
    st ← H.get
    for_ st.customPage \page → do
      let
        pageIndex = clamp 0 (st.pageCount - 1) (Int.floor (readFloat page) - 1)
      H.raise $ Q.StateUpdated _ { pageIndex = pageIndex }
      H.modify _ { customPage = Nothing, loading = true }
    pure next
  Q.ChangePageSize size next → do
    st ← H.get
    let
      pageSize  = Int.floor (readFloat size)
    H.modify _ { pageSize = pageSize, loading = true }
    H.raise Q.ModelUpdated
    pure next
  Q.Load model next → do
    H.modify _ { pageSize = model.pageSize }
    pure next
  Q.Save k → do
    { pageSize } ← H.get
    pure $ k { pageSize }
