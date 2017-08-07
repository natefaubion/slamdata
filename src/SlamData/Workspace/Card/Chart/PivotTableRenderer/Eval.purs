{-
Copyright 2017 SlamData, Inc.

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

module SlamData.Workspace.Card.Chart.PivotTableRenderer.Eval where

import SlamData.Prelude

import Control.Monad.State (class MonadState, put, get)
import Data.Argonaut as J
import Data.Array as Array
import Data.Lens (preview, _Just)
import Data.List as List
import SlamData.Quasar.Class (class QuasarDSL)
import SlamData.Workspace.Card.Chart.Error (ChartError(..), throwChartError)
import SlamData.Workspace.Card.Chart.PivotTableRenderer.Common (PTree(..), topField, buildTree, pagedTree, sizeOfRow, calcPageCount)
import SlamData.Workspace.Card.Chart.PivotTableRenderer.Model as M
import SlamData.Workspace.Card.Eval.Common as CEC
import SlamData.Workspace.Card.Eval.Monad as CEM
import SlamData.Workspace.Card.Eval.State as ES
import SlamData.Workspace.Card.Port as Port

initialState ∷ Port.Resource → Int → Port.PivotTablePort → ES.PivotTableR
initialState resource pageSize options =
  { resource
  , result: []
  , buckets: Bucket []
  , pageIndex: 0
  , pageSize
  , pageCount: 0
  , count: 0
  , options
  }

eval
  ∷ ∀ m v
  . MonadState CEM.CardState m
  ⇒ MonadAsk CEM.CardEnv m
  ⇒ MonadThrow (Variant (chart ∷ ChartError | v)) m
  ⇒ QuasarDSL m
  ⇒ M.Model
  → Port.PivotTablePort
  → Port.VarMap
  → m Port.Out
eval model port varMap = do
  Port.extractResource varMap
    # maybe (throwChartError ChartMissingResourceInputError) \resource → do
      prevState ← preview (_Just ∘ ES._PivotTable) <$> get
      let
        state =
          case prevState of
            Just t → t
              { resource = resource
              , pageSize = model.pageSize
              , options  = port
              }
            Nothing →
              initialState resource model.pageSize port
      nextState ←
        if maybe true _.options.isSimpleQuery prevState && state.options.isSimpleQuery
          then runPagedQuery prevState state
          else runAggregatedQuery prevState state
      put $ Just $ ES.PivotTable nextState
      pure (Port.ResourceKey Port.defaultResourceVar × varMap)

runPagedQuery
  ∷ ∀ m v
  . MonadThrow (Variant (chart ∷ ChartError | v)) m
  ⇒ MonadAsk CEM.CardEnv m
  ⇒ QuasarDSL m
  ⇒ Maybe ES.PivotTableR
  → ES.PivotTableR
  → m ES.PivotTableR
runPagedQuery prev next = do
  t2 ← case prev of
    Just t1 | t1.resource ≡ next.resource && t1.pageSize ≡ next.pageSize →
      pure next
    _ → do
      count ← runCount next.resource
      let
        pageCount = calcPageCount count next.pageSize
        pageIndex = clamp 0 (pageCount - 1) next.pageIndex
      pure next
        { pageIndex = pageIndex
        , pageCount = pageCount
        , count = count
        }
  result ← runQuery next.resource t2.pageSize t2.pageIndex
  pure t2
    { result = result
    , buckets = buildTree mempty Bucket Grouped result
    }

runAggregatedQuery
  ∷ ∀ m v
  . MonadThrow (Variant (chart ∷ ChartError | v)) m
  ⇒ MonadAsk CEM.CardEnv m
  ⇒ QuasarDSL m
  ⇒ Maybe ES.PivotTableR
  → ES.PivotTableR
  → m ES.PivotTableR
runAggregatedQuery = case _, _ of
  Just t1, next | t1.resource ≡ next.resource → do
    pure $ pageTree next
  _, next → do
    result ← runAll next.resource
    pure (pageTree next { result = result })
  where
  pageTree ∷ ES.PivotTableR → ES.PivotTableR
  pageTree next =
    let
      dims = List.fromFoldable $ J.cursorGet ∘ topField ∘ fst <$> next.options.dimensions
      tree = buildTree dims Bucket Grouped next.result
      count × pages = pagedTree next.pageSize (sizeOfRow next.options.columns) tree
      pageCount = Array.length pages
      pageIndex = clamp 0 (pageCount - 1) next.pageIndex
    in next
      { buckets = fromMaybe next.buckets (Array.index pages pageIndex)
      , count = count
      , pageCount = pageCount
      , pageIndex = pageIndex
      }

runCount
  ∷ ∀ m v
  . MonadThrow (Variant (chart ∷ ChartError | v)) m
  ⇒ MonadAsk CEM.CardEnv m
  ⇒ QuasarDSL m
  ⇒ Port.Resource
  → m Int
runCount resource = do
  CEC.countResource resource >>= case _ of
    Left err → throwChartError (ChartCountQuasarError err)
    Right result → pure result

runQuery
  ∷ ∀ m v
  . MonadThrow (Variant (chart ∷ ChartError | v)) m
  ⇒ MonadAsk CEM.CardEnv m
  ⇒ QuasarDSL m
  ⇒ Port.Resource
  → Int
  → Int
  → m (Array J.Json)
runQuery resource limit offset = do
  CEM.CardEnv { path } ← ask
  CEC.sampleResource path resource (Just { limit, offset }) >>= case _ of
    Left err → throwChartError (ChartSampleQuasarError err)
    Right result → pure result

runAll
  ∷ ∀ m v
  . MonadThrow (Variant (chart ∷ ChartError | v)) m
  ⇒ MonadAsk CEM.CardEnv m
  ⇒ QuasarDSL m
  ⇒ Port.Resource
  → m (Array J.Json)
runAll resource = do
  CEM.CardEnv { path } ← ask
  CEC.sampleResource path resource Nothing >>= case _ of
    Left err → throwChartError (ChartSampleQuasarError err)
    Right result → pure result
