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

module SlamData.Workspace.Card.Setups.Common.Eval
  ( analysisEval
  , ChartSetupEval
  , chartSetupEval
  , analyze
  , type (>>)
  , assoc
  , deref
  , groupOn
  ) where

import SlamData.Prelude

import Control.Monad.Aff.Class (class MonadAff)
import Control.Monad.State (class MonadState, get, put)
import Control.Monad.Writer.Class (class MonadTell)
import Data.Argonaut as J
import Data.Array as A
import Data.Foreign (Foreign, toForeign)
import Data.Foreign.Index (readProp)
import Data.Function (on)
import Data.Map as M
import Data.NonEmpty as NE
import Data.StrMap as SM
import ECharts.Monad (DSL)
import ECharts.Monad as EM
import ECharts.Types as ET
import ECharts.Types.Phantom (I)
import SlamData.Effects (SlamDataEffects)
import SlamData.Quasar.Class (class ParQuasarDSL, class QuasarDSL)
import SlamData.Workspace.Card.Error as CE
import SlamData.Workspace.Card.Eval.Common as CEC
import SlamData.Workspace.Card.Eval.Monad as CEM
import SlamData.Workspace.Card.Port as Port
import SlamData.Workspace.Card.Port.VarMap as VM
import SlamData.Workspace.Card.Setups.Axis (Axes, buildAxes)
import SlamData.Workspace.Card.Setups.Common.Sql (numFuncs)
import SqlSquared as Sql
import Utils (hush')

infixr 3 type M.Map as >>

analysisEval
  ∷ ∀ m v p
  . MonadState CEM.CardState m
  ⇒ MonadThrow (Variant (stringly ∷ String, qerror ∷ CE.QError | v)) m
  ⇒ MonadAsk CEM.CardEnv m
  ⇒ QuasarDSL m
  ⇒ (Axes → p → Array J.Json → Port.Port)
  → Maybe p
  → (Axes → Maybe p)
  → Port.Resource
  → m Port.Port
analysisEval build model defaultModel resource = do
  records × axes ← analyze resource =<< get
  put (Just (CEM.Analysis { resource, records, axes }))
  case model <|> defaultModel axes of
    Just ch → pure $ build axes ch records
    Nothing → CE.throw "Please select an axis."

type ChartSetupEval p m v =
  MonadState CEM.CardState m
  ⇒ MonadThrow (Variant (stringly ∷ String, qerror ∷ CE.QError, resource ∷ CE.ResourceError | v)) m
  ⇒ MonadAsk CEM.CardEnv m
  ⇒ MonadTell CEM.CardLog m
  ⇒ MonadAff SlamDataEffects m
  ⇒ ParQuasarDSL m
  ⇒ Maybe p
  → Port.Port
  → m Port.Out

chartSetupEval
  ∷ ∀ m v p
  . MonadState CEM.CardState m
  ⇒ MonadThrow (Variant (stringly ∷ String, qerror ∷ CE.QError, resource ∷ CE.ResourceError | v)) m
  ⇒ MonadAsk CEM.CardEnv m
  ⇒ MonadTell CEM.CardLog m
  ⇒ MonadAff SlamDataEffects m
  ⇒ ParQuasarDSL m
  ⇒ (p → VM.Var → Sql.Sql)
  → (p → Axes → Port.Port)
  → Maybe p
  → Port.Port
  → m Port.Out
chartSetupEval buildSql buildPort m port = do
  var × resource ← CEM.extractResourcePair port
  records × axes ← analyze resource =<< get
  put $ Just $ CEM.Analysis { resource, records, axes }
  case m of
    Nothing → CE.throw "Incorrect chart setup model"
    Just r → do
      CEM.CardEnv { varMap, cardId } ← ask
      let
        sql = buildSql r var
        port' = buildPort r axes
      resource' ← CE.liftQ $ CEC.localEvalResource (Sql.Query numFuncs sql) varMap
      pure (port' × VM.insert cardId (VM.Var Port.defaultResourceVar) (VM.Resource resource') varMap)

analyze
  ∷ ∀ m v
  . MonadThrow (Variant (qerror ∷ CE.QError | v)) m
  ⇒ MonadAsk CEM.CardEnv m
  ⇒ QuasarDSL m
  ⇒ Port.Resource
  → CEM.CardState
  → m (Array J.Json × Axes)
analyze resource = case _ of
  Just (CEM.Analysis st) | resource ≡ st.resource →
    pure (st.records × st.axes)
  _ → do
    CEM.CardEnv { path } ← ask
    records ← either (const []) id <$> CEC.sampleResource path resource (Just { offset: 0, limit: 300 })
    let axes = buildAxes (unwrapValue <$> records)
    pure (records × axes)
  where
  unwrapValue json =
    json # J.foldJsonObject json \obj →
      case SM.keys obj, SM.lookup "value" obj of
        ["value"], Just value → value
        _, _ → json

assoc ∷ ∀ a i. a → DSL (value ∷ I | i)
assoc = EM.set "$$assoc" <<< toForeign

deref ∷ ET.Item → Maybe Foreign
deref (ET.Item item) = hush' $ readProp "$$assoc" item

groupOn ∷ ∀ a b. Eq b ⇒ (a → b) → Array a → Array (b × Array a)
groupOn f = A.groupBy (eq `on` f) >>> map \as → f (NE.head as) × NE.fromNonEmpty A.cons as
