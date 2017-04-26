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

import Control.Monad.State (class MonadState, get, put)
import Control.Monad.Writer.Class (class MonadTell)
import Data.Argonaut as J
import Data.Array as A
import Data.Foreign (Foreign, toForeign)
import Data.Foreign.Index (readProp)
import Data.Function (on)
import Data.Lens ((^.))
import Data.Map as M
import Data.NonEmpty as NE
import Data.Path.Pathy as Path
import Data.StrMap as SM
import ECharts.Monad (DSL)
import ECharts.Monad as EM
import ECharts.Types as ET
import ECharts.Types.Phantom (I)
import SlamData.Quasar.Class (class QuasarDSL)
import SlamData.Quasar.Error as QE
import SlamData.Quasar.FS as QFS
import SlamData.Quasar.Query as QQ
import SlamData.Workspace.Card.Eval.Monad as CEM
import SlamData.Workspace.Card.Port as Port
import SlamData.Workspace.Card.Setups.Axis (Axes, buildAxes)
import SqlSquared as Sql
import Utils (hush')
import Utils.Path as PU

infixr 3 type M.Map as >>

analysisEval
  ∷ ∀ m p
  . MonadState CEM.CardState m
  ⇒ MonadThrow CEM.CardError m
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
    Nothing → CEM.throw "Please select an axis."

type ChartSetupEval p m =
  MonadState CEM.CardState m
  ⇒ MonadThrow CEM.CardError m
  ⇒ MonadAsk CEM.CardEnv m
  ⇒ MonadTell CEM.CardLog m
  ⇒ QuasarDSL m
  ⇒ Maybe p
  → Port.Resource
  → m Port.Out

chartSetupEval
  ∷ ∀ m p
  . MonadState CEM.CardState m
  ⇒ MonadThrow CEM.CardError m
  ⇒ MonadAsk CEM.CardEnv m
  ⇒ MonadTell CEM.CardLog m
  ⇒ QuasarDSL m
  ⇒ (p → PU.FilePath → Sql.Sql)
  → (p → Axes → Port.Port)
  → Maybe p
  → Port.Resource
  → m Port.Out
chartSetupEval buildSql buildPort m resource = do
  records × axes ← analyze resource =<< get
  put $ Just $ CEM.Analysis { resource, records, axes }
  case m of
    Nothing → CEM.throw "Incorrect chart setup model"
    Just r → do
      let
        path = resource ^. Port._filePath
        backendPath = fromMaybe Path.rootDir $ Path.parentDir path
        sql = buildSql r path

      outputResource ← CEM.temporaryOutputResource

      { inputs } ←
        CEM.liftQ $ lmap (QE.prefixMessage "Error compiling query") <$>
          QQ.compile backendPath sql SM.empty

      _ ← CEM.liftQ do
        _ ← QQ.viewQuery outputResource sql SM.empty
        QFS.messageIfFileNotFound
          outputResource
          "Error making search temporary resource"
      let
        view = Port.View outputResource (Sql.print sql) SM.empty
        port = buildPort r axes
      pure (port × SM.singleton Port.defaultResourceVar (Left view))

analyze
  ∷ ∀ m
  . MonadThrow CEM.CardError m
  ⇒ QuasarDSL m
  ⇒ Port.Resource
  → CEM.CardState
  → m (Array J.Json × Axes)
analyze resource = case _ of
  Just (CEM.Analysis st) | resource ≡ st.resource →
    pure (st.records × st.axes)
  _ → do
    records ← CEM.liftQ (QQ.sample (resource ^. Port._filePath) 0 300)
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
