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

module SlamData.Workspace.Card.Eval.Monad
  ( CardEval
  , CardLog
  , CardState
  , CardEnv(..)
  , CardResult
  , CardEvalM
  , ChildOut
  , addSource
  , addCache
  , addSources
  , addCaches
  , additionalSources
  , temporaryOutputResource
  , temporaryOutputModule
  , anyTemporaryPath
  , localUrlVarMap
  , localVarMap
  , localCardId
  , resourceOut
  , extractResourcePair
  , extractResourceVar
  , extractResource
  , runCardEvalM
  , module SlamData.Workspace.Card.Eval.State
  , module SlamData.Workspace.Deck.AdditionalSource
  ) where

import SlamData.Prelude

import Control.Applicative.Free (FreeAp, liftFreeAp, foldFreeAp)
import Control.Monad.Aff (Aff)
import Control.Monad.Aff.Class (class MonadAff, liftAff)
import Control.Monad.Eff.Class (class MonadEff, liftEff)
import Control.Monad.Free (Free, liftF, resume)
import Control.Monad.State.Class (class MonadState)
import Control.Monad.Writer.Class (class MonadTell, tell)
import Control.Parallel.Class (parallel, sequential)
import Data.List ((:))
import Data.List as L
import Data.Map as Map
import Data.Path.Pathy ((</>))
import Data.Path.Pathy as Path
import Data.Set (Set)
import Data.Set as Set
import Data.StrMap as SM
import Quasar.Advanced.QuasarAF as QA
import SlamData.Effects (SlamDataEffects)
import SlamData.Quasar.Class (class QuasarDSL, class ParQuasarDSL, liftQuasar)
import SlamData.Workspace.Card.CardId as CID
import SlamData.Workspace.Card.Error as CE
import SlamData.Workspace.Card.Eval.State (EvalState(..))
import SlamData.Workspace.Card.Port as Port
import SlamData.Workspace.Card.Port.VarMap as VM
import SlamData.Workspace.Deck.AdditionalSource (AdditionalSource(..))
import Utils.Path (DirPath, RelDirPath, AnyFilePath, FilePath, RelFilePath, tmpDir)

type CardEval = CardEvalM SlamDataEffects

type CardLog = Set AdditionalSource

type CardState = Maybe EvalState

type CardResult err a =
  { output ∷ Either err a
  , sources ∷ Set.Set AdditionalSource
  , state ∷ CardState
  }

type ChildOut =
  { namespace ∷ String
  , varMap ∷ Port.VarMap
  }

newtype CardEnv = CardEnv
  { path ∷ DirPath
  , cardId ∷ CID.CardId
  , varMap ∷ Port.VarMap
  , urlVarMaps ∷ Map.Map CID.CardId Port.URLVarMap
  , children ∷ L.List ChildOut
  , readOnly ∷ Boolean
  }

data CardEvalF eff err a
  = Aff (Aff eff a)
  | Quasar (QA.QuasarAFC a)
  | ParQuasar (FreeAp QA.QuasarAFC a)
  | Tell (a × Set AdditionalSource)
  | State (CardState → a × CardState)
  | Ask (CardEnv → a)
  | Throw err

instance functorCardEvalF ∷ Functor (CardEvalF eff err) where
  map f = case _ of
    Aff aff     → Aff (f <$> aff)
    Quasar q    → Quasar (f <$> q)
    ParQuasar a → ParQuasar (f <$> a)
    Tell a      → Tell (lmap f a)
    State a     → State (lmap f <$> a)
    Ask a       → Ask (f <$> a)
    Throw err   → Throw err

newtype CardEvalM eff err a = CardEvalM (Free (CardEvalF eff err) a)

unCardEvalM ∷ ∀ eff err. CardEvalM eff err ~> Free (CardEvalF eff err)
unCardEvalM (CardEvalM a) = a

derive newtype instance functorCardEvalM ∷ Functor (CardEvalM eff err)
derive newtype instance applyCardEvalM ∷ Apply (CardEvalM eff err)
derive newtype instance applicativeCardEvalM ∷ Applicative (CardEvalM eff err)
derive newtype instance bindCardEvalM ∷ Bind (CardEvalM eff err)
derive newtype instance monadCardEvalM ∷ Monad (CardEvalM eff err)

instance monadThrowCardEvalM ∷ MonadThrow err (CardEvalM eff err) where
  throwError = CardEvalM ∘ liftF ∘ Throw

instance monadStateCardEvalM ∷ MonadState (Maybe EvalState) (CardEvalM eff err) where
  state = CardEvalM ∘ liftF ∘ State

instance monadAskCardEvalM ∷ MonadAsk CardEnv (CardEvalM eff err) where
  ask = CardEvalM (liftF (Ask id))

instance monadTellCardEvalM ∷ MonadTell (Set AdditionalSource) (CardEvalM eff err) where
  tell as = CardEvalM (liftF (Tell (unit × as)))

instance monadEffCardEvalM ∷ MonadEff eff (CardEvalM eff err) where
  liftEff = CardEvalM ∘ liftF ∘ Aff ∘ liftEff

instance monadAffCardEvalM ∷ MonadAff eff (CardEvalM eff err) where
  liftAff = CardEvalM ∘ liftF ∘ Aff

instance quasarDSLCardEvalM ∷ QuasarDSL (CardEvalM eff err) where
  liftQuasar = CardEvalM ∘ liftF ∘ Quasar

instance parQuasarDSLCardEvalM ∷ ParQuasarDSL (CardEvalM eff err) where
  sequenceQuasar = CardEvalM ∘ liftF ∘ ParQuasar ∘ traverse liftFreeAp

addSource ∷ ∀ m. (MonadTell (Set AdditionalSource) m) ⇒ FilePath → m Unit
addSource fp = tell (Set.singleton (Source fp))

addCache ∷ ∀ m. (MonadTell (Set AdditionalSource) m) ⇒ FilePath → m Unit
addCache fp = tell (Set.singleton (Cache fp))

addSources ∷ ∀ f m. Foldable f ⇒ MonadTell (Set AdditionalSource) m ⇒ f FilePath → m Unit
addSources fps = tell (foldMap (Set.singleton ∘ Source) fps)

addCaches ∷ ∀ f m. Foldable f ⇒ MonadTell (Set AdditionalSource) m ⇒ f FilePath → m Unit
addCaches fps = tell (foldMap (Set.singleton ∘ Cache) fps)

additionalSources ∷ ∀ f m. Foldable f ⇒ MonadTell (Set AdditionalSource) m ⇒ f AdditionalSource → m Unit
additionalSources = tell ∘ foldMap Set.singleton

temporaryOutputResource ∷ ∀ m. MonadAsk CardEnv m ⇒ m (FilePath × RelFilePath)
temporaryOutputResource = do
  CardEnv { cardId, path } ← ask
  let res = Path.file ("out" ⊕ CID.toString cardId)
  pure $ path </> tmpDir </> res × Path.currentDir </> res

temporaryOutputModule ∷ ∀ m. MonadAsk CardEnv m ⇒ m (DirPath × RelDirPath)
temporaryOutputModule = do
  CardEnv { cardId, path } ← ask
  let res = Path.dir ("out" ⊕ CID.toString cardId)
  pure $ path </> tmpDir </> res × Path.currentDir </> res

anyTemporaryPath ∷ ∀ m. MonadAsk CardEnv m ⇒ AnyFilePath → m FilePath
anyTemporaryPath = case _ of
  Left p → pure p
  Right r → do
    CardEnv { path } ← ask
    pure $ path </> tmpDir </> r

localUrlVarMap ∷ ∀ m. MonadAsk CardEnv m ⇒ m Port.URLVarMap
localUrlVarMap = do
  CardEnv { cardId, urlVarMaps } ← ask
  pure
    (fromMaybe SM.empty
      (Map.lookup cardId urlVarMaps))

localVarMap ∷ ∀ m. MonadAsk CardEnv m ⇒ m Port.VarMap
localVarMap = do
  CardEnv { varMap } ← ask
  pure varMap

localCardId ∷ ∀ m. MonadAsk CardEnv m ⇒ m CID.CardId
localCardId = do
  CardEnv { cardId } ← ask
  pure cardId

resourceOut ∷ ∀ m. MonadAsk CardEnv m ⇒ Port.Resource → m Port.Out
resourceOut res = do
  CardEnv { cardId, varMap } ← ask
  pure (Port.resourceOut cardId res varMap)

extractResourcePair
  ∷ ∀ m v
  . MonadThrow (Variant (resource ∷ CE.ResourceError | v)) m
  ⇒ MonadAsk CardEnv m
  ⇒ Port.Port
  → m (Port.Var × Port.Resource)
extractResourcePair port = do
  varMap ← localVarMap
  case port of
    Port.ResourceKey key
      | Just (Port.Resource r) ← VM.lookup (Port.Var key) varMap
      → pure (Port.Var key × r)
    _ →
      case Port.filterResources varMap of
        rs@(_ : _ : _) → CE.throwResourceError $ CE.ResourceError (snd <$> rs)
        L.Nil → CE.throwResourceError $ CE.ResourceError L.Nil
        r : L.Nil → pure r

extractResource
  ∷ ∀ m v
  . MonadThrow (Variant (resource ∷ CE.ResourceError | v)) m
  ⇒ MonadAsk CardEnv m
  ⇒ Port.Port
  → m Port.Resource
extractResource = map snd ∘ extractResourcePair

extractResourceVar
  ∷ ∀ m v
  . MonadThrow (Variant (resource ∷ CE.ResourceError | v)) m
  ⇒ MonadAsk CardEnv m
  ⇒ Port.Port
  → m Port.Var
extractResourceVar = map fst ∘ extractResourcePair

runCardEvalM
  ∷ ∀ eff err f m a
  . QuasarDSL m
  ⇒ MonadAff eff m
  ⇒ Parallel f m
  ⇒ Monad m
  ⇒ CardEnv
  → CardState
  → CardEvalM eff err a
  → m (CardResult err a)
runCardEvalM env initialState (CardEvalM ce) = go initialState Set.empty ce
  where
    go st as ce' = case resume ce' of
      Left ctr →
        case ctr of
          Aff aff → liftAff aff >>= go st as
          Quasar q → liftQuasar q >>= go st as
          ParQuasar q → sequential (foldFreeAp (parallel ∘ liftQuasar) q) >>= go st as
          Tell (n × as') → go st (as <> as') n
          State k → let res = k st in go (snd res) as (fst res)
          Ask k → go st as (k env)
          Throw err → pure { output: Left err, sources: as, state: st }
      Right a →
        pure { output: Right a, sources: as, state: st }
