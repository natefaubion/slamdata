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

module SlamData.Monad where

import SlamData.Prelude

import Control.Applicative.Free (FreeAp, hoistFreeAp, liftFreeAp, retractFreeAp)
import Control.Monad.Aff (Aff)
import Control.Monad.Aff.AVar as AVar
import Control.Monad.Aff.AVar (AVAR)
import Control.Monad.Aff.Bus as Bus
import Control.Monad.Aff.Class (class MonadAff, liftAff)
import Control.Monad.Aff.Unsafe (unsafeCoerceAff)
import Control.Monad.Eff.Class (class MonadEff, liftEff)
import Control.Monad.Eff.Exception (Error)
import Control.Monad.Fork (class MonadFork, fork)
import Control.Monad.Free (Free, liftF, foldFree)
import Control.Monad.Rec.Class (tailRecM, Step(..))
import Control.Parallel (parallel, sequential)
import Control.UI.Browser (locationObject, newTab)

import DOM (DOM)
import DOM.HTML.Location (setHash)

import Data.Array as Array
import Data.Exists as Exists
import Data.Path.Pathy ((</>))
import Data.Path.Pathy as P

import OIDC.Crypt.Types as OIDC

import Quasar.Advanced.QuasarAF as QA
import Quasar.Advanced.Types as QAT

import SlamData.Workspace.AccessType as AT
import SlamData.Effects (SlamDataEffects)
import SlamData.GlobalError as GE
import SlamData.License as License
import SlamData.LocalStorage as LS
import SlamData.LocalStorage.Class (class LocalStorageDSL)
import SlamData.Monad.Auth (getIdTokenSilently)
import SlamData.Monad.ForkF as FF
import SlamData.Notification as N
import SlamData.Quasar.Aff (runQuasarF)
import SlamData.Quasar.Auth (class QuasarAuthDSL)
import SlamData.Quasar.Auth.Authentication as Auth
import SlamData.Quasar.Class (class QuasarDSL, liftQuasar)
import SlamData.Wiring (Wiring)
import SlamData.Wiring as Wiring
import SlamData.Workspace.Class (class WorkspaceDSL)
import SlamData.Workspace.Deck.DeckId as DeckId
import SlamData.Workspace.Routing as Routing

type Slam = SlamM SlamDataEffects

data SlamF eff a
  = Aff (Aff eff a)
  | GetAuthIdToken (Maybe OIDC.IdToken → a)
  | Quasar (QA.QuasarAFC a)
  | LocalStorage (Exists.Exists (LS.LocalStorageF a))
  | Notify N.NotificationOptions a
  | Halt GE.GlobalError a
  | Par (SlamA eff a)
  | Fork (FF.Fork (SlamM eff) a)
  | Ask (Wiring → a)
  | Navigate Routing.Routes a

newtype SlamM eff a = SlamM (Free (SlamF eff) a)

unSlamM ∷ ∀ eff. SlamM eff ~> Free (SlamF eff)
unSlamM (SlamM a) = a

derive newtype instance functorSlamM ∷ Functor (SlamM eff)
derive newtype instance applySlamM ∷ Apply (SlamM eff)
derive newtype instance applicativeSlamM ∷ Applicative (SlamM eff)
derive newtype instance bindSlamM ∷ Bind (SlamM eff)
derive newtype instance monadSlamM ∷ Monad (SlamM eff)

instance monadEffSlamM ∷ MonadEff eff (SlamM eff) where
  liftEff = SlamM ∘ liftF ∘ Aff ∘ liftEff

instance monadAffSlamM ∷ MonadAff eff (SlamM eff) where
  liftAff = SlamM ∘ liftF ∘ Aff

instance monadParSlamM ∷ Parallel (SlamA eff) (SlamM eff) where
  parallel = SlamA ∘ liftFreeAp
  sequential = SlamM ∘ liftF ∘ Par

instance monadForkSlamM ∷ MonadFork Error (SlamM eff) where
  fork a = map liftAff <$> SlamM (liftF $ Fork $ FF.fork a)

instance monadAskSlamM ∷ MonadAsk Wiring (SlamM eff) where
  ask = SlamM $ liftF $ Ask id

instance monadRecSlamM ∷ MonadRec (SlamM eff) where
  tailRecM k a = k a >>= case _ of
    Loop b → tailRecM k b
    Done r → pure r

instance monadThrow ∷ MonadThrow Error (SlamM eff) where
  throwError = liftAff ∘ throwError

instance quasarAuthDSLSlamM ∷ QuasarAuthDSL (SlamM eff) where
  getIdToken = SlamM $ liftF $ GetAuthIdToken id

instance quasarDSLSlamM ∷ QuasarDSL (SlamM eff) where
  liftQuasar = SlamM ∘ liftF ∘ Quasar

instance notifyDSLSlamM ∷ N.NotifyDSL (SlamM eff) where
  notify notification detail timeout actionOptions =
    SlamM $ liftF $ Notify { notification, detail, timeout, actionOptions } unit

instance globalErrorDSLSlamM ∷ GE.GlobalErrorDSL (SlamM eff) where
  raiseGlobalError = SlamM ∘ liftF ∘ flip Halt unit

instance workspaceDSLSlamM ∷ WorkspaceDSL (SlamM eff) where
  navigate = SlamM ∘ liftF ∘ flip Navigate unit

instance localStorageDSLSlamM :: LocalStorageDSL (SlamM eff) where
  persist encode key value =
    SlamM $ liftF $ LocalStorage $ Exists.mkExists $ LS.Persist encode key value unit
  retrieve decode key =
    SlamM $ liftF $ LocalStorage $ Exists.mkExists $ LS.Retrieve decode key id
  remove key =
    SlamM $ liftF $ LocalStorage $ Exists.mkExists $ LS.Remove key unit
  awaitChange decode key =
    SlamM $ liftF $ LocalStorage $ Exists.mkExists $ LS.AwaitChange decode key id

newtype SlamA eff a = SlamA (FreeAp (SlamM eff) a)

derive newtype instance functorSlamA ∷ Functor (SlamA eff)
derive newtype instance applySlamA ∷ Apply (SlamA eff)
derive newtype instance applicativeSlamA ∷ Applicative (SlamA eff)

--------------------------------------------------------------------------------

runSlam ∷ Wiring → Slam ~> Aff SlamDataEffects
runSlam wiring@(Wiring.Wiring { auth, bus }) = foldFree go ∘ unSlamM
  where

  go ∷ SlamF SlamDataEffects ~> Aff SlamDataEffects
  go = case _ of
    Aff aff →
      aff
    GetAuthIdToken k → do
      idToken ← getIdTokenSilentlyIfNoPermissionTokens
      case idToken of
        Just (Left error) →
          for_ (Auth.toNotificationOptions error) \opts →
            Bus.write opts bus.notify
        _ →
          pure unit
      pure $ k $ maybe Nothing hush idToken
    Quasar qf → do
      idToken ← getIdTokenSilentlyIfNoPermissionTokens
      case idToken of
        Just (Left error) →
          for_ (Auth.toNotificationOptions error) \opts →
            Bus.write opts bus.notify
        _ →
          pure unit
      runQuasarF (maybe Nothing hush idToken) qf
    LocalStorage lse →
      Exists.runExists LS.run lse
    Notify no a → do
      Bus.write no bus.notify
      pure a
    Halt err a → do
      case err of
        GE.PaymentRequired → do
          runQuasarF Nothing QA.licenseInfo >>= case _ of
            Right licenseInfo →
              case licenseInfo.status of
                QAT.LicenseExpired →
                  Bus.write (License.Expired licenseInfo.type) bus.licenseProblems
                _ → pure unit
            Left _ ->
              Bus.write License.Invalid bus.licenseProblems
        _ -> Bus.write (GE.toNotificationOptions err) bus.notify
      pure a
    Par (SlamA p) →
      sequential $ retractFreeAp $ hoistFreeAp (parallel <<< runSlam wiring) p
    Fork f →
      goFork f
    Ask k →
      pure (k wiring)
    Navigate (Routing.WorkspaceRoute path deckIds action varMaps) a → do
      let
        path' = foldr (\y x → x </> P.dir (DeckId.toString y)) path deckIds
        hash  = Routing.mkWorkspaceHash path' action varMaps
      liftEff $ locationObject >>= setHash hash
      pure a

  goFork ∷ FF.Fork Slam ~> Aff SlamDataEffects
  goFork = FF.unFork \(FF.ForkF fx k) →
    k ∘ map unsafeCoerceAff <$> fork (runSlam wiring fx)

  getIdTokenSilentlyIfNoPermissionTokens ∷ Aff SlamDataEffects (Maybe Auth.EIdToken)
  getIdTokenSilentlyIfNoPermissionTokens =
    if Array.null auth.permissionTokenHashes
      then hush <$> getIdTokenSilently auth.allowedModes auth.requestToken
      else pure Nothing

notifyDaysRemainingIfNeeded
  ∷ ∀ m eff
  . MonadAff (avar ∷ AVAR, dom ∷ DOM | eff) m
  ⇒ MonadAsk Wiring m
  ⇒ MonadFork Error m
  ⇒ QuasarDSL m
  ⇒ m Unit
notifyDaysRemainingIfNeeded =
  void $ fork do
    { accessType, bus } ← Wiring.expose
    daysRemaining ← map _.daysRemaining <$> liftQuasar QA.licenseInfo
    case daysRemaining, accessType of
      Right i, AT.Editable | i <= 30 && i > 0 →
        void $ liftAff do
          trigger ← AVar.makeVar
          Bus.write (N.daysRemainingNotification trigger i) bus.notify
          fork do
            AVar.takeVar trigger
            liftEff $ newTab "https://slamdata.com/contact-us/"
      _, _ →
        pure unit
