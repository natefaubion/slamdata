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

module SlamData.Wiring
  ( ActiveState
  , AuthWiring
  , BusWiring
  , CacheWiring
  , DebounceEval
  , DebounceSave
  , DeckMessage(..)
  , EChartsWiring
  , EvalWiring
  , HintDismissalMessage(..)
  , Wiring(..)
  , WiringR
  , WorkspaceMessage(..)
  , expose
  , focusDeck
  , make
  , showDialog
  , switchDeckToFlip
  , switchDeckToFront
  , getTheme
  , setTheme
  , unWiring
  ) where

import SlamData.Prelude

import Control.Monad.Aff.AVar (AVar)
import Control.Monad.Aff.AVar as AVar
import Control.Monad.Aff.Bus as Bus
import Control.Monad.Aff.Class (class MonadAff, liftAff)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Ref (Ref, readRef, writeRef)
import Control.Monad.Eff.Ref as Ref
import DOM.BrowserFeatures.Detectors (detectBrowserFeatures)
import Data.BrowserFeatures (BrowserFeatures)
import Data.StrMap (StrMap)
import ECharts.Theme as ETheme
import Quasar.Advanced.Types (TokenHash)
import SlamData.AuthenticationMode (AllowedAuthenticationModes, allowedAuthenticationModesForAccessType)
import SlamData.Effects (SlamDataEffects)
import SlamData.GlobalError as GE
import SlamData.GlobalMenu.Bus (SignInBus)
import SlamData.License (LicenseProblem)
import SlamData.Notification as N
import SlamData.Quasar.Auth.Authentication as Auth
import SlamData.Theme.Theme as Theme
import SlamData.Wiring.Cache (Cache)
import SlamData.Wiring.Cache as Cache
import SlamData.Workspace.AccessType (AccessType)
import SlamData.Workspace.Card.Port.VarMap as Port
import SlamData.Workspace.Deck.DeckId (DeckId)
import SlamData.Workspace.Deck.Options (DeckOptions)
import SlamData.Workspace.Dialog.Types (Dialog)
import SlamData.Workspace.EChartThemeLoader as EChartThemeLoader
import SlamData.Workspace.Eval.Card as Card
import SlamData.Workspace.Eval.Deck as Deck
import SlamData.Workspace.Eval.Graph (EvalGraph)
import SlamData.Workspace.Guide (GuideType)
import Utils.Path (DirPath)

-- TODO: DeckFocused should use DeckOptions too. It's not totally trivial though,
-- as Defocus in the deck uses DeckFocused to focus the root of its cursor,
-- which we only have a DeckId for. -gb
data DeckMessage
  = DeckFocused DeckId
  | SwitchToFront DeckOptions
  | SwitchToFlip DeckOptions

data WorkspaceMessage
  = ShowDialog Dialog

data HintDismissalMessage
  = DeckFocusHintDismissed
  | DeckFrameFocusHintDismissed

type ActiveState =
  { cardIndex ∷ Int
  }

type EChartsWiring =
  { theme ∷ Maybe ETheme.Theme
  }

type DebounceEval =
  { source ∷ Card.DisplayCoord
  , graph ∷ EvalGraph
  , avar ∷ AVar Unit
  }

type DebounceSave =
  { avar ∷ AVar Unit
  }

type EvalWiring =
  { tick ∷ Ref Int
  , root ∷ AVar Deck.Id
  , cards ∷ Cache Card.Id Card.Cell
  , decks ∷ Cache Deck.Id Deck.Cell
  -- We need to use AVars for debounce state rather than storing Cancelers,
  -- because the Canceler would need to reference `Slam` resulting in a
  -- circular dependency.
  , debounceEvals ∷ Cache Card.Id DebounceEval
  , debounceSaves ∷ Cache DirPath DebounceSave
  }

type AuthWiring =
  { hasIdentified ∷ Ref Boolean
  , requestToken ∷ Auth.RequestIdTokenBus
  , signIn ∷ SignInBus
  , allowedModes ∷ AllowedAuthenticationModes
  , permissionTokenHashes ∷ Array TokenHash
  , retryEval ∷ Ref (Maybe Card.CardId)
  , retrySave ∷ Ref Boolean
  , retryCardUI ∷ Ref (Array Card.CardId)
  }

type CacheWiring =
  { activeState ∷ Cache Deck.Id ActiveState
  }

type BusWiring =
  { decks ∷ Bus.BusRW DeckMessage
  , workspace ∷ Bus.BusRW WorkspaceMessage
  , notify ∷ Bus.BusRW N.NotificationOptions
  , globalError ∷ Bus.BusRW GE.GlobalError
  , stepByStep ∷ Bus.BusRW GuideType
  , hintDismissals ∷ Bus.BusRW HintDismissalMessage
  , licenseProblems ∷ Bus.BusRW LicenseProblem
  , themeChange ∷ Bus.BusRW (Maybe Theme.Theme)
  }

type WiringR =
  { path ∷ DirPath
  , accessType ∷ AccessType
  , varMaps ∷ Ref (StrMap Port.URLVarMap)
  , eval ∷ EvalWiring
  , auth ∷ AuthWiring
  , cache ∷ CacheWiring
  , bus ∷ BusWiring
  , echarts ∷ EChartsWiring
  , browserFeatures ∷ BrowserFeatures
  , theme ∷ Ref (Maybe Theme.Theme)
  }

newtype Wiring = Wiring WiringR

unWiring ∷ Wiring → WiringR
unWiring (Wiring w) = w

expose
  ∷ ∀ m
  . MonadAsk Wiring m
  ⇒ m WiringR
expose = unWiring <$> ask

make
  ∷ ∀ m
  . MonadAff SlamDataEffects m
  ⇒ DirPath
  → AccessType
  → StrMap Port.URLVarMap
  → Array TokenHash
  → m Wiring
make path accessType vm permissionTokenHashes = liftAff do
  eval ← makeEval
  auth ← makeAuth
  cache ← makeCache
  bus ← makeBus
  echarts ← makeEcharts
  browserFeatures ← liftEff detectBrowserFeatures
  varMaps ← liftEff (Ref.newRef vm)
  license ← liftEff (Ref.newRef vm)
  theme ← liftEff (Ref.newRef Nothing)
  themeChange ← liftEff (Ref.newRef Nothing)
  pure $ Wiring { path, accessType, varMaps, eval, auth, cache, bus, echarts, browserFeatures, theme }

  where
  makeEcharts = do
    theme ← EChartThemeLoader.load
    pure { theme }
  makeEval = do
    tick ← liftEff $ Ref.newRef 0
    root ← AVar.makeVar
    cards ← Cache.make
    decks ← Cache.make
    debounceEvals ← Cache.make
    debounceSaves ← Cache.make
    pure
      { tick
      , root
      , cards
      , decks
      , debounceEvals
      , debounceSaves
      }

  makeAuth = do
    hasIdentified ← liftEff (Ref.newRef false)
    requestToken ← Auth.authentication
    signIn ← Bus.make
    retryEval ← liftEff $ Ref.newRef Nothing
    retrySave ← liftEff $ Ref.newRef false
    retryCardUI ← liftEff $ Ref.newRef []
    let allowedModes = allowedAuthenticationModesForAccessType accessType
    pure
      { hasIdentified
      , requestToken
      , signIn
      , allowedModes
      , permissionTokenHashes
      , retryEval
      , retrySave
      , retryCardUI
      }

  makeCache = do
    activeState ← Cache.make
    pure { activeState }

  makeBus = do
    decks ← Bus.make
    workspace ← Bus.make
    notify ← Bus.make
    globalError ← Bus.make
    stepByStep ← Bus.make
    hintDismissals ← Bus.make
    licenseProblems ← Bus.make
    themeChange ← Bus.make
    pure
      { decks
      , workspace
      , notify
      , globalError
      , stepByStep
      , hintDismissals
      , licenseProblems
      , themeChange
      }

focusDeck
  ∷ ∀ m
  . MonadAsk Wiring m
  ⇒ MonadAff SlamDataEffects m
  ⇒ DeckId
  → m Unit
focusDeck deckId = do
  { bus } ← expose
  liftAff $ Bus.write (DeckFocused deckId) bus.decks

switchDeckToFront
  ∷ ∀ m
  . MonadAsk Wiring m
  ⇒ MonadAff SlamDataEffects m
  ⇒ DeckOptions
  → m Unit
switchDeckToFront deckOpts = do
  { bus } ← expose
  liftAff $ Bus.write (SwitchToFront deckOpts) bus.decks

switchDeckToFlip
  ∷ ∀ m
  . MonadAsk Wiring m
  ⇒ MonadAff SlamDataEffects m
  ⇒ DeckOptions
  → m Unit
switchDeckToFlip deckOpts = do
  { bus } ← expose
  liftAff $ Bus.write (SwitchToFlip deckOpts) bus.decks

showDialog
  ∷ ∀ m
  . MonadAsk Wiring m
  ⇒ MonadAff SlamDataEffects m
  ⇒ Dialog
  → m Unit
showDialog dialog = do
  { bus } ← expose
  liftAff $ Bus.write (ShowDialog dialog) bus.workspace

getTheme
  ∷ ∀ m
  . MonadAsk Wiring m
  ⇒ MonadAff SlamDataEffects m
  ⇒ m (Maybe Theme.Theme)
getTheme = do
  { theme: themeRef } ← expose
  liftEff $ readRef themeRef

setTheme
  ∷ ∀ m
  . MonadAsk Wiring m
  ⇒ MonadAff SlamDataEffects m
  ⇒ Maybe Theme.Theme
  → m Unit
setTheme theme = do
  { bus, theme: themeRef } ← expose
  liftEff $ writeRef themeRef theme
  liftAff $ Bus.write theme bus.themeChange
