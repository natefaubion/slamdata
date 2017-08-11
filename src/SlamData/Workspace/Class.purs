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

module SlamData.Workspace.Class
  ( class WorkspaceDSL
  , changeTheme
  , navigate
  , navigateToDeck
  , navigateToIndex
  , module SlamData.Workspace.Routing
  ) where

import SlamData.Prelude

import Control.Monad.Aff (delay)
import Control.Monad.Aff.AVar (AVAR)
import Control.Monad.Aff.Class (class MonadAff, liftAff)
import Control.Monad.Eff.Class (class MonadEff, liftEff)
import Control.Monad.Eff.Now (NOW, now)
import Control.Monad.Eff.Ref (REF, readRef)
import Control.Monad.Eff.Timer (TIMER, setTimeout)
import Control.UI.Browser as Browser
import Data.Int (ceil)
import DOM (DOM)
import DOM.HTML (window)
import DOM.HTML.Types as Ht
import DOM.HTML.Window as Win
import DOM.Node.Element (setAttribute)
import DOM.Node.NonElementParentNode (getElementById)
import DOM.Node.Types as Nt
import Data.DateTime.Instant (unInstant)
import Data.List as L
import Data.URI (printURIRef)
import Halogen.Query (HalogenM)
import Math as Math
import SlamData.FileSystem.Routing (parentURL)
import SlamData.Theme.Theme as Theme
import SlamData.Wiring (Wiring)
import SlamData.Wiring as Wiring
import SlamData.Workspace.Action as WA
import SlamData.Workspace.Deck.DeckId (DeckId)
import SlamData.Workspace.Routing (Routes(..))
import Utils.DOM (hideLoadingOverlay, loadStyleSheet, showLoadingOverlay)

class WorkspaceDSL (m ∷ Type → Type) where
  navigate ∷ Routes → m Unit

instance workspaceDSLMaybeT ∷ (Monad m, WorkspaceDSL m) ⇒ WorkspaceDSL (MaybeT m) where
  navigate = lift ∘ navigate

instance workspaceDSLExceptT ∷ (Monad m, WorkspaceDSL m) ⇒ WorkspaceDSL (ExceptT e m) where
  navigate = lift ∘ navigate

instance workspaceDSLHalogenM ∷ (Monad m, WorkspaceDSL m) ⇒ WorkspaceDSL (HalogenM s f g p o m) where
  navigate = lift ∘ navigate

changeTheme
  ∷ ∀ m eff
  . MonadAsk Wiring m
  ⇒ MonadAff (avar ∷ AVAR, dom ∷ DOM, now ∷ NOW, ref ∷ REF, timer ∷ TIMER | eff) m
  ⇒ Maybe Theme.Theme
  → m Unit
changeTheme theme = do
  let uri = Theme.getURI $ fromMaybe Theme.Light theme
  liftEff showLoadingOverlay
  start <- liftEff now
  liftAff $ loadStyleSheet uri
  liftEff do
    doc ← Win.document =<< window
    mbStyle ← getElementById (Nt.ElementId "theme-css") (Ht.htmlDocumentToNonElementParentNode doc)
    for_ mbStyle $ setAttribute "href" (printURIRef uri)
    end <- liftEff now
    -- Delay to allow the screen to repaint. It's not sync and
    -- `requestAnimationFrame` did not work.  So this `d` guarantees that
    -- the overlay is up for some unjarring amount of time. If the overlay
    -- is up for a 100ms flash, it feels bad. 350 seemed to me like a
    -- reasonable number (with 200ms being easily humanly perceptible).
    let d = 350.0 - ((unwrap $ unInstant end) - (unwrap $ unInstant start))
    _ ← setTimeout (ceil $ Math.max 0.0 d) hideLoadingOverlay
    pure unit
  -- We need to defer rendering the app by a non-zero tick (so as not to
  -- invoke setImmediate) to give FF a chance to recalc styles.
  liftAff $ delay one
  Wiring.setTheme theme

navigateToDeck
  ∷ ∀ m eff
  . MonadAsk Wiring m
  ⇒ MonadEff (ref ∷ REF, dom ∷ DOM | eff) m
  ⇒ WorkspaceDSL m
  ⇒ L.List DeckId
  → m Unit
navigateToDeck = case _ of
  L.Nil → navigateToIndex
  cursor → do
    { path, accessType, varMaps } ← Wiring.expose
    urlVarMaps ← liftEff $ readRef varMaps
    navigate $ WorkspaceRoute path cursor (WA.Load accessType) urlVarMaps

navigateToIndex
  ∷ ∀ m eff
  . MonadAsk Wiring m
  ⇒ MonadEff (dom ∷ DOM | eff) m
  ⇒ m Unit
navigateToIndex = do
  { path } ← Wiring.expose
  void $ liftEff $ Browser.setHref $ parentURL $ Left path
