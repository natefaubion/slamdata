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

module SlamData.Workspace (main) where

import SlamData.Prelude

import Control.Monad.Aff (Aff, forkAff)
import Control.Monad.Eff (Eff)

import Ace.Config as AceConfig

import Halogen (Driver, runUI, parentState)
import Halogen.Util (runHalogenAff, awaitBody)

import SlamData.Config as Config
import SlamData.FileSystem.Routing (parentURL)
import SlamData.Workspace.Action (Action(..), toAccessType)
import SlamData.Workspace.Card.Port as Port
import SlamData.Workspace.Component as Workspace
import SlamData.Workspace.Deck.Component as Deck
import SlamData.Workspace.Deck.DeckId (DeckId)
import SlamData.Effects (SlamDataRawEffects, SlamDataEffects)
import SlamData.Workspace.Routing (Routes(..), routing)
import SlamData.Workspace.StyleLoader as StyleLoader

import Utils.Path as UP

main ∷ Eff SlamDataEffects Unit
main = do
  AceConfig.set AceConfig.basePath (Config.baseUrl ⊕ "js/ace")
  AceConfig.set AceConfig.modePath (Config.baseUrl ⊕ "js/ace")
  AceConfig.set AceConfig.themePath (Config.baseUrl ⊕ "js/ace")
  runHalogenAff do
    let st = Workspace.initialState (Just "3.0")
    driver ← runUI Workspace.comp (parentState st) =<< awaitBody
    forkAff (routeSignal driver)
  StyleLoader.loadStyles

routeSignal
  ∷ Driver Workspace.QueryP SlamDataRawEffects
  → Aff SlamDataEffects Unit
routeSignal driver =
  Routing.matchesAff' UP.decodeURIPath routing >>= snd >>> case _ of
    WorkspaceRoute res deckId action varMap →
      workspace res deckId action varMap

  where
  workspace
    ∷ UP.DirPath
    → Maybe DeckId
    → Action
    → Port.VarMap
    → Aff SlamDataEffects Unit
  workspace path deckId action varMap = do
    let name = UP.getNameStr $ Left path
        accessType = toAccessType action
    currentPath ← driver $ Workspace.fromWorkspace Workspace.GetPath

    when (currentPath ≠ pure path) case action of
      New → driver $ Workspace.toWorkspace $ Workspace.Reset (Just path)
      Load _ → driver $ Workspace.toWorkspace $ Workspace.Load path deckId
      Exploring fp → do
        driver $ Workspace.toWorkspace $ Workspace.Reset (Just path)
        driver $ Workspace.toDeck $ Deck.ExploreFile fp

    driver $ Workspace.toWorkspace $ Workspace.SetAccessType accessType
    driver $ Workspace.toDeck $ Deck.SetGlobalVarMap varMap
    driver $ Workspace.toWorkspace $ Workspace.SetParentHref
      $ parentURL $ Left path
