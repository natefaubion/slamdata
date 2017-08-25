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

module SlamData.FileSystem (main) where

import SlamData.Prelude

import Ace.Config as AceConfig
import Control.Coroutine (Producer, Consumer, consumer, producer, ($$), runProcess)
import Control.Monad.Aff (Aff, delay)
import Control.Monad.Aff.AVar (AVar, makeVar, makeVar', modifyVar, putVar, takeVar)
import Control.Monad.Aff.Class (liftAff)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Exception (Error, error)
import Control.Monad.Fork (Canceler(..), fork, cancel)
import Control.UI.Browser (setTitle, replaceLocation)
import Data.Array (filter, mapMaybe, take, drop)
import Data.Lens (Lens', lens, (%~), (<>~))
import Data.Map as M
import Data.Path.Pathy ((</>), rootDir, parseAbsDir, sandbox, currentDir)
import Data.StrMap as SM
import Data.Time.Duration (Milliseconds(..))
import DOM (DOM)
import Halogen as H
import Halogen.Aff as HA
import Halogen.VDom.Driver (runUI, HalogenIO)
import Quasar.Error as QE
import Routing (matchesAff)
import SlamData.Analytics as Analytics
import SlamData.Common.Sort (Sort(..))
import SlamData.Config as Config
import SlamData.Config.Version (slamDataVersion)
import SlamData.Effects (SlamDataEffects)
import SlamData.FileSystem.Component (Query(..), component)
import SlamData.FileSystem.Listing.Item (Item(..))
import SlamData.FileSystem.Resource (getPath)
import SlamData.FileSystem.Routing (Routes(..), routing, browseURL)
import SlamData.FileSystem.Routing.Salt (Salt, newSalt)
import SlamData.FileSystem.Routing.Search (isSearchQuery, searchPath, filterByQuery)
import SlamData.GlobalError as GE
import SlamData.Monad (Slam, runSlam)
import SlamData.Quasar.Auth.Permission as Permission
import SlamData.Quasar.FS (children) as Quasar
import SlamData.Wiring as Wiring
import SlamData.Workspace.AccessType (AccessType(..))
import Text.SlamSearch.Printer (strQuery)
import Text.SlamSearch.Types (SearchQuery)
import Utils.Path (DirPath, hidePath, renderPath)

type FileSystemIO = HalogenIO Query Void (Aff SlamDataEffects)

main ∷ Eff SlamDataEffects Unit
main = do
  _ ← AceConfig.set AceConfig.basePath $ Config.baseUrl ⊕ "js/ace"
  _ ← AceConfig.set AceConfig.modePath $ Config.baseUrl ⊕ "js/ace"
  _ ← AceConfig.set AceConfig.themePath $ Config.baseUrl ⊕ "js/ace"

  HA.runHalogenAff do
    _ ← fork Analytics.enableAnalytics
    permissionTokenHashes ← liftEff $ Permission.retrieveTokenHashes
    wiring ← Wiring.make rootDir Editable SM.empty permissionTokenHashes
    let ui = H.hoist (runSlam wiring) component
    driver ← runUI ui unit =<< HA.awaitBody

    _ ← fork do
      setSlamDataTitle slamDataVersion
      driver.query $ H.action $ SetVersion slamDataVersion

    runSlam wiring $ fork $ routeSignal driver

setSlamDataTitle ∷ ∀ e. String → Aff (dom ∷ DOM|e) Unit
setSlamDataTitle version =
  liftEff $ setTitle $ "SlamData " ⊕ version


type ListingState =
  { canceler ∷ Canceler Error Slam
    -- depth ↔ active requests
  , requestMap ∷ M.Map Int Int
  , queue ∷ Array { depth ∷ Int, dir ∷ DirPath }
  }

initialListingState ∷ ListingState
initialListingState =
  { canceler: mempty
  , requestMap: M.singleton 0 0
  , queue: []
  }

_canceler ∷ ∀ a r. Lens' { canceler ∷ a | r } a
_canceler = lens _.canceler _{ canceler = _ }

_requestMap ∷ ∀ a r. Lens' { requestMap ∷ a | r } a
_requestMap = lens _.requestMap _{ requestMap = _ }

_queue ∷ ∀ a r. Lens' { queue ∷ a | r } a
_queue = lens _.queue _{ queue = _ }

routeSignal ∷ FileSystemIO → Slam Unit
routeSignal driver = do
  avar ←
    liftAff $ makeVar' initialListingState

  routeTpl ←
    liftAff $ matchesAff routing

  flip uncurry routeTpl
    $ redirects driver avar

redirects
  ∷ FileSystemIO
  → AVar ListingState
  → Maybe Routes
  → Routes
  → Slam Unit
redirects driver var mbOld = case _ of
  Index →
    updateURL Nothing Asc Nothing rootDir

  Sort sort →
    updateURL Nothing sort Nothing rootDir

  SortAndQ sort query →
    let
      queryParts = splitQuery query
    in
      updateURL queryParts.query sort Nothing queryParts.path

  Salted sort query salt → do
    {canceler} ← liftAff $ takeVar var

    _ ← cancel canceler $ error "cancel search"

    liftAff
      $ putVar var initialListingState

    liftAff
      $ driver.query
      $ H.action
      $ SetIsSearching
      $ isSearchQuery query

    let
      queryParts = splitQuery query

      isNewPage = fromMaybe true do
        old ← mbOld
        oldQuery × oldSalt ← case old of
          Salted _ oldQuery' oldSalt' → pure $ oldQuery' × oldSalt'
          _ → Nothing
        pure $ oldQuery ≠ query ∨ oldSalt ≠ salt

    if isNewPage
      then do
      liftAff $ driver.query $ H.action $ Transition
        { path: queryParts.path
        , query: queryParts.query
        , sort
        , salt
        , isMount: false
        }

      let
        p = listingProducer var query queryParts.path
        c = listingConsumer driver
        process = p $$ c

      listingProcessHandler driver =<< runProcess process

      when (isNothing queryParts.query)
        $ liftAff $ driver.query $ H.action $ CheckIsMount queryParts.path
      liftAff $ driver.query $ H.action $ CheckIsUnconfigured
      else
        liftAff $ driver.query $ H.action $ SetLoading false

listingProducer
  ∷ AVar ListingState
  → SearchQuery
  → DirPath
  → Producer (Array Item) Slam (Maybe QE.QError)
listingProducer var query startingDir = produceSlam $ go startingDir 0
  where
  go dir depth emit = do
    liftAff
      $ flip modifyVar var
      $ _requestMap %~ M.alter memThisRequest depth

    canceler ←
      map Canceler $ fork $ runRequest dir depth emit

    liftAff
      $ flip modifyVar var
      $ _canceler <>~ canceler

  runRequest dir depth emit = do
    liftAff $ delay requestDelay
    Quasar.children dir >>= case _ of
      Left e → case GE.fromQError e of
        -- Do not show error message notification if we're in root or searching
        Left _ | (not $ isSearchQuery query) ∨ depth ≡ 0 →
          emit $ Right Nothing
        _ →
          emit $ Right $ Just e

      Right cs →
        getChildren depth emit cs

    liftAff
      $ flip modifyVar var
      $ _requestMap %~ M.alter markThisRequestAsFinished depth

    st@{canceler: c, requestMap: r} ←
      liftAff $ takeVar var

    if allRequestsAreFinished r
      then do
      liftAff $ putVar var initialListingState
      emit $ Right Nothing
      else
      liftAff $ putVar var st

  getChildren depth emit ress = do
    let
      next =
        map {dir: _, depth: depth + one}
          $ flip mapMaybe ress
          $ either Just (const Nothing)
          ∘ getPath

      toAdd =
        map Item
          $ flip filter ress
          $ filterByQuery query

    emit $ Left toAdd

    if isSearchQuery query
      then do
      liftAff
        $ flip modifyVar var
        $ _queue %~ flip append next

      st ←
        liftAff $ takeVar var

      let
        running = runningRequests st.requestMap
        count = max 0 $ maxParallelRequests - running
        toRun = take count st.queue
        toPut = drop count st.queue

      liftAff
        $ putVar var
        $ st{queue = toPut}

      void $ fork $ flip parTraverse_ toRun \r →
        go r.dir r.depth emit

      else
      emit $ Right Nothing

  -- This is just emperical stuff, things works nice on my machine with them @cryogenian
  maxParallelRequests =
    16

  requestDelay =
    Milliseconds 64.0

  memThisRequest = case _ of
    Nothing → Just 1
    Just n → Just $ n + 1

  markThisRequestAsFinished = case _ of
    Just v | v > 1 → Just $ v - 1
    _ → Nothing

  -- Note that initialState is (zero ↔ zero) and final Ø
  allRequestsAreFinished = M.isEmpty

  runningRequests =
    foldl add zero ∘ M.values

listingConsumer
  ∷ FileSystemIO
  → Consumer (Array Item) Slam (Maybe QE.QError)
listingConsumer driver = consumer \is → do
  liftAff
    $ driver.query
    $ H.action
    $ AddListings is
  pure Nothing

listingProcessHandler
  ∷ FileSystemIO
  → Maybe QE.QError
  → Slam Unit
listingProcessHandler driver = case _ of
  Nothing →
    liftAff
      $ driver.query
      $ H.action
      $ SetLoading false

  Just err → case GE.fromQError err of
    Left msg →
      presentError
        $ "There was a problem accessing this directory listing. " <> msg
    Right ge →
      GE.raiseGlobalError ge

  where
  presentError message =
    liftAff
      $ driver.query
      $ H.action
      $ ShowError message

updateURL
  ∷ Maybe String
  → Sort
  → Maybe Salt
  → DirPath
  → Slam Unit
updateURL query sort salt path = liftEff do
  salt' ← maybe newSalt pure salt
  replaceLocation $ browseURL query sort salt' path

splitQuery
  ∷ SearchQuery
  → { path ∷ DirPath, query ∷ Maybe String }
splitQuery q =
  { path: path
  , query: query
  }
  where
  path =
    rootDir </> fromMaybe currentDir
      (searchPath q >>= parseAbsDir >>= sandbox rootDir)
  query = do
    guard $ isSearchQuery q
    pure $ hidePath (renderPath $ Left path) (strQuery q)

produceSlam
  ∷ ∀ a r
  . ((a ⊹ r → Slam Unit) → Slam Unit)
  → Producer a Slam r
produceSlam recv = do
  v ← lift $ liftAff makeVar
  _ ← lift $ fork $ recv $ liftAff ∘ putVar v
  producer $ liftAff $ takeVar v
