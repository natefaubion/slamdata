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

module SlamData.FileSystem.Component
  ( module SlamData.FileSystem.Component.State
  , module SlamData.FileSystem.Component.Query
  , module SlamData.FileSystem.Component.ChildSlot
  , component
  ) where

import SlamData.Prelude

import Control.Monad.Rec.Class (tailRecM, Step(Done, Loop))
import Control.UI.Browser (setLocation, locationString, clearValue)
import Control.UI.Browser as Browser
import Control.UI.Browser.Event as Be
import Control.UI.File as Cf

import CSS as CSS

import Data.Argonaut (jsonParser, jsonEmptyObject)
import Data.Array as Array
import Data.Coyoneda (liftCoyoneda)
import Data.Foldable as F
import Data.Lens ((.~), preview)
import Data.MediaType (MediaType(..))
import Data.MediaType.Common (textCSV, applicationJSON)
import Data.Path.Pathy (rootDir, (</>), dir, file, parentDir, printPath)
import Data.String as S
import Data.String.Regex as RX
import Data.String.Regex.Flags as RXF

import DOM.Event.Event as DEE

import Halogen as H
import Halogen.Component.Utils (busEventSource)
import Halogen.HTML as HH
import Halogen.HTML.Core as HC
import Halogen.HTML.CSS as HCSS
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.Query.EventSource as ES

import Quasar.Data (QData(..))
import Quasar.Mount as QM

import SlamData.Common.Sort (notSort)
import SlamData.Config as Config
import SlamData.Dialog.Render as RenderDialog
import SlamData.FileSystem.Breadcrumbs.Component as Breadcrumbs
import SlamData.FileSystem.Component.CSS as FileSystemClassNames
import SlamData.FileSystem.Component.ChildSlot (ChildQuery, ChildSlot)
import SlamData.FileSystem.Component.ChildSlot as CS
import SlamData.FileSystem.Component.Query (Query(..))
import SlamData.FileSystem.Component.Render (sorting, toolbar)
import SlamData.FileSystem.Component.State (State, initialState)
import SlamData.FileSystem.Component.State as State
import SlamData.FileSystem.Dialog.Component as Dialog
import SlamData.FileSystem.Dialog.Component.Message as DialogMessage
import SlamData.FileSystem.Dialog.Mount.Component as Mount
import SlamData.FileSystem.Dialog.Mount.Couchbase.Component.State as Couchbase
import SlamData.FileSystem.Dialog.Mount.MarkLogic.Component.State as MarkLogic
import SlamData.FileSystem.Dialog.Mount.MongoDB.Component.State as MongoDB
import SlamData.FileSystem.Dialog.Mount.SQL2.Component.State as SQL2
import SlamData.FileSystem.Dialog.Mount.SparkHDFS.Component.State as Spark
import SlamData.FileSystem.Dialog.Mount.SparkLocal.Component.State as SparkLocal
import SlamData.FileSystem.Listing.Component as Listing
import SlamData.FileSystem.Listing.Item (Item(..), itemResource, sortItem)
import SlamData.FileSystem.Listing.Item.Component as Item
import SlamData.FileSystem.Resource (Resource)
import SlamData.FileSystem.Resource as R
import SlamData.FileSystem.Routing (browseURL)
import SlamData.FileSystem.Routing.Salt (newSalt)
import SlamData.FileSystem.Search.Component as Search
import SlamData.GlobalError as GE
import SlamData.GlobalMenu.Component as GlobalMenu
import SlamData.Header.Component as Header
import SlamData.Header.Gripper.Component as Gripper
import SlamData.LocalStorage.Class as LS
import SlamData.LocalStorage.Keys as LSK
import SlamData.Monad (Slam)
import SlamData.Notification.Component as NC
import SlamData.Quasar (ldJSON) as API
import SlamData.Quasar.Auth (authHeaders) as API
import SlamData.Quasar.Data (makeFile, save) as API
import SlamData.Quasar.FS (children, delete, getNewName) as API
import SlamData.Quasar.Mount (mountInfo) as API
import SlamData.Render.Common (content, row)
import SlamData.Wiring as Wiring
import SlamData.Workspace.Action (Action(..), AccessType(..))
import SlamData.Workspace.Routing (mkWorkspaceURL)

import Utils (finally)
import Utils.DOM as D
import Utils.Path (DirPath, getNameStr)

type HTML = H.ParentHTML Query ChildQuery ChildSlot Slam
type DSL = H.ParentDSL State Query ChildQuery ChildSlot Void Slam

component ∷ H.Component HH.HTML Query Unit Void Slam
component =
  H.lifecycleParentComponent
    { render
    , eval
    , receiver: const Nothing
    , initialState: const initialState
    , initializer: Just (H.action Init)
    , finalizer: Nothing
    }

render ∷ State → HTML
render state@{ version, sort, salt, path } =
  HH.div
    [ HP.classes [ FileSystemClassNames.filesystem ]
    , HE.onClick (HE.input_ DismissSignInSubmenu)
    ]
    $ [ HH.slot' CS.cpHeader unit Header.component unit absurd
      , content
          [ HH.slot' CS.cpSearch unit Search.component unit $ HE.input HandleSearch
          , HH.div_
            [ Breadcrumbs.render { path, sort, salt }
            , toolbar state
            ]
          , row [ sorting state ]
          , HH.slot' CS.cpListing unit Listing.component unit $ HE.input HandleListing
          ]
      , HH.slot' CS.cpDialog unit Dialog.component unit $ HE.input HandleDialog
      , HH.slot' CS.cpNotify unit (NC.component $ NC.renderModeFromAccessType Editable) unit
          $ HE.input HandleNotifications
      ]
    ⊕ (guard state.presentIntroVideo $> renderIntroVideo)
    ⊕ (guard state.presentIntroVideo $> renderIntroVideoBackdrop)

renderIntroVideoBackdrop ∷ HTML
renderIntroVideoBackdrop =
  HH.div
    [ HP.class_ (HH.ClassName "deck-dialog-backdrop")
    , HE.onMouseDown $ HE.input_ DismissIntroVideo
    ]
    []

renderIntroVideo ∷ HTML
renderIntroVideo =
  HH.div
    [ HP.class_ $ HC.ClassName "deck-dialog" ]
    [ HH.div
        [ HCSS.style do
             CSS.paddingLeft CSS.nil
             CSS.paddingRight CSS.nil
        ]
        [ HH.h4
            [ HCSS.style do
                CSS.paddingLeft $ CSS.rem 1.0
                CSS.paddingRight $ CSS.rem 1.0
            ]
            [ HH.text "Welcome to SlamData!" ]
        , HH.video
            [ HP.autoplay true ]
            [ HH.source
                [ HP.type_ (MediaType "video/mp4")
                , HP.src "video/getting-started.mp4"
                ]
            ]
        , RenderDialog.modalFooter
            [ HH.button
                [ HP.type_ HP.ButtonButton
                , HE.onClick $ HE.input_ DismissIntroVideo
                , HP.classes [ HH.ClassName "btn", HH.ClassName "btn-primary" ]
                , HCSS.style $ CSS.marginRight $ CSS.rem 1.0
                ]
                [ HH.text "Skip video" ]
            ]
        ]
    ]

eval ∷ Query ~> DSL
eval = case _ of
  Init next → do
    w ← H.lift Wiring.expose
    whenM
      (not <$> dismissedIntroVideoBefore)
      (H.modify $ State._presentIntroVideo .~ true)
    H.subscribe $ busEventSource (flip HandleError ES.Listening) w.bus.globalError
    H.subscribe $ busEventSource (flip HandleSignInMessage ES.Listening) w.auth.signIn
    pure next
  Transition page next → do
    H.modify
      $ (State._isMount .~ page.isMount)
      ∘ (State._salt .~ page.salt)
      ∘ (State._sort .~ page.sort)
      ∘ (State._path .~ page.path)
    _ ← H.query' CS.cpListing unit $ H.action $ Listing.Reset
    _ ← H.query' CS.cpSearch unit $ H.action $ Search.SetLoading true
    _ ← H.query' CS.cpSearch unit $ H.action $ Search.SetValue $ fromMaybe "" page.query
    _ ← H.query' CS.cpSearch unit $ H.action $ Search.SetValid true
    _ ← H.query' CS.cpSearch unit $ H.action $ Search.SetPath page.path
    resort
    pure next

  PreventDefault e q → do
    H.liftEff $ DEE.preventDefault e
    eval q
  Resort next → do
    st ← H.get
    searchValue ← H.query' CS.cpSearch unit (H.request Search.GetValue)
    H.liftEff $ setLocation $ browseURL searchValue (notSort st.sort) st.salt st.path
    pure next
  SetPath path next → do
    H.modify $ State._path .~ path
    pure next
  SetSort sort next → do
    H.modify $ State._sort .~ sort
    resort
    pure next
  SetSalt salt next → do
    H.modify $ State._salt .~ salt
    pure next
  CheckIsMount path next → do
    checkIsMount path
    pure next
  CheckIsUnconfigured next → do
    checkIsUnconfigured
    pure next
  SetVersion version next → do
    H.modify $ State._version .~ Just version
    pure next

  ShowHiddenFiles next → do
    H.modify $ State._showHiddenFiles .~ true
    _ ← H.query' CS.cpListing unit $ H.action $ Listing.SetIsHidden false
    pure next
  HideHiddenFiles next → do
    H.modify $ State._showHiddenFiles .~ false
    _ ← H.query' CS.cpListing unit $ H.action $ Listing.SetIsHidden true
    pure next

  Configure next → do
    path ← H.gets _.path
    configure $ R.Database path
    pure next
  MakeMount next → do
    parent ← H.gets _.path
    showDialog $ Dialog.Mount $ Mount.New { parent }
    pure next
  MakeFolder next → do
    result ← runExceptT do
      path ← lift $ H.gets _.path
      dirName ← ExceptT $ API.getNewName path Config.newFolderName
      let
        dirPath = path </> dir dirName
        dirRes = R.Directory dirPath
        dirItem = PhantomItem dirRes
        hiddenFile = dirPath </> file (Config.folderMark)
        cleanupItem = void $ H.lift $ H.query' CS.cpListing unit $ H.action $ Listing.Filter (_ ≠ dirItem)
      _ ← H.lift $ H.query' CS.cpListing unit $ H.action $ Listing.Add dirItem
      finally cleanupItem $ ExceptT $ API.save hiddenFile jsonEmptyObject
      pure dirRes
    case result of
      Left err → case GE.fromQError err of
        Left msg →
          showDialog $ Dialog.Error
            $ "There was a problem creating the directory: " ⊕ msg
        Right ge →
          GE.raiseGlobalError ge
      Right dirRes →
        void $ H.query' CS.cpListing unit $ H.action $ Listing.Add $ Item dirRes
    pure next
  MakeWorkspace next → do
    state ← H.get
    isMounted >>= if _
      then do
        let
          newWorkspaceName = Config.newWorkspaceName ⊕ "." ⊕ Config.workspaceExtension
        name ← API.getNewName state.path newWorkspaceName
        case name of
          Left err →
            case GE.fromQError err of
              Left msg →
                -- This error isn't strictly true as we're not actually creating the
                -- workspace here, but saying there was a problem "creating a name for the
                -- workspace" would be a little strange
                showDialog $ Dialog.Error
                  $ "There was a problem creating the workspace: " ⊕ msg
              Right ge →
                GE.raiseGlobalError ge
          Right name' → do
            H.liftEff $ setLocation $ mkWorkspaceURL (state.path </> dir name') New
       else
         showDialog
           $ Dialog.Error
           $ "There was a problem creating the workspace: Path "
           ⊕ printPath state.path
           ⊕ " is not inside a mount."
    pure next
  UploadFile el next → do
    mbInput ← H.liftEff $ D.querySelector "input" el
    for_ mbInput \input →
      void $ H.liftEff $ Be.raiseEvent "click" input
    pure next
  FileListChanged el next → do
    fileArr ← map Cf.fileListToArray $ (H.liftAff $ Cf.files el)
    H.liftEff $ clearValue el
    -- TODO: notification? this shouldn't be a runtime exception anyway!
    -- let err ∷ Slam Unit
    --     err = throwError $ error "empty filelist"
    -- in H.liftAff err
    for_ (Array.head fileArr) uploadFileSelected
    pure next
  Download next → do
    path ← H.gets _.path
    download $ R.Directory path
    pure next

  DismissSignInSubmenu next → do
    dismissSignInSubmenu
    pure next
  DismissMountHint next → do
    dismissMountHint
    pure next
  DismissIntroVideo next → do
    dismissIntroVideo
    pure next

  HandleError ge next → do
    showDialog $ Dialog.Error $ GE.print ge
    pure next
  HandleListing (Listing.ItemMessage m) next → do
    handleItemMessage m
    pure next
  HandleListing (Listing.Added items) next
    | Array.length items < 2 → do
        resort
        pure next
    | otherwise → do
        path ← H.gets _.path
        presentMountHint items path
        resort
        pure next
  HandleDialog DialogMessage.Dismiss next →
    pure next
  HandleDialog DialogMessage.MountSave next → do
    mount ←
      H.query' CS.cpDialog unit $ H.request Dialog.SaveMount
    for_ (join mount) \m → do
      hideDialog
      -- check if we just edited the mount for the current directory, as if
      -- so, we don't want to add an item to the list for it
      isCurrentMount ← case m of
        R.Database path' → (\p → path' ≡ (p </> dir "")) <$> H.gets _.path
        _ → pure false
      if isCurrentMount
        then
          H.liftEff Browser.reload
        else do
          _ ← H.query' CS.cpListing unit $ H.action $ Listing.Add $ Item (R.Mount m)
          dismissMountHint
          resort
          checkIsMount =<< H.gets _.path
          checkIsUnconfigured
    pure next
  HandleDialog DialogMessage.MountDelete next → do
    mount ← H.query' CS.cpDialog unit $ H.request Dialog.SaveMount
    for_ (join mount) (remove ∘ R.Mount)
    H.liftEff Browser.reload
    pure next
  HandleDialog (DialogMessage.ExploreFile fp initialName) next → do
    { path } ← H.get
    let newWorkspaceName = initialName ⊕ "." ⊕ Config.workspaceExtension
    name ← API.getNewName path newWorkspaceName
    case name of
      Left err →
        case GE.fromQError err of
          Left msg →
            showDialog $ Dialog.Error
            $ "There was a problem creating the workspace: " ⊕ msg
          Right ge →
            GE.raiseGlobalError ge
      Right name' →
        H.liftEff $ setLocation  $ mkWorkspaceURL (path </> dir name') $ Exploring fp
    pure next
  HandleNotifications NC.ExpandGlobalMenu next → do
    gripperState ← queryHeaderGripper $ H.request Gripper.GetState
    when (gripperState ≠ Just Gripper.Opened) do
      _ ← queryHeaderGripper $ H.action $ Gripper.StartDragging 0.0
      _ ← queryHeaderGripper $ H.action Gripper.StopDragging
      pure unit
    pure next
  HandleNotifications _ next →
    pure next
  HandleSearch m next → do
    salt ← H.liftEff newSalt
    st ← H.get
    value ← case m of
      Search.Cleared →
        pure Nothing
      Search.Submit → do
        H.query' CS.cpSearch unit $ H.request Search.GetValue
    H.liftEff $ setLocation $ browseURL value st.sort salt st.path
    pure next
  SetLoading bool next → do
    _ ← H.query' CS.cpSearch unit $ H.action $ Search.SetLoading bool
    pure next
  SetIsSearching bool next → do
    _ ← H.query' CS.cpListing unit $ H.action $ Listing.SetIsSearching bool
    pure next
  AddListings items next → do
    _ ← H.query' CS.cpListing unit $ H.action $ Listing.Adds items
    pure next
  ShowError message next → do
    _ ← H.query' CS.cpDialog unit $ H.action $ Dialog.Show $ Dialog.Error message
    pure next
  HandleSignInMessage message next → do
    when (message ≡ GlobalMenu.SignInSuccess) (H.liftEff Browser.reload)
    pure next

handleItemMessage ∷ Item.Message → DSL Unit
handleItemMessage = case _ of
  Item.Selected →
    pure unit
  Item.Edit res → do
    loc ← H.liftEff locationString
    for_ (preview R._Workspace res) \wp →
      H.liftEff $ setLocation $ append (loc ⊕ "/") $ mkWorkspaceURL wp (Load Editable)
  Item.Open res → do
    { sort, salt, path } ← H.get
    loc ← H.liftEff locationString
    for_ (preview R._filePath res) \fp →
      showDialog $ Dialog.Explore fp
    for_ (preview R._dirPath res) \dp →
      H.liftEff $ setLocation $ browseURL Nothing sort salt dp
    for_ (preview R._Workspace res) \wp →
      H.liftEff $ setLocation $ append (loc ⊕ "/") $ mkWorkspaceURL wp (Load ReadOnly)
  Item.Configure (R.Mount mount) → do
    configure mount
  Item.Configure _ →
    pure unit
  Item.Move res → do
    showDialog $ Dialog.Rename res
    flip getDirectories rootDir \x →
      void $ H.query' CS.cpDialog unit $ H.action $ Dialog.AddDirsToRename x
  Item.Remove res →
    remove res
  Item.Share res → do
    path ← H.gets _.path
    loc ← map (_ ⊕ "/") $ H.liftEff locationString
    for_ (preview R._filePath res) \fp → do
      let newWorkspaceName = Config.newWorkspaceName ⊕ "." ⊕ Config.workspaceExtension
      name ← API.getNewName path newWorkspaceName
      case name of
        Left err →
          case GE.fromQError err of
            Left m →
              showDialog $ Dialog.Error
                $ "There was a problem creating the workspace: " ⊕ m
            Right ge →
              GE.raiseGlobalError ge
        Right name' → do
          showDialog (Dialog.Share $ append loc $  mkWorkspaceURL (path </> dir name') $ Exploring fp)
    for_ (preview R._Workspace res) \wp → do
      showDialog (Dialog.Share $ append loc $ mkWorkspaceURL wp (Load ReadOnly))
  Item.Download res →
    download res

checkIsMount ∷ DirPath → DSL Unit
checkIsMount path = do
  isMount ← isRight <$> API.mountInfo (Left path)
  H.modify $ State._isMount .~ isMount

isMounted ∷ DSL Boolean
isMounted = do
  path ← H.gets _.path
  tailRecM go path
  where
  go ∷ DirPath → DSL (Step DirPath Boolean)
  go path = do
    isMount ← isRight <$> API.mountInfo (Left path)
    pure
      $ if isMount
          then Done true
          else maybe (Done false) Loop (parentDir path)

checkIsUnconfigured ∷ DSL Unit
checkIsUnconfigured = do
  isMount ← isRight <$> API.mountInfo (Left rootDir)
  isEmpty ← either (const false) Array.null <$> API.children rootDir
  H.modify $ State._isUnconfigured .~ (not isMount ∧ isEmpty)

remove ∷ Resource → DSL Unit
remove res = do
  -- Replace actual item with phantom
  _ ← H.query' CS.cpListing unit $ H.action $ Listing.Filter $ not ∘ eq res ∘ itemResource
  _ ← H.query' CS.cpListing unit $ H.action $ Listing.Add $ PhantomItem res
  -- Save order of items during deletion (or phantom will be on top of list)
  resort
  -- Try to delete
  mbTrashFolder ← H.lift $ API.delete res
  -- Remove phantom resource after we have response from server
  _ ← H.query' CS.cpListing unit $ H.action $ Listing.Filter $ not ∘ eq res ∘ itemResource
  case mbTrashFolder of
    Left err → do
      -- Error occured: put item back and show dialog
      void $ H.query' CS.cpListing unit $ H.action $ Listing.Add (Item res)
      case GE.fromQError err of
        Left m →
          showDialog $ Dialog.Error m
        Right ge →
          GE.raiseGlobalError ge
    Right mbRes →
      -- Item has been deleted: probably add trash folder
      for_ mbRes \res' →
        void $ H.query' CS.cpListing unit $ H.action $ Listing.Add (Item res')

  listing ← fromMaybe [] <$> (H.query' CS.cpListing unit $ H.request Listing.Get)
  path ← H.gets _.path
  presentMountHint listing path

  resort
  checkIsMount path
  checkIsUnconfigured

dismissMountHint ∷ DSL Unit
dismissMountHint = do
  LS.persist LSK.dismissedMountHintKey true
  H.modify $ State._presentMountHint .~ false

dismissIntroVideo ∷ DSL Unit
dismissIntroVideo = do
  LS.persist LSK.dismissedIntroVideoKey true
  H.modify $ State._presentIntroVideo .~ false

dismissedIntroVideoBefore ∷ DSL Boolean
dismissedIntroVideoBefore =
  either (const false) id <$> LS.retrieve LSK.dismissedIntroVideoKey

uploadFileSelected ∷ Cf.File → DSL Unit
uploadFileSelected f = do
  { path, sort, salt } ← H.get
  name ←
    H.liftEff (Cf.name f)
      <#> RX.replace (unsafePartial fromRight $ RX.regex "/" RXF.global) ":"
      >>= API.getNewName path

  case name of
    Left err → handleError err
    Right name' → do
      reader ← H.liftEff Cf.newReaderEff
      content' ← H.liftAff $ Cf.readAsBinaryString f reader

      let fileName = path </> file name'
          res = R.File fileName
          fileItem = PhantomItem res
          ext = Array.last (S.split (S.Pattern ".") name')
          mime = if ext ≡ Just "csv"
                 then textCSV
                 else if isApplicationJSON content'
                      then applicationJSON
                      else API.ldJSON
      _ ← H.query' CS.cpListing unit $ H.action (Listing.Add fileItem)
      f' ← API.makeFile fileName (CustomData mime content')
      _ ← H.query' CS.cpListing unit $ H.action $ Listing.Filter (not ∘ eq res ∘ itemResource)
      case f' of
        Left err → handleError err
        Right _ →
          void $ H.query' CS.cpListing unit $ H.action $ Listing.Add (Item res)

  where
  isApplicationJSON ∷ String → Boolean
  isApplicationJSON content'
    -- Parse if content is small enough
    | S.length content' < 1048576 = isRight $ jsonParser content'
    -- Or check if its first/last characters are [/]
    | otherwise =
        let trimmed = S.trim content'
        in (startsWithEndsWith "[" "]" trimmed) || (startsWithEndsWith "{" "}" trimmed)

  startsWithEndsWith startsWith endsWith s =
    F.all isJust [S.stripPrefix (S.Pattern startsWith) s, S.stripSuffix (S.Pattern endsWith) s]

  handleError err =
    case GE.fromQError err of
      Left msg → showDialog $ Dialog.Error msg
      Right ge → GE.raiseGlobalError ge

presentMountHint ∷ ∀ a. Array a → DirPath → DSL Unit
presentMountHint xs path = do
  isSearching ←
    map (fromMaybe false) $ H.query' CS.cpSearch unit (H.request Search.IsSearching)
  isLoading ←
    map (fromMaybe true)  $ H.query' CS.cpSearch unit (H.request Search.IsLoading)

  H.modify
    ∘ (State._presentMountHint .~ _)
    ∘ ((Array.null xs ∧ path ≡ rootDir ∧ not (isSearching ∧ isLoading)) ∧ _)
    ∘ not
    ∘ either (const false) id
    =<< dismissedBefore
  where
  dismissedBefore ∷ DSL (Either String Boolean)
  dismissedBefore =
    LS.retrieve LSK.dismissedMountHintKey

dismissSignInSubmenu ∷ DSL Unit
dismissSignInSubmenu =
  void $ queryGlobalMenu (H.action GlobalMenu.DismissSubmenu)

resort ∷ DSL Unit
resort = do
  sort ← H.gets _.sort
  H.query' CS.cpSearch unit (H.request Search.IsSearching)
    >>= traverse_ \isSearching →
      void $ H.query' CS.cpListing unit $ H.action $ Listing.SortBy (sortItem isSearching sort)

configure ∷ R.Mount → DSL Unit
configure m =
  API.mountInfo anyPath >>= case m, _ of
    R.View path, Left err → raiseError err
    R.Database path, Left err
      | path /= rootDir → raiseError err
      | otherwise →
          showDialog $ Dialog.Mount Mount.Root
    R.View path, Right config →
      showDialog $ Dialog.Mount
        $ Mount.Edit
          { parent: either parentDir parentDir anyPath
          , name: Just $ getNameStr anyPath
          , settings: fromConfig config
          }
    R.Database path, Right config →
      showDialog $ Dialog.Mount
        $ Mount.Edit
          { parent: if path ≡ rootDir then Nothing else either parentDir parentDir anyPath
          , name: if path ≡ rootDir then Nothing else Just $ getNameStr anyPath
          , settings: fromConfig config
          }
  where
    anyPath =
      R.mountPath m

    fromConfig = case _ of
      QM.ViewConfig config → Mount.SQL2 (SQL2.stateFromViewInfo config)
      QM.MongoDBConfig config → Mount.MongoDB (MongoDB.fromConfig config)
      QM.CouchbaseConfig config → Mount.Couchbase (Couchbase.fromConfig config)
      QM.MarkLogicConfig config → Mount.MarkLogic (MarkLogic.fromConfig config)
      QM.SparkHDFSConfig config → Mount.SparkHDFS (Spark.fromConfig config)
      QM.SparkLocalConfig config → Mount.SparkLocal (SparkLocal.fromConfig config)

    raiseError err = case GE.fromQError err of
      Left msg →
        showDialog $ Dialog.Error
          $ "There was a problem reading the mount settings: "
          ⊕ msg
      Right ge →
        GE.raiseGlobalError ge

download ∷ R.Resource → DSL Unit
download res = do
  hs ← H.lift API.authHeaders
  showDialog $ Dialog.Download res hs
  pure unit

getChildren
  ∷ (R.Resource → Boolean)
  → (Array R.Resource → DSL Unit)
  → DirPath
  → DSL Unit
getChildren pred cont start = do
  ei ← API.children start
  case ei of
    Right items → do
      let items' = Array.filter pred items
          parents = Array.mapMaybe (either Just (const Nothing) ∘ R.getPath) items
      cont items'
      traverse_ (getChildren pred cont) parents
    _ → pure unit

getDirectories ∷ (Array R.Resource → DSL Unit) → DirPath → DSL Unit
getDirectories = getChildren $ R.isDirectory ∨ R.isDatabaseMount

showDialog ∷ Dialog.Dialog → DSL Unit
showDialog = void ∘ H.query' CS.cpDialog unit ∘ H.action ∘ Dialog.Show

hideDialog ∷ DSL Unit
hideDialog =
  void $ H.query' CS.cpDialog unit $ H.action Dialog.RaiseDismiss

queryHeaderGripper ∷ ∀ a. Gripper.Query a → DSL (Maybe a)
queryHeaderGripper =
   H.query' CS.cpHeader unit ∘ Header.QueryGripper ∘ liftCoyoneda

queryGlobalMenu ∷ ∀ a. GlobalMenu.Query a → DSL (Maybe a)
queryGlobalMenu =
   H.query' CS.cpHeader unit ∘ Header.QueryGlobalMenu ∘ liftCoyoneda
