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

import CSS as CSS
import Control.Monad.Aff.AVar as AVar
import Control.Monad.Fork (fork)
import Control.Monad.Rec.Class (tailRecM, Step(Done, Loop))
import Control.UI.Browser as Browser
import Control.UI.Browser.Event as Be
import Utils.File as UF
import DOM.Event.Event as DEE
import DOM.File.Types as DF
import DOM.File.File as DFF
import DOM.File.FileList as DFL
import DOM.HTML.HTMLInputElement as HIE
import Data.Argonaut as J
import Data.Array as Array
import Data.Coyoneda (liftCoyoneda)
import Data.Foldable as F
import Data.Lens (preview)
import Data.MediaType (MediaType(..))
import Data.Path.Pathy (rootDir, (</>), dir, file, parentDir, printPath)
import Data.String as S
import Data.String.Regex as RX
import Data.String.Regex.Flags as RXF
import Data.Variant as V
import Halogen as H
import Halogen.Component.Utils (busEventSource)
import Halogen.HTML as HH
import Halogen.HTML.CSS as HCSS
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.Query.EventSource as ES
import Quasar.Advanced.QuasarAF as QA
import Quasar.Advanced.Types as QAT
import Quasar.Data as QD
import Quasar.Data.CSV as CSV
import Quasar.Data.Json as Json
import Quasar.Error as QE
import SlamData.AdminUI.Component as AdminUI
import SlamData.AdminUI.Types as AdminUI.Types
import SlamData.Common.Sort (notSort)
import SlamData.Config as Config
import SlamData.Dialog.License.Component as LicenseDialog
import SlamData.Dialog.Render as RenderDialog
import SlamData.FileSystem.Breadcrumbs.Component as Breadcrumbs
import SlamData.FileSystem.Component.CSS as FileSystemClassNames
import SlamData.FileSystem.Component.ChildSlot (ChildQuery, ChildSlot)
import SlamData.FileSystem.Component.ChildSlot as CS
import SlamData.FileSystem.Component.Query (Query(..))
import SlamData.FileSystem.Component.Render (sorting, toolbar)
import SlamData.FileSystem.Component.State (State, initialState)
import SlamData.FileSystem.Dialog as Dialog
import SlamData.FileSystem.Dialog.Mount.Component as Mount
import SlamData.FileSystem.Listing.Component as Listing
import SlamData.FileSystem.Listing.Item (Item(..), itemResource, sortItem)
import SlamData.FileSystem.Listing.Item.Component as Item
import SlamData.FileSystem.Resource as R
import SlamData.FileSystem.Routing (browseURL, parentURL)
import SlamData.FileSystem.Routing.Salt (newSalt)
import SlamData.FileSystem.Search.Component as Search
import SlamData.GlobalError as GE
import SlamData.GlobalMenu.Component as GlobalMenu
import SlamData.Header.Component as Header
import SlamData.Header.Gripper.Component as Gripper
import SlamData.LocalStorage.Class as LS
import SlamData.LocalStorage.Keys as LSK
import SlamData.Monad (Slam)
import SlamData.Monad.License (notifyDaysRemainingIfNeeded)
import SlamData.Notification.Component as NC
import SlamData.Quasar.Class (liftQuasar)
import SlamData.Quasar.Data (makeFile, makeDir, save) as API
import SlamData.Quasar.FS (children, getNewName) as API
import SlamData.Quasar.Mount (mountInfo) as API
import SlamData.Render.ClassName as CN
import SlamData.Render.Common (content, row)
import SlamData.Wiring as Wiring
import SlamData.Workspace.Action (Action(..), AccessType(..))
import SlamData.Workspace.Routing (mkWorkspaceURL)
import Utils (finally)
import Utils.DOM as DOM
import Utils.Path (AnyPath, DirPath)

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
    $ [ HH.slot' CS.cpHeader unit Header.component unit $ HE.input HandleHeader
      , content
          [ HH.slot' CS.cpSearch unit Search.component unit $ HE.input HandleSearch
          , HH.div_
            [ Breadcrumbs.render { path, sort, salt }
            , toolbar state
            ]
          , row [ sorting state ]
          , HH.slot' CS.cpListing unit Listing.component unit $ HE.input HandleListing
          ]
      , HH.slot' CS.cpLicenseDialog unit LicenseDialog.component state.licenseProblem (HE.input_ (HandleLicenseProblem Nothing))
      , HH.slot' CS.cpDialog unit Dialog.component state.dialog $ HE.input HandleDialog
      , HH.slot' CS.cpNotify unit (NC.component NC.Hidden) unit $ HE.input HandleNotifications
      , HH.slot' CS.cpAdminUI unit AdminUI.component unit $ HE.input HandleAdminUI
      ]
    <> (guard state.presentIntroVideo $> renderIntroVideo)

renderIntroVideo ∷ HTML
renderIntroVideo =
  HH.div
    [ HP.class_ CN.dialogContainer
    , HE.onClick $ HE.input DismissIntroVideoBackdrop
    ]
    [ HH.div
        [ HP.class_ CN.dialog
        , HCSS.style do
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
    w ← Wiring.expose
    dismissedIntroVideoBefore >>= if _
      then
        void $ H.query' CS.cpNotify unit $ H.action $ NC.UpdateRenderMode NC.Notifications
      else
        void $ fork $ liftQuasar QA.licenseInfo >>= case _ of
          Right { status: QAT.LicenseValid } →
            H.modify (_ { presentIntroVideo = true })
          Right { status: QAT.LicenseExpired } →
            pure unit
          Left _ →
            liftQuasar QA.serverInfo >>= traverse_ \{ name } →
              when (name /= "Quasar-Advanced") $
                H.modify (_ { presentIntroVideo = true })
    H.subscribe $ busEventSource (flip HandleError ES.Listening) w.bus.globalError
    H.subscribe $ busEventSource (flip HandleSignInMessage ES.Listening) w.auth.signIn
    H.subscribe $ busEventSource (flip (HandleLicenseProblem ∘ Just) ES.Listening) w.bus.licenseProblems
    notifyDaysRemainingIfNeeded
    pure next
  Transition page next → do
    H.modify (_ { isMount = page.isMount, salt = page.salt, sort = page.sort, path = page.path })
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
    H.liftEff $ Browser.setLocation $ browseURL searchValue (notSort st.sort) st.salt st.path
    pure next
  SetPath path next → do
    H.modify (_ { path = path })
    pure next
  SetSort sort next → do
    H.modify (_ { sort = sort })
    resort
    pure next
  SetSalt salt next → do
    H.modify (_ { salt = salt })
    pure next
  CheckIsMount path next → do
    checkIsMount path
    pure next
  CheckIsUnconfigured next → do
    checkIsUnconfigured
    pure next
  SetVersion version next → do
    H.modify (_ { version = Just version })
    pure next
  ShowHiddenFiles next → do
    H.modify (_ { showHiddenFiles = true })
    _ ← H.query' CS.cpListing unit $ H.action $ Listing.SetIsHidden false
    pure next
  HideHiddenFiles next → do
    H.modify (_ { showHiddenFiles = false })
    _ ← H.query' CS.cpListing unit $ H.action $ Listing.SetIsHidden true
    pure next
  Configure next → do
    path ← H.gets _.path
    configure true (R.Database path)
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
      finally cleanupItem $ ExceptT $ API.save hiddenFile J.jsonEmptyObject
      pure dirRes
    case result of
      Left err → case GE.fromQError err of
        Left msg →
          showDialog $ Dialog.Error
            $ "You can only create files or folders in data sources."
            ⊕ "Please mount a data source, and then create your file or location inside the mounted data source."
        Right ge →
          GE.raiseGlobalError ge
      Right dirRes →
        void $ H.query' CS.cpListing unit $ H.action $ Listing.Add $ Item dirRes
    pure next
  MakeWorkspace next → do
    state ← H.get
    isMounted >>= if _
       then
         createWorkspace state.path \mkUrl →
           H.liftEff $ Browser.setLocation $ mkUrl New
       else
         showDialog
           $ Dialog.Error
           $ "There was a problem creating the workspace: Path "
           ⊕ printPath state.path
           ⊕ " is not inside a mount."
    pure next
  UploadFile el next → do
    H.liftEff $ traverse_ (Be.raiseEvent "click") =<< DOM.querySelector "input" el
    pure next
  FileListChanged el next → do
    files ← H.liftEff $ HIE.files el
    traverse_ uploadFileSelected (DFL.item 0 =<< files)
    -- Clear the input so the same file can be re-uploaded if necessary
    H.liftEff $ HIE.setValue "" el
    pure next
  Download next → do
    showDialog ∘ Dialog.Download ∘ R.Directory =<< H.gets _.path
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
  DismissIntroVideoBackdrop me next → do
    isDialog ← H.liftEff $ DOM.nodeEq (DOM.target me) (DOM.currentTarget me)
    when isDialog do
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
  HandleDialog (Dialog.Bubble msg) next → do
    msg # (V.case_
      # V.on (SProxy ∷ SProxy "mounted") (const repopulate)
      # V.on (SProxy ∷ SProxy "renamed") (const repopulate)
      # V.on (SProxy ∷ SProxy "deleted") handleDeleted)
    pure next
  HandleDialog Dialog.Dismiss next → do
    H.modify (_ { dialog = Nothing })
    pure next
  HandleHeader (Header.GlobalMenuMessage GlobalMenu.OpenAdminUI) next → do
    _ ← H.query' CS.cpAdminUI unit (H.action AdminUI.Types.Open)
    pure next
  HandleHeader _ next →
    pure next
  HandleNotifications NC.ExpandGlobalMenu next → do
    gripperState ← queryHeaderGripper $ H.request Gripper.GetState
    when (gripperState ≠ Just Gripper.Opened) do
      _ ← queryHeaderGripper $ H.action $ Gripper.StartDragging 0.0
      _ ← queryHeaderGripper $ H.action Gripper.StopDragging
      pure unit
    pure next
  HandleNotifications (NC.Fulfill trigger) next → do
    H.liftAff $ AVar.putVar trigger unit
    pure next
  HandleSearch m next → do
    salt ← H.liftEff newSalt
    st ← H.get
    value ← case m of
      Search.Cleared →
        pure Nothing
      Search.Submit → do
        H.query' CS.cpSearch unit $ H.request Search.GetValue
    H.liftEff $ Browser.setLocation $ browseURL value st.sort salt st.path
    pure next
  HandleLicenseProblem problem next → do
    when (isJust problem) $
      void $ H.query' CS.cpNotify unit $ H.action $ NC.UpdateRenderMode NC.Hidden
    H.modify (_ { licenseProblem = problem })
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
    showDialog (Dialog.Error message)
    pure next
  HandleSignInMessage message next → do
    when (message ≡ GlobalMenu.SignInSuccess) (H.liftEff Browser.reload)
    pure next
  HandleAdminUI message next → case message of
    AdminUI.Types.Closed → do
      _ ← queryHeaderGripper $ H.action $ Gripper.Close
      pure next

handleItemMessage ∷ Item.Message → DSL Unit
handleItemMessage = case _ of
  Item.Selected →
    pure unit
  Item.Edit res → do
    loc ← H.liftEff Browser.locationString
    for_ (preview R._Workspace res) \wp →
      H.liftEff $ Browser.setLocation $ append (loc ⊕ "/") $ mkWorkspaceURL wp (Load Editable)
  Item.Open res → do
    { sort, salt, path } ← H.get
    loc ← H.liftEff Browser.locationString
    for_ (preview R._filePath res) \fp →
      createWorkspace path \mkUrl →
        H.liftEff $ Browser.setLocation $ mkUrl $ Exploring fp
    for_ (preview R._dirPath res) \dp →
      H.liftEff $ Browser.setLocation $ browseURL Nothing sort salt dp
    for_ (preview R._Workspace res) \wp →
      H.liftEff $ Browser.setLocation $ append (loc ⊕ "/") $ mkWorkspaceURL wp (Load ReadOnly)
  Item.Configure (R.Mount mount) → do
    configure false mount
  Item.Configure _ →
    pure unit
  Item.Move res →
    showDialog (Dialog.Rename res)
  Item.Remove res →
    showDialog (Dialog.Delete res)
  Item.Share res → do
    path ← H.gets _.path
    loc ← map (_ ⊕ "/") $ H.liftEff Browser.locationString
    for_ (preview R._filePath res) \fp →
      createWorkspace path \mkUrl → do
        let url = append loc $ mkUrl $ Exploring fp
        showDialog (Dialog.Share { name: R.resourceName res, url })
    for_ (preview R._Workspace res) \wp → do
      let url = append loc $ mkWorkspaceURL wp (Load ReadOnly)
      showDialog (Dialog.Share { name: R.resourceName res, url })
  Item.Download res →
    showDialog (Dialog.Download res)

checkIsMount ∷ DirPath → DSL Unit
checkIsMount path = do
  isMount ← isRight <$> API.mountInfo (Left path)
  H.modify (_ { isMount = isMount })

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
  H.modify (_ { isUnconfigured = not isMount ∧ isEmpty })

dismissMountHint ∷ DSL Unit
dismissMountHint = do
  LS.persist J.encodeJson LSK.dismissedMountHintKey true
  H.modify (_ { presentMountHint = false })

dismissIntroVideo ∷ DSL Unit
dismissIntroVideo = do
  LS.persist J.encodeJson LSK.dismissedIntroVideoKey true
  H.modify (_ { presentIntroVideo = false })
  void $ H.query' CS.cpNotify unit $ H.action $ NC.UpdateRenderMode NC.Notifications

dismissedIntroVideoBefore ∷ DSL Boolean
dismissedIntroVideoBefore =
  either (const false) id <$> LS.retrieve J.decodeJson LSK.dismissedIntroVideoKey

uploadFileSelected ∷ DF.File → DSL Unit
uploadFileSelected f = do
  { path, sort, salt } ← H.get
  let fileName = RX.replace (unsafePartial fromRight $ RX.regex "/" RXF.global) ":" (DFF.name f)
  name ← API.getNewName path $ fromMaybe fileName $ S.stripSuffix (S.Pattern ".zip") fileName
  case name of
    Left err → handleError err
    Right uploadName → do
      case Array.last (S.split (S.Pattern ".") fileName) of
        Just "zip" →
          uploadDir path uploadName (DF.fileToBlob f)
        Just "csv" → do
          uploadFile path uploadName ∘ QD.QData (Right CSV.defaultOptions) =<< H.liftAff (UF.readAsText f)
        _ → do
          content ← H.liftAff $ UF.readAsText f
          let encoding = if isArrayEncoded content then Json.Array else Json.LineDelimited
          let options = Json.Options { precision: Json.Readable, encoding }
          uploadFile path uploadName $ QD.QData (Left options) content
  where

  bracketUpload ∷ R.Resource → DSL (Either QE.QError Unit) → DSL Unit
  bracketUpload res upload = do
    _ ← H.query' CS.cpListing unit $ H.action $ Listing.Add (PhantomItem res)
    uploadResult ← upload
    _ ← H.query' CS.cpListing unit $ H.action $ Listing.Filter (not ∘ eq res ∘ itemResource)
    case uploadResult of
      Left err → handleError err
      Right _ → void $ H.query' CS.cpListing unit $ H.action $ Listing.Add (Item res)

  uploadFile ∷ DirPath → String → QD.QData → DSL Unit
  uploadFile path name qdata = do
    let fileName = path </> file name
    bracketUpload (R.File fileName) (API.makeFile fileName qdata)

  uploadDir ∷ DirPath → String → DF.Blob → DSL Unit
  uploadDir path name zipdata = do
    let dirName = path </> dir name
    let mkRes = if isJust (S.stripSuffix (S.Pattern ".slam") name) then R.Workspace else R.Directory
    bracketUpload (mkRes dirName) (API.makeDir dirName zipdata)

  isArrayEncoded ∷ String → Boolean
  isArrayEncoded content'
    -- Parse if content is small enough
    | S.length content' < 1048576 = isRight $ J.jsonParser content'
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
    ∘ (flip (_ { presentMountHint = _ }))
    ∘ ((Array.null xs ∧ path ≡ rootDir ∧ not (isSearching ∧ isLoading)) ∧ _)
    ∘ not
    ∘ either (const false) id
    =<< dismissedBefore
  where
  dismissedBefore ∷ DSL (Either String Boolean)
  dismissedBefore =
    LS.retrieve J.decodeJson LSK.dismissedMountHintKey

dismissSignInSubmenu ∷ DSL Unit
dismissSignInSubmenu =
  void $ queryGlobalMenu (H.action GlobalMenu.DismissSubmenu)

resort ∷ DSL Unit
resort = do
  sort ← H.gets _.sort
  H.query' CS.cpSearch unit (H.request Search.IsSearching)
    >>= traverse_ \isSearching →
      void $ H.query' CS.cpListing unit $ H.action $ Listing.SortBy (sortItem isSearching sort)

configure ∷ Boolean → R.Mount → DSL Unit
configure fromRoot m = do
  let anyPath = R.mountPath m
  API.mountInfo anyPath >>= case m, _ of
    R.View path, Left err → raiseError err
    R.Module path, Left err → raiseError err
    R.Database path, Left err
      | path /= rootDir → raiseError err
      | otherwise → showMountDialog Mount.Root
    R.View path, Right mount →
      showMountDialog $ Mount.Edit { path: Right path, mount, fromRoot }
    R.Module path, Right mount →
      showMountDialog $ Mount.Edit { path: Left path, mount, fromRoot }
    R.Database path, Right mount →
      showMountDialog $ Mount.Edit { path: Left path, mount, fromRoot }
  where
    raiseError ∷ QE.QError → DSL Unit
    raiseError err = case GE.fromQError err of
      Left msg →
        showDialog $ Dialog.Error
          $ "There was a problem reading the mount settings: " <> msg
      Right ge →
        GE.raiseGlobalError ge
    showMountDialog ∷ Mount.Input → DSL Unit
    showMountDialog = showDialog ∘ Dialog.Mount

handleDeleted ∷ AnyPath → DSL Unit
handleDeleted deletedPath = do
  { path } ← H.get
  case deletedPath of
    Left dir
      | dir == rootDir → H.liftEff $ Browser.reload
      | dir == path → H.liftEff $ Browser.setLocation $ parentURL deletedPath
    _ → repopulate

repopulate ∷ DSL Unit
repopulate = do
  salt ← H.liftEff newSalt
  { sort, path } ← H.get
  searchValue ← H.query' CS.cpSearch unit (H.request Search.GetValue)
  H.liftEff $ Browser.setLocation $ browseURL searchValue sort salt path

showDialog ∷ Dialog.Definition → DSL Unit
showDialog d = H.modify (_ { dialog = Just d })

queryHeaderGripper ∷ ∀ a. Gripper.Query a → DSL (Maybe a)
queryHeaderGripper =
   H.query' CS.cpHeader unit ∘ Header.QueryGripper ∘ liftCoyoneda

queryGlobalMenu ∷ ∀ a. GlobalMenu.Query a → DSL (Maybe a)
queryGlobalMenu =
   H.query' CS.cpHeader unit ∘ Header.QueryGlobalMenu ∘ liftCoyoneda

createWorkspace ∷ ∀ a. DirPath → ((Action → String) → DSL a) → DSL Unit
createWorkspace path action = do
  let newWorkspaceName = Config.newWorkspaceName ⊕ "." ⊕ Config.workspaceExtension
  name ← API.getNewName path newWorkspaceName
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
    Right name' →
      void $ action (mkWorkspaceURL (path </> dir name'))
