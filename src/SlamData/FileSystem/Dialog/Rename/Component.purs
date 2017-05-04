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

module SlamData.FileSystem.Dialog.Rename.Component where

import SlamData.Prelude

import Control.UI.Browser (reload)

import Data.Array (elemIndex, singleton, sort, nub)
import Data.Lens ((^.), (%~), (.~), (?~), lens, Lens')
import Data.Path.Pathy (printPath, parseAbsDir, sandbox, rootDir, (</>))
import Data.String as S

import DOM.Event.Event as DEE

import Halogen as H
import Halogen.HTML.Events as HE
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Halogen.HTML.Properties.ARIA as ARIA
import Halogen.Themes.Bootstrap3 as B

import SlamData.Config as Config
import SlamData.Dialog.Render (modalDialog, modalHeader, modalBody, modalFooter)
import SlamData.FileSystem.Dialog.Component.Message (Message(..))
import SlamData.FileSystem.Listing.Item.Component.CSS as ItemCSS
import SlamData.FileSystem.Resource as R
import SlamData.GlobalError as GE
import SlamData.Monad (Slam)
import SlamData.Quasar.FS as API
import SlamData.Render.Common (formGroup)
import SlamData.Render.CSS as Rc

import Utils.Path (DirPath, dropWorkspaceExt)
import Utils.DOM as DOM

type State =
  { showList ∷ Boolean
  , initial ∷ R.Resource
  , name ∷ String
  , dirs ∷ Array R.Resource
  , dir ∷ DirPath
  , typedDir ∷ String
  , siblings ∷ Array R.Resource
  , error ∷ Maybe String
  }

initialState ∷ R.Resource → State
initialState resource =
  { showList: false
  , initial: resource
  , name: if R.isWorkspace resource
          then dropWorkspaceExt $ R.resourceName resource
          else R.resourceName resource
  , dir: R.resourceDir resource
  , typedDir: printPath $ R.resourceDir resource
  , siblings: mempty
  , dirs: singleton R.root
  , error: Nothing
  }

_showList ∷ Lens' State Boolean
_showList = lens _.showList (_ { showList = _ })

_initial ∷ Lens' State R.Resource
_initial = lens _.initial (_ { initial = _ })

_name ∷ Lens' State String
_name = lens _.name (_ { name = _ })

_typedDir ∷ Lens' State String
_typedDir = lens _.typedDir (_ { typedDir = _ })

_dirs ∷ Lens' State (Array R.Resource)
_dirs = lens _.dirs (_ { dirs = _ })

_dir ∷ Lens' State DirPath
_dir = lens _.dir (_ { dir = _ })

_siblings ∷ Lens' State (Array R.Resource)
_siblings = lens _.siblings (_ { siblings = _ })

_error ∷ Lens' State (Maybe String)
_error = lens _.error (_ { error = _ })

renameSlam ∷ State → R.Resource
renameSlam r =
  let initial = r.initial
      name = r.name
      nameWithExt = if R.isWorkspace initial
                    then name <> "." <> Config.workspaceExtension
                    else name
  in initial # (R._name .~ nameWithExt)
           <<< (R._root .~ r.dir)

validate ∷ State → State
validate r
  | r.initial == renameSlam r = r # _error .~ Nothing
  | otherwise = r # _error .~ either Just (const Nothing) do
    let name = r.name
    when (name == "")
      $ throwError "Please enter a name for the file"

    when (isJust $ S.stripSuffix (S.Pattern $ "." <> Config.workspaceExtension) name)
      $ throwError $ "Please choose an alternative name, ."
      <> Config.workspaceExtension
      <> " is a reserved extension"

    when (isJust $ S.indexOf (S.Pattern "/") name)
      $ throwError "Please enter a valid name for the file"

    let nameWithExt = if R.isWorkspace (r.initial)
                      then name <> "." <> Config.workspaceExtension
                      else name
    when (isJust $ elemIndex nameWithExt (map (_ ^. R._name) (r.siblings)))
      $ throwError "An item with this name already exists in the target folder"

data Query a
  = RaiseDismiss a
  | SetShowList Boolean a
  | ToggleShowList a
  | NameTyped String a
  | DirTyped String a
  | DirClicked R.Resource a
  | SetSiblings (Array R.Resource) a
  | AddDirs (Array R.Resource) a
  | PreventDefaultAndRename DOM.Event a
  | StopPropagation DOM.Event (Query a)
  | Init a

type DSL = H.ComponentDSL State Query Message Slam
type HTML = H.ComponentHTML Query

component ∷ H.Component HH.HTML Query R.Resource Message Slam
component =
  H.lifecycleComponent
    { initialState
    , render
    , eval
    , initializer: Just (H.action Init)
    , finalizer: Nothing
    , receiver: const Nothing
    }

render ∷ State → HTML
render dialog =
  HH.form
    [ HE.onSubmit $ HE.input PreventDefaultAndRename ]
    [ modalDialog
      [ modalHeader "Move/rename"
      , modalBody
        $ HH.div
            [ HP.classes [ Rc.renameDialogForm ]
            , HE.onClick \e → Just $ StopPropagation (DOM.toEvent e) $ H.action $ SetShowList false
            ]
            [ nameInput
            , dirDropdownField
            , dirDropdownList
            , errorMessage
            ]
      , modalFooter
          [ HH.button
              [ HP.type_ HP.ButtonButton
              , HP.classes [ B.btn ]
              , HE.onClick (HE.input_ RaiseDismiss)
              ]
              [ HH.text "Cancel" ]
          , HH.button
              [ HP.classes [ B.btn, B.btnPrimary ]
              , HP.disabled $ isJust $ dialog.error
              ]
              [ HH.text "Rename" ]
          ]
      ]
    ]
  where
  nameInput ∷ HTML
  nameInput =
    formGroup [ HH.input [ HP.classes [ B.formControl ]
                        , HP.value (dialog.name)
                        , HP.placeholder "New name"
                        , HE.onValueInput (HE.input NameTyped)
                        ]
              ]

  dirDropdownField ∷ HTML
  dirDropdownField =
    HH.div
      [ HP.classes [ B.inputGroup, HH.ClassName "file-list-field" ] ]
      [ HH.input
          [ HP.type_ HP.InputText
          , HP.classes [ B.formControl ]
          , HP.placeholder "New directory"
          , HE.onValueInput (HE.input DirTyped)
          , HP.value $ dialog ^. _typedDir
          ]
      , HH.span
          [ HP.classes [ B.inputGroupBtn ] ]
          [ HH.button
              [ HP.type_ HP.ButtonButton
              , HP.classes [ B.btn, B.btnDefault ]
              , HE.onClick \e →
                   Just $ StopPropagation (DOM.toEvent e) $ H.action $ ToggleShowList
              , ARIA.label "Select a destination folder"
              , HP.title "Select a destination folder"
              ]
              [ HH.span [ HP.classes [ B.caret ] ] [ ] ]
          ]
      ]
  dirDropdownList ∷ HTML
  dirDropdownList =
    HH.ul
      ([ HP.classes  [ B.listGroup, Rc.fileListGroup ] ]
       <> if dialog.showList then [] else [ ARIA.hidden "true" ])
    $ renameItem <$> dialog.dirs

  errorMessage ∷ HTML
  errorMessage =
    HH.div
      ([ HP.classes $ [ B.alert, B.alertDanger ] ]
       <> if isJust dialog.error then [] else [ ARIA.hidden "true" ])
      $ maybe [ ] (pure <<< HH.text) (dialog.error)

  renameItem ∷ R.Resource → HTML
  renameItem res =
    HH.button
      [ HP.classes ([ B.listGroupItem ] <> (guard (R.isHidden res) $> ItemCSS.itemHidden))
      , HE.onClick (HE.input_ (DirClicked res))
      , HP.type_ HP.ButtonButton
      ]
      [ HH.text (R.resourcePath res) ]

eval ∷ Query ~> DSL
eval (RaiseDismiss next) = do
  H.raise Dismiss
  pure next
eval (StopPropagation e q) = do
  H.liftEff $ DEE.stopPropagation e
  eval q
eval (SetShowList bool next) = do
  H.modify (_showList .~ bool)
  H.modify validate
  pure next
eval (ToggleShowList next) = do
  H.modify (_showList %~ not)
  H.modify validate
  pure next
eval (PreventDefaultAndRename ev next) = do
  H.liftEff $ DEE.preventDefault ev
  dirStr <- endingInSlash <$> H.gets _.typedDir
  maybe presentDirNotExistError moveIfDirAccessible (parsedDir dirStr)
  pure next
  where
  parsedDir =
    map (rootDir </> _) ∘ sandbox rootDir <=< parseAbsDir

  presentSourceMissingError =
    H.modify $ _error .~ Just "The file you are trying to move is unavailable, please refresh."

  presentDirNotExistError =
    H.modify $ _error .~ Just "Target directory does not exist."

  presentError e =
    case GE.fromQError e of
      Left msg → H.modify $ _error .~ Just msg
      Right ge → GE.raiseGlobalError ge

  moveIfDirAccessible dir =
    maybe (move dir) presentError =<< API.dirNotAccessible dir

  move dir = do
    H.modify $ (_dir .~ dir) <<< (_showList .~ false)
    state ← H.get
    let src = state.initial
        tgt = R.getPath $ renameSlam state
    result ← H.lift $ API.move src tgt
    case result of
      Left e ->
        case GE.fromQError e of
          Left msg -> H.modify (_error ?~ msg)
          Right ge -> GE.raiseGlobalError ge
      Right x ->
        maybe
          presentSourceMissingError
          (const $ H.modify (_error .~ Nothing) *> H.liftEff reload)
          x
    pure unit

  lastChar s = S.drop (S.length s - 1) s

  endingInSlash s = if lastChar s == "/" then s else s <> "/"

eval (NameTyped str next) = do
  H.modify (_name .~ str)
  H.modify validate
  pure next
eval (DirTyped str next) = do
  H.modify $ _typedDir .~ str
  pure next
eval (DirClicked res next) = do
  dirItemClicked res
  pure next
eval (SetSiblings ss next) = do
  H.modify (_siblings .~ ss)
  pure next
eval (AddDirs ds next) = do
  H.modify (_dirs %~ append ds >>> nub >>> sort)
  pure next
eval (Init next) = do
  state <- H.get
  dirItemClicked $ R.parent $ state.initial
  pure next

dirItemClicked ∷ R.Resource → DSL Unit
dirItemClicked res = do
  case R.getPath res of
    Right _ → pure unit
    Left dir → do
      siblings ← API.children dir
      H.modify
        $ (_dir .~ dir)
        <<< (_showList .~ false)
        <<< (_siblings .~ either (const []) id siblings)
        <<< (_typedDir .~ printPath dir)
