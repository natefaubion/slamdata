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

module SlamData.Workspace.Dialog.Unshare.Component where

import SlamData.Prelude

import Clipboard as C
import Control.Monad.Aff.AVar (makeVar, takeVar, putVar)
import Control.Monad.Eff.Ref (newRef, modifyRef, readRef)
import Control.Monad.Fork.Class (fork)
import Control.UI.Browser (select)
import DOM.Classy.Element (toElement)
import DOM.HTML.Types (readHTMLElement)
import Data.Array as Arr
import Data.Foldable as F
import Data.Foreign (toForeign)
import Data.Lens (Lens', lens, (.~), (%~), (?~))
import Data.Lens.Index (ix)
import Data.List as L
import Data.Map as Map
import Data.Set as Set
import Data.StrMap as SM
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Elements.Keyed as HK
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Quasar.Advanced.Types as QTA
import SlamData.Monad (Slam)
import SlamData.Quasar.Security as Q
import SlamData.Render.ClassName as CN
import SlamData.Render.Common as RC
import SlamData.Render.Icon as I
import SlamData.Workspace.Dialog.Share.Model (ShareResume(..), printShareResume)
import SlamData.Workspace.Dialog.Share.Model as Model
import Utils.DOM as DOM
import Utils.Foldable (chunkedParTraverse, splitList)

type HTML = H.ComponentHTML Query
type DSL = H.ComponentDSL State Query Message Slam

type PermissionId = Set.Set QTA.PermissionId

data PermissionState
  = Unsharing
  | Modifying
  | RevokeError
  | ModifyError

derive instance permissionStateEq ∷ Eq PermissionState

type Permission =
  { resume ∷ ShareResume
  , actions ∷ Map.Map QTA.PermissionId QTA.ActionR
  , state ∷ Maybe PermissionState
  }

data Modifyable = User | Group

type TokenPermission =
  { name ∷ Maybe String
  , secret ∷ String
  , resume ∷ ShareResume
  , tokenId ∷ QTA.TokenId
  , state ∷ Maybe PermissionState
  }

_resume ∷ ∀ a r. Lens' {resume ∷ a|r} a
_resume = lens (_.resume) (_{resume = _})

_tokenId ∷ ∀ a r. Lens' {tokenId ∷ a|r} a
_tokenId = lens (_.tokenId) (_{tokenId = _})

_secret ∷ ∀ a r. Lens' {secret ∷ a|r} a
_secret = lens (_.secret) (_{secret = _})

-- :( `ix` doesn't work without `Maybe PermissionState`
_state ∷ ∀ r. Lens' {state ∷ (Maybe PermissionState)|r} (Maybe PermissionState)
_state = lens (_.state) (_{state = _})

_actions
  ∷ ∀ r
  . Lens'
      {actions ∷ Map.Map QTA.PermissionId QTA.ActionR|r}
      (Map.Map QTA.PermissionId QTA.ActionR)
_actions = lens (_.actions) (_{actions = _})

type State =
  { userPermissions ∷ SM.StrMap Permission
  , tokenPermissions ∷ Array TokenPermission
  , groupPermissions ∷ SM.StrMap Permission
  , loading ∷ Boolean
  , errored ∷ Boolean
  , sharingInput ∷ Model.SharingInput
  }

initialState ∷ Model.SharingInput → State
initialState sharingInput =
  { userPermissions: SM.empty
  , tokenPermissions: [ ]
  , groupPermissions: SM.empty
  , loading: true
  , errored: false
  , sharingInput
  }

_userPermissions ∷ ∀ a r. Lens' {userPermissions ∷ a|r} a
_userPermissions = lens (_.userPermissions) (_{userPermissions = _})

_groupPermissions ∷ ∀ a r. Lens' {groupPermissions ∷ a |r} a
_groupPermissions = lens (_.groupPermissions) (_{groupPermissions = _})

_tokenPermissions ∷ ∀ a r. Lens' {tokenPermissions ∷ a|r} a
_tokenPermissions = lens (_.tokenPermissions) (_{tokenPermissions = _})

_loading ∷ ∀ a r. Lens' {loading ∷ a|r} a
_loading = lens (_.loading) (_{loading = _})

_errored ∷ ∀ a r. Lens' {errored ∷ a|r} a
_errored = lens (_.errored) (_{errored = _})

data Query a
  = Init a
  | SelectElement DOM.Event a
  | ChangePermissionResume String String a
  | Unshare String a
  | UnshareToken QTA.TokenId a
  | PreventDefault DOM.Event a
  | Done a

data Message = Dismiss

type AdjustedPermissions =
  { users ∷ SM.StrMap Permission
  , groups ∷ SM.StrMap Permission
  }

copyButtonRef ∷ QTA.TokenId → H.RefLabel
copyButtonRef tokenId = H.RefLabel $ "copy" <> QTA.runTokenId tokenId

component ∷ H.Component HH.HTML Query Model.SharingInput Message Slam
component =
  H.lifecycleComponent
    { render
    , eval
    , initialState
    , initializer: Just (H.action Init)
    , finalizer: Nothing
    , receiver: const Nothing
    }

render ∷ State → HTML
render state =
  HH.div
    [ HP.classes [ HH.ClassName "deck-dialog-unshare" ] ]
    [ HH.h4_ [ HH.text "Unshare deck" ]
    , HH.div
        [ HP.classes
            $ [ CN.alert, CN.alertInfo, HH.ClassName "share-loading" ]
            ⊕ if state.loading then [ ] else [ CN.hidden ]
        ]
        [ RC.spinner
        , HH.text "Loading"
        ]

    , HH.div
        [ HP.classes
            $ [ HH.ClassName "deck-dialog-body" ]
            ⊕ (if SM.isEmpty state.userPermissions
                  ∧ SM.isEmpty state.groupPermissions
                  ∧ Arr.null state.tokenPermissions then [ CN.hidden ] else [ ])
        ]
        [ HH.form
            [ HE.onSubmit $ HE.input PreventDefault ]
            $ (if SM.isEmpty state.userPermissions
                 then
                   [ ]
                 else
                   [ HH.h5_ [ HH.text "Users" ] ]
                   ⊕ (foldMap renderUserOrGroup $ asList $ SM.toUnfoldable state.userPermissions)
              )
            ⊕ (if SM.isEmpty state.groupPermissions
                 then
                   [ ]
                 else
                   [ HH.h5_ [ HH.text "Groups" ] ]
                   ⊕ (foldMap renderUserOrGroup $ asList $ SM.toUnfoldable state.groupPermissions)
              )
            ⊕ (if Arr.null state.tokenPermissions
                 then
                   [ ]
                 else
                   [ HH.h5_ [ HH.text "Tokens" ]
                   , HK.div_ $ state.tokenPermissions <#> \token →
                      QTA.runTokenId token.tokenId × renderToken token
                   ]
              )
            ⊕ (if SM.isEmpty state.userPermissions
                  ∧ SM.isEmpty state.groupPermissions
                  ∧ Arr.null state.tokenPermissions
                  ∧ not state.errored
                 then
                   [ HH.div_
                       [ HH.text "This deck hasn't been shared with anyone." ]
                   ]
                 else [ ]
              )
        ]
    , let
        states =
          (foldMap (\x → [x.state]) state.userPermissions)
          ⊕ (foldMap (\x → [x.state]) state.groupPermissions)
          ⊕ (foldMap (\x → [x.state]) state.tokenPermissions)
        somethingErrored =
          F.any (\x → x ≡ Just ModifyError ∨ x ≡ Just RevokeError) states
        somethingHappening =
          F.any (\x → x ≡ Just Unsharing ∨ x ≡ Just Modifying) states
      in
       HH.div
        [ HP.classes
            $ [ HH.ClassName "deck-dialog-footer share" ]
            ⊕ (if state.loading then [ CN.hidden ] else [ ])
        ]
        [ HH.div
            [ HP.classes
                $ [ CN.alert, CN.alertDanger ]
                ⊕ (if state.errored ∨ somethingErrored then [ ] else [ CN.hidden ])
            ]
            [ HH.text
                $ if state.errored
                  then
                    "Sharing information is unavailable. To access or change the "
                    ⊕ "sharing information for this deck please check your network connection "
                    ⊕ "and try again."

                  else
                    "This action couldn't be performed. "
                    ⊕ "Please check your network connection and try again."
            ]
        , HH.button
            [ HE.onClick (HE.input_ Done)
            , HP.type_ HP.ButtonButton
            , HP.classes [ CN.btn, CN.btnDefault ]
            , HP.disabled somethingHappening
            ]
            [ HH.text "Done" ]
        ]

    ]

renderUserOrGroup ∷ (String × Permission) → Array HTML
renderUserOrGroup (name × perm) =
  [ HH.div
      [ HP.class_ $ HH.ClassName "sd-unshare-subject" ]
      [ HH.label_ [ HH.text name ]
      , HH.div
          [ HP.classes $ if perm.state ≡ Just ModifyError then [ CN.hasError ] else [ ] ]
          [ HH.select
              [ HP.classes [ CN.formControl ]
              , HE.onValueChange (HE.input (ChangePermissionResume name))
              , HP.disabled $ perm.state ≡ Just Modifying ∨ perm.state ≡ Just Unsharing
              ]
              [ HH.option
                  [ HP.value "view"
                  , HP.selected $ perm.resume ≡ View
                  ]
                  [ HH.text
                      $ "View"
                      ⊕ if perm.state ≡ Just Modifying
                          then "..."
                          else ""
                  ]
              , HH.option
                  [ HP.value "edit"
                  , HP.selected $ perm.resume ≡ Edit
                  ]
                  [ HH.text
                      $ "Edit"
                      ⊕ if perm.state ≡ Just Modifying
                          then "..."
                          else ""
                  ]
              ]
          ]
      , HH.div
          [ HP.classes $ if perm.state ≡ Just RevokeError then [ CN.hasError ] else [ ] ]
          [ HH.button
              [ HP.classes [ CN.btn, CN.btnPrimary ]
              , HE.onClick (HE.input_ (Unshare name))
              , HP.disabled $ perm.state ≡ Just Modifying ∨ perm.state ≡ Just Unsharing
              ]
              [ HH.text if perm.state ≡ Just Unsharing
                          then "Revoking..."
                          else "Unshare"
              ]
          ]
      ]
  ]

renderToken ∷ TokenPermission → HTML
renderToken token =
  HH.div
    [ HP.class_ $ HH.ClassName "sd-unshare-subject" ]
    [ HH.label_
        [ HH.text $ fromMaybe "Untitled token" token.name
        ]
    , HH.span
        [ HP.classes [ HH.ClassName "sd-unshare-subject-access-type" ] ]
        [ HH.text $ printShareResume token.resume ]
    , HH.div
        [ HP.classes
            $ [ HH.ClassName "sd-url" ]
            ⊕ if token.state ≡ Just ModifyError then [ CN.hasError ] else [ ]
        ]
        [ HH.input
            [ HP.readOnly true
            , HP.title "Token secret"
            , HP.value token.secret
            , HE.onClick $ HE.input (SelectElement ∘ DOM.toEvent)
            , HP.disabled $ isJust token.state
        ]
        , HH.button
            [ HP.classes [ CN.btn, CN.btnDefault ]
            , HP.title "Copy to clipboard"
            , HP.ref $ copyButtonRef token.tokenId
            , HP.disabled $ token.state ≡ Just Modifying ∨ token.state ≡ Just Unsharing
            ]
            [ I.copySm ]
        ]
    , HH.div
        [ HP.classes $ if token.state ≡ Just RevokeError then [ CN.hasError ] else [ ] ]
        [ HH.div
            [ HP.class_ CN.btnGroup ]
            [ HH.button
                [ HP.classes [ CN.btn, CN.btnPrimary ]
                , HE.onClick (HE.input_ (UnshareToken token.tokenId))
                , HP.disabled $ token.state ≡ Just Modifying ∨ token.state ≡ Just Unsharing
                ]
                [ HH.text if token.state ≡ Just Unsharing
                            then "Revoking..."
                            else "Unshare"
                ]
            ]
        ]
    ]


eval ∷ Query ~> DSL
eval = case _ of
  Init next → do
    _ ← fork do
      sharingInput ← H.gets _.sharingInput
      Q.tokenList >>= case _ of
        Left e →
          H.modify _ { errored = true }
        Right toks → do
          let tokenPerms = prepareAndFilterTokens toks sharingInput
          H.modify _ { tokenPermissions = tokenPerms }
          for_ tokenPerms initClipboard
      Q.permissionList false >>= case _ of
        Left e →
          H.modify _
            { errored = true
            , loading = false
            }
        Right ps → do
          let adjusted = adjustPermissions ps sharingInput
          H.modify _
            { userPermissions = adjusted.users
            , groupPermissions = adjusted.groups
            , loading = false
            }
    pure next

  ChangePermissionResume name string next → do
    state ← H.get
    H.modify (_{errored = false})
    let
      mbUserPerms = SM.lookup name state.userPermissions
      mbGroupPerms = SM.lookup name state.groupPermissions
      newResume = case string of
        "view" → View
        _ → Edit
      oldResume = case string of
        "view" → Edit
        _ → View

    for_ mbUserPerms \perms → do
      H.modify
        $ (_userPermissions ∘ ix name ∘ _state ?~ Modifying)
        ∘ (_userPermissions ∘ ix name ∘ _resume .~ newResume)
      (succeeded × actionMap) ←
        changePermissionResumeForUser name state.sharingInput newResume perms
      H.modify
        $ (_userPermissions ∘ ix name ∘ _actions .~ actionMap)
        ∘ if succeeded
          then
            (_userPermissions ∘ ix name ∘ _state .~ Nothing)
          else
            (_userPermissions ∘ ix name ∘ _state ?~ ModifyError)
            ∘ (_userPermissions ∘ ix name ∘ _resume .~ oldResume)

    for_ mbGroupPerms \perms → do
      H.modify
        $ (_groupPermissions ∘ ix name ∘ _state ?~ Modifying)
        ∘ (_groupPermissions ∘ ix name ∘ _resume .~ newResume)

      (succeeded × actionMap)  ←
        changePermissionResumeForGroup name state.sharingInput newResume perms
      H.modify
        $ (_groupPermissions ∘ ix name ∘ _actions .~ actionMap)
        ∘ if succeeded
          then
            (_groupPermissions ∘ ix name ∘ _state .~ Nothing)
          else
            (_groupPermissions ∘ ix name ∘ _state ?~ ModifyError)
            ∘ (_groupPermissions ∘ ix name ∘ _resume .~ oldResume)
    pure next
  UnshareToken tokenId next → do
    state ← H.get
    let mtix = Arr.findIndex (\x → x.tokenId ≡ tokenId) state.tokenPermissions
    for_ mtix \tix → do
      H.modify $ _tokenPermissions ∘ ix tix ∘ _state ?~ Unsharing
      Q.deleteToken tokenId >>= case _ of
        Left _ →
          H.modify
            $ (_tokenPermissions ∘ ix tix ∘ _state ?~ RevokeError)
            ∘ (_errored .~ false)
        Right _ →
          H.modify
            $ _tokenPermissions %~ Arr.filter (\x → x.tokenId ≠ tokenId)
    pure next
  Unshare name next → do
    state ← H.get
    H.modify _ { errored = false }
    let
      mbUserPerms = SM.lookup name state.userPermissions
      mbGroupPerms = SM.lookup name state.groupPermissions

    for_ mbUserPerms \userPerms → do
      H.modify $ _userPermissions ∘ ix name ∘ _state ?~ Unsharing
      leftPids ← deletePermission userPerms.actions
      if Map.isEmpty leftPids
        then
          H.modify
            $ (_userPermissions %~ SM.delete name)
            ∘ (_userPermissions ∘ ix name ∘ _state .~ Nothing)
        else
          H.modify
            $ (_userPermissions ∘ ix name ∘ _actions .~ leftPids)
            ∘ (_userPermissions ∘ ix name ∘ _state ?~ RevokeError)

    for_ mbGroupPerms \groupPerms → do
      H.modify $ _groupPermissions ∘ ix name ∘ _state ?~ Unsharing
      leftPids ← deletePermission groupPerms.actions
      if Map.isEmpty leftPids
        then
          H.modify
            $ (_groupPermissions ∘ ix name ∘ _state .~ Nothing)
            ∘ (_groupPermissions %~ SM.delete name)
        else
          H.modify
            $ (_groupPermissions ∘ ix name ∘ _actions .~ leftPids)
            ∘ (_groupPermissions ∘ ix name ∘ _state ?~ RevokeError)
    pure next
  SelectElement ev next → do
    H.liftEff $ DOM.currentTarget ev
      # readHTMLElement ∘ toForeign
      # runExcept
      # traverse_ select
    pure next
  PreventDefault ev next →
    H.liftEff (DOM.preventDefault ev) $> next
  Done next →
    H.raise Dismiss $> next

initClipboard ∷ TokenPermission → DSL Unit
initClipboard token =
  H.getHTMLElementRef (copyButtonRef token.tokenId) >>= traverse_ \htmlEl →
    H.liftEff $ C.fromElement (toElement htmlEl) (pure token.secret)

changePermissionResumeForUser
  ∷ String
  → Model.SharingInput
  → ShareResume
  → Permission
  → DSL (Boolean × (Map.Map QTA.PermissionId QTA.ActionR))
changePermissionResumeForUser name sharingInput res perm = do
  leftPids ← deletePermission perm.actions
  if not $ Map.isEmpty leftPids
    then
    pure $ false × leftPids
    else do
    let
      actions = Model.sharingActions sharingInput res
      shareRequest =
        { users: [ QTA.UserId name ]
        , groups: [ ]
        , actions
        }
    shareRes ← Q.sharePermission shareRequest
    case shareRes of
      Left _ → pure $ false × Map.empty
      Right ps → pure $ true × foldMap (\p → Map.singleton p.id p.action) ps


changePermissionResumeForGroup
  ∷ String
  → Model.SharingInput
  → ShareResume
  → Permission
  → DSL (Boolean × (Map.Map QTA.PermissionId QTA.ActionR))
changePermissionResumeForGroup name sharingInput res perm =
  case hush (QTA.parseGroupPath name) of
    Nothing → pure $ false × perm.actions
    Just groupPath → do
      leftPids ← deletePermission perm.actions
      if not $ Map.isEmpty leftPids
        then
        pure $ false × leftPids
        else do
        let
          actions = Model.sharingActions sharingInput res
          shareRequest =
            { users: [ ]
            , groups: [ groupPath ]
            , actions
            }
        shareRes ← Q.sharePermission shareRequest
        case shareRes of
          Left _ → pure $ false × Map.empty
          Right ps → pure $ true × foldMap (\p → Map.singleton p.id p.action) ps

deletePermission
  ∷ Map.Map QTA.PermissionId QTA.ActionR
  → DSL (Map.Map QTA.PermissionId QTA.ActionR)
deletePermission permissionMap = do
  r ← H.liftEff $ newRef $ Map.empty × Map.size permissionMap
  resultVar ← H.liftAff makeVar
  _ ← H.lift $ chunkedParTraverse (go r resultVar) (Map.toUnfoldable permissionMap) (splitList 20) L.concat
  H.liftAff $ takeVar resultVar
  where
  go r resultVar (pid × act) = do
    result ← Q.deletePermission pid
    H.liftEff $ modifyRef r \(acc × count) → acc × (count - 1)
    case result of
      Left _ → H.liftEff $ modifyRef r \(acc × count) → (Map.insert pid act acc) × count
      Right _ → pure unit
    (acc × count) ← H.liftEff $ readRef r
    when (count ≡ 0) $ H.liftAff $ putVar resultVar acc


adjustPermissions ∷ Array QTA.PermissionR → Model.SharingInput → AdjustedPermissions
adjustPermissions prs sharingInput =
  let
    folded = foldl foldFn Map.empty prs
    foldFn mp pr =
      Map.alter (alterFn pr) pr.grantedTo mp

    alterFn pr Nothing = Just [ pr ]
    alterFn pr (Just arr) = Just $ Arr.cons pr arr

    sharingActionsEdit =
      Set.fromFoldable $ map QTA.Action $ Model.sharingActions sharingInput Edit
    sharingActionsView =
      Set.fromFoldable $ map QTA.Action $ Model.sharingActions sharingInput View


    obj = foldl objFoldFn { users: SM.empty, groups: SM.empty } $ asList $ Map.toUnfoldable folded

    -- Set.subset is sooooo slow :( @crygoenian
    subset ∷ ∀ a. Ord a ⇒ Set.Set a → Set.Set a → Boolean
    subset needle hay =
      let
        needleLst = L.sort $ L.fromFoldable needle
        hayLst = L.sort $ L.fromFoldable hay

        subset' ∷ L.List a → L.List a → Boolean
        subset' L.Nil _ = true
        subset' _ L.Nil = false
        subset' needle'@(L.Cons x xs) (L.Cons y ys) =
          if x ≡ y
          then subset' xs ys
          else subset' needle' ys
      in subset' needleLst hayLst


    objFoldFn acc@{ users, groups } (key × arr) =
      let
        actions = foldMap (\a → Map.singleton a.id a.action) arr
        actionsSet = Set.fromFoldable $ map QTA.Action actions
        perm = { resume: View
               , actions
               , state: Nothing
               }
      in case key of
        QTA.TokenGranted _ → acc
        QTA.UserGranted (QTA.UserId uid) →
          let
            res
              | subset sharingActionsEdit actionsSet =
                acc{users = SM.insert uid perm{resume = Edit} users }
              | subset sharingActionsView actionsSet =
                acc{users = SM.insert uid perm users}
              | otherwise = acc
          in res
        QTA.GroupGranted groupPath →
          let
            gid = QTA.printGroupPath groupPath
            res
              | subset sharingActionsEdit actionsSet =
                acc{groups = SM.insert gid perm{resume = Edit} groups }
              | subset sharingActionsView actionsSet =
                acc{groups = SM.insert gid perm groups}
              | otherwise = acc
          in res
  in obj


prepareAndFilterTokens ∷ Array QTA.TokenR → Model.SharingInput →  Array TokenPermission
prepareAndFilterTokens toks sharingInput =
  foldMap foldFn toks
  where
  foldFn ∷ QTA.TokenR → Array TokenPermission
  foldFn tr =
    let
      sharingActionsEdit =
        Set.fromFoldable $ map QTA.Action $ Model.sharingActions sharingInput Edit
      sharingActionsView =
        Set.fromFoldable $ map QTA.Action $ Model.sharingActions sharingInput View
      actions =
        Set.fromFoldable $ map QTA.Action tr.actions
      preToken =
        { name: map QTA.runTokenName tr.name
        , secret: QTA.runTokenHash tr.secret
        , tokenId: tr.id
        , state: Nothing
        , resume: View
        }
      res
        | sharingActionsEdit ≡ actions =
          [ preToken{resume = Edit} ]
        | sharingActionsView ≡ actions =
          [ preToken ]
        | otherwise =
            [ ]
    in
      res
