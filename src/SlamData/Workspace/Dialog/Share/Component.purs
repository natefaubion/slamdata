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

module SlamData.Workspace.Dialog.Share.Component where

import SlamData.Prelude

import Clipboard as C
import Control.UI.Browser (select)
import Data.Array as Arr
import Data.Foreign (toForeign)
import Data.Path.Pathy (rootDir, (</>), file)
import Data.Path.Pathy as Pt
import Data.String as Str
import DOM.Classy.Element (toElement)
import DOM.HTML.Types (readHTMLElement)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.Themes.Bootstrap3 as B
import Quasar.Advanced.Types as QT
import SlamData.Monad (Slam)
import SlamData.Quasar.Security as Q
import SlamData.Render.Icon as I
import SlamData.Workspace.Dialog.Share.Model (ShareResume(..), sharingActions, SharingInput)
import Utils.DOM as DOM
import Utils.Path (rootFile, FilePath)

type HTML = H.ComponentHTML Query
type DSL = H.ComponentDSL State Query Message Slam

data SubjectType
  = User
  | Group
  | Token

derive instance subjectTypeEq ∷ Eq SubjectType

data ErrorType
  = Connection
  | Validation
  | GroupList

derive instance errorTypeEq ∷ Eq ErrorType

-- This is utility function that should be used only in
-- select/option stuff
readSubjectType ∷ String → SubjectType
readSubjectType = case _ of
  "user" → User
  "group" → Group
  "token" → Token
  _ → User

-- Same with `readSubjectType`
readShareResume ∷ String → ShareResume
readShareResume = case _ of
  "edit" → Edit
  _ → View

printShareResume ∷ ShareResume → String
printShareResume = case _ of
  Edit → "Edit"
  View → "View"

-- It would be better if all this Boolean stuff was replaced with
-- some kind of ADT abstracting automata.
type State =
  { subjectType ∷ SubjectType
  , error ∷ Maybe ErrorType
  , email ∷ String
    -- Actually should be Group from purescript-quasar
  , groups ∷ Array FilePath
  , groupSelected ∷ Maybe FilePath
  , tokenName ∷ String
  , loading ∷ Boolean
  , showError ∷ Boolean
  , submitting ∷ Boolean
  , shareResume ∷ ShareResume
  , tokenSecret ∷ Maybe String
  , sharingInput ∷ SharingInput
  }

initialState ∷ SharingInput → State
initialState sharingInput =
  { subjectType: User
  , error: Nothing
  , email: ""
  , groups: [ ]
  , groupSelected: Nothing
  , tokenName: ""
  , loading: true
  , showError: false
  , submitting: false
  , shareResume: View
  , tokenSecret: Nothing
  , sharingInput
  }

data Query a
  = Init a
  | ChangeGroup String a
  | ChangeEmail String a
  | ChangeShareResume ShareResume a
  | ChangeSubjectType SubjectType a
  | ChangeTokenName String a
  | DismissError a
  | SelectElement DOM.Event a
  | PreventDefaultAndShare DOM.Event a
  | PreventDefault DOM.Event a
  | Cancel a

data Message = Dismiss

copyButtonRef ∷ H.RefLabel
copyButtonRef = H.RefLabel "copy"

component ∷ H.Component HH.HTML Query SharingInput Message Slam
component =
  H.lifecycleComponent
    { render
    , eval
    , initialState
    , finalizer: Nothing
    , initializer: Just (H.action Init)
    , receiver: const Nothing
    }

render ∷ State → HTML
render state =
  HH.form
    [ HP.classes [ HH.ClassName "deck-dialog-share" ]
    , HE.onSubmit (HE.input PreventDefaultAndShare)
    ]
    ( renderTitle
    ⊕ renderBody
    ⊕ renderFooter
    )

  where
  renderTitle ∷ Array HTML
  renderTitle = case state.tokenSecret of
    Nothing → [ HH.h4_ [ HH.text "Share deck" ] ]
    Just _ →
      [ HH.h4_
          [ HH.text
              if state.tokenName ≡ ""
                then "Untitled token"
                else state.tokenName ⊕ " token"
          ]
      , HH.h5_
          [ HH.text $ printShareResume state.shareResume ]
      ]

  renderBody ∷ Array HTML
  renderBody =
    [ HH.div
       [ HP.classes [ HH.ClassName "deck-dialog-body" ] ]
       ( renderLoading
       ⊕ foldMap renderTokenSecret state.tokenSecret
       ⊕ renderFieldsetWrapper renderSubjectForm
       )
    ]

  renderLoading ∷ Array HTML
  renderLoading =
    [ HH.div
        [ HP.classes $
            [ B.alert
            , B.alertInfo
            , HH.ClassName "share-loading"
            ]
            ⊕ (guard (not state.loading) $> B.hidden)
        ]
        [ HH.img [ HP.src "img/blue-spin.svg" ]
        , HH.text "Loading"
        ]
    ]

  renderTokenSecret ∷ String → Array HTML
  renderTokenSecret token =
    [ HH.form
        [ HE.onSubmit (HE.input PreventDefault)
        , HP.classes $ guard state.loading $> B.hidden
        ]
        [ HH.div
            [ HP.classes [ B.inputGroup ]
            , HE.onClick (HE.input SelectElement ∘ DOM.toEvent)
            ]
            [ HH.input
                [ HP.classes [ B.formControl ]
                , HP.value token
                , HP.readOnly true
                , HP.title "Token secret"
                ]
            , HH.span
                [ HP.classes [ B.inputGroupBtn ] ]
                [ HH.button
                    [ HP.classes [ B.btn, B.btnDefault ]
                    , HP.ref copyButtonRef
                    ]
                    [ I.copySm ]
                ]
            ]
        ]
    ]

  renderFieldsetWrapper :: Array HTML -> Array HTML
  renderFieldsetWrapper c =
    let
      isFatalError = case _ of
        Just Connection -> true
        Just GroupList -> true
        _ -> false
    in
      [ HH.fieldset [ HP.disabled (isFatalError state.error) ] c ]
      
  renderSubjectForm ∷ Array HTML
  renderSubjectForm =
    [ HH.div
        [ HP.classes $ guard (isJust state.tokenSecret ∨ state.loading) $> B.hidden ]
        [ HH.label_
            [ HH.text "Subject type"
            , HH.select
                [ HP.classes [ B.formControl ]
                , HE.onValueChange (HE.input (ChangeSubjectType ∘ readSubjectType))
                , HP.disabled state.submitting
                ]
                $ [ HH.option
                      [ HP.value "user" ]
                      [ HH.text "User" ]
                  ]
                ⊕ (guard (not $ Arr.null state.groups) $>
                    HH.option
                      [ HP.value "group" ]
                      [ HH.text "Group" ]
                  )
                ⊕ [ HH.option
                      [ HP.value "token" ]
                      [ HH.text "Token" ]
                  ]
            ]
        , HH.label
            [ HP.classes
                $ (guard (state.subjectType ≠ User) $> B.hidden)
                ⊕ (guard (state.error ≡ Just Validation) $> B.hasError)
            ]
            [ HH.text "User email"
            , HH.input
                [ HP.classes [ B.formControl ]
                , HP.value state.email
                , HE.onValueInput $ HE.input ChangeEmail
                , HP.placeholder "User email"
                , HP.type_ HP.InputText
                , HP.disabled state.submitting
                ]
            ]
        , HH.label
            [ HP.classes $ guard (state.subjectType ≠ Group) $> B.hidden ]
            [ HH.text "Group path"
            , HH.select
                [ HP.classes [ B.formControl ]
                , HE.onValueChange $ HE.input ChangeGroup
                , HP.disabled $ Arr.null state.groups ∨ state.submitting
                ]
                -- I find using such things in where a bit complicated
                -- because it's break context. OTOH, lambdas are not
                -- so fancy as named func. cryogenian.
                let
                  renderOption ∷ FilePath → HTML
                  renderOption group =
                    HH.option
                      [ HP.value $ Pt.printPath  group ]
                      [ HH.text $ Pt.printPath  group ]
                in
                  map renderOption state.groups
            ]
        , HH.label
            [ HP.classes $ guard (state.subjectType ≠ Token) $> B.hidden
            ]
            [ HH.text "Token name"
            , HH.input
                [ HP.classes [ B.formControl ]
                , HP.type_ HP.InputText
                , HP.placeholder "Token name"
                , HP.value state.tokenName
                , HE.onValueInput (HE.input ChangeTokenName)
                , HP.disabled state.submitting
                ]
            ]
        , HH.label_
            [ HH.text "Permission"
            , HH.select
                [ HP.classes [ B.formControl ]
                , HE.onValueChange (HE.input (ChangeShareResume ∘ readShareResume))
                , HP.disabled state.submitting
                ]
                [ HH.option
                    [ HP.value "view" ]
                    [ HH.text "View" ]
                , HH.option
                    [ HP.value "edit" ]
                    [ HH.text "Edit" ]
                ]
            ]
        , HH.div
            [ HP.classes
                $ [ B.alert, B.alertDanger ]
                ⊕ (if state.showError
                      ∧ (state.error ≡ Just Connection ∨ state.error ≡ Just GroupList)
                      then [ ]
                      else [ B.hidden ])
            ]
            [ HH.text
                if state.error ≡ Just Connection
                then
                  "This action couldn't be performed. "
                  ⊕ "Please check your network connection and try again"
                else
                  "Groups are unavailable. To share this deck with a group "
                  ⊕ "please check your network connection and try again."
            ]
        , HH.div
            [ HP.classes
                $ [ B.alert, B.alertInfo ]
                ⊕ (if state.showError ∧ state.error ≡ Just Validation then [ ] else [ B.hidden ])
            ]
            [ HH.text "Please check if user email is correct" ]
        ]
    ]

  renderFooter ∷ Array HTML
  renderFooter =
    [ HH.div
        [ HP.classes [ HH.ClassName "deck-dialog-footer" ] ]
        $ [ HH.button
              [ HP.classes
                  $ [ B.btn, B.btnDefault ]
                  ⊕ (guard state.loading $> B.hidden)
              , HP.type_ HP.ButtonButton
              , HE.onClick $ HE.input_ Cancel
              , HP.disabled state.submitting
              ]
              [ HH.text "Dismiss" ]
          ]
        ⊕ (guard (isNothing state.tokenSecret) *> renderShare)
    ]

  renderShare ∷ Array HTML
  renderShare =
    [ HH.button
        [ HP.classes
            $ [ B.btn, B.btnPrimary ]
            ⊕ (guard state.loading $> B.hidden)
            ⊕ (guard (isJust state.error && state.showError) $> B.hasError)
        , HP.disabled $
            state.submitting
            ∨ ((state.email ≡ "" ∨ state.error ≡ Just Validation)
               ∧ state.subjectType ≡ User)
        ]
        [ HH.text
            if state.submitting
              then "Sharing..."
              else "Share"
        ]
    ]

eval ∷ Query ~> DSL
eval = case _ of
  Init next → do
    Q.groupInfo (rootDir </> file "") >>= case _ of
      Left _ →
        H.modify _
          { error = Just GroupList
          , showError = true
          , loading = false
          }
      Right grInfo → do
        let groups = [ rootFile ] ⊕ grInfo.subGroups
        H.modify _
          { groups = groups
          , loading = false
          }
    pure next
  PreventDefault ev next ->
     H.liftEff (DOM.preventDefault ev) $> next
  PreventDefaultAndShare ev next → do
    H.liftEff (DOM.preventDefault ev)
    H.modify _
      { error = Nothing
      , showError = false
      , submitting = true
      }
    state ← H.get
    if state.subjectType ≡ Token
      then shareToken state
      else sharePermission state
    pure next
  DismissError next →
    H.modify _ { showError = false } $> next
  ChangeSubjectType st next → do
    if st ≡ User
      then H.modify _
        { subjectType = st
        , error = Nothing
        , showError = false
        }
      else H.modify _ { subjectType = st }
    pure next
  ChangeEmail str next → do
    mbError ← H.gets _.error
    unless (mbError ≡ Just Connection) $
      if emailIsIncorrect str
        then H.modify _
          { error = Just Validation
          , showError = true
          , email = str
          }
        else H.modify _
          { error = Nothing
          , showError = false
          , email = str
          }
    pure next
  ChangeGroup grpString next → do
    groups ← H.gets _.groups
    let group = Arr.find (eq grpString ∘ Pt.printPath) groups
    H.modify _ { groupSelected = group }
    pure next
  ChangeTokenName str next →
    H.modify _ { tokenName = str } $> next
  ChangeShareResume sr next →
    H.modify _ { shareResume = sr } $> next
  SelectElement ev next → do
    H.liftEff $ DOM.currentTarget ev
      # readHTMLElement ∘ toForeign
      # runExcept
      # traverse_ select
    pure next
  Cancel next →
    H.raise Dismiss $> next

emailIsIncorrect ∷ String → Boolean
emailIsIncorrect = not ∘ Str.contains (Str.Pattern "@")

initClipboard ∷ String → DSL Unit
initClipboard token =
  H.getHTMLElementRef copyButtonRef >>= traverse_ \htmlEl →
    H.liftEff $ C.fromElement (toElement htmlEl) (pure token)

shareToken ∷ State → DSL Unit
shareToken state = do
  res ←
    Q.createToken
      (if state.tokenName ≡ "" then Nothing else Just $ QT.TokenName state.tokenName)
      (sharingActions state.sharingInput state.shareResume)
  case res of
    Left _ → showConnectionError
    Right token → do
      let tokenSecret = QT.runTokenHash token.secret
      H.modify _
        { tokenSecret = Just tokenSecret
        , submitting = false
        }
      initClipboard tokenSecret

sharePermission ∷ State → DSL Unit
sharePermission state = do
  let
    users = if state.subjectType ≡ User then [ QT.UserId state.email ] else [ ]
    groups = if state.subjectType ≡ Group then foldMap (pure ∘ Right) state.groupSelected else [ ]
    actions = sharingActions state.sharingInput state.shareResume
  Q.sharePermission { users, groups, actions } >>= case _ of
    Left _   → showConnectionError
    Right [] → showConnectionError
    Right _  → H.raise Dismiss

showConnectionError ∷ DSL Unit
showConnectionError =
  H.modify _
    { error = Just Connection
    , showError = true
    , submitting = false
    }
