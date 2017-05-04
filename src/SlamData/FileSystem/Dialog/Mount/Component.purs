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

module SlamData.FileSystem.Dialog.Mount.Component
  ( component
  , module SlamData.FileSystem.Dialog.Mount.Component.Query
  , module MCS
  ) where

import SlamData.Prelude

import Data.Argonaut (jsonParser, decodeJson, (.?))
import Data.Lens (set, (.~), (?~))
import Data.Path.Pathy (rootDir)

import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.Themes.Bootstrap3 as B

import SlamData.Dialog.Render (modalDialog, modalHeader, modalBody, modalFooter)
import SlamData.FileSystem.Dialog.Component.Message (Message)
import SlamData.FileSystem.Dialog.Component.Message as Message
import SlamData.FileSystem.Dialog.Mount.Common.SettingsQuery as SQ
import SlamData.FileSystem.Dialog.Mount.Component.ChildSlot as CS
import SlamData.FileSystem.Dialog.Mount.Component.Query (Query(..))
import SlamData.FileSystem.Dialog.Mount.Component.State as MCS
import SlamData.FileSystem.Dialog.Mount.Couchbase.Component as Couchbase
import SlamData.FileSystem.Dialog.Mount.MarkLogic.Component as MarkLogic
import SlamData.FileSystem.Dialog.Mount.MongoDB.Component as MongoDB
import SlamData.FileSystem.Dialog.Mount.SQL2.Component as SQL2
import SlamData.FileSystem.Dialog.Mount.Scheme as MS
import SlamData.FileSystem.Dialog.Mount.SparkHDFS.Component as Spark
import SlamData.FileSystem.Dialog.Mount.SparkLocal.Component as SparkLocal
import SlamData.GlobalError as GE
import SlamData.Monad (Slam)
import SlamData.Quasar.FS as Api
import SlamData.Render.CSS as RC

import Utils.DOM as DOM

type DSL = H.ParentDSL MCS.State Query CS.ChildQuery CS.ChildSlot Message Slam
type HTML = H.ParentHTML Query CS.ChildQuery CS.ChildSlot Slam

component ∷ H.Component HH.HTML Query MCS.Input Message Slam
component =
  H.parentComponent
    { initialState: MCS.initialState
    , render
    , eval
    , receiver: const Nothing
    }

render ∷ MCS.State → HTML
render state@{ name, new, parent } =
  HH.form
    [ HE.onSubmit $ HE.input PreventDefaultAndNotifySave ]
    [ modalDialog
        [ modalHeader "Mount"
        , modalBody
            $ HH.div
                [ HP.class_ RC.dialogMount ]
                $ maybe [] (pure ∘ fldName) state.name
                <> (pure $ selScheme state)
                <> maybe [] (pure ∘ settings) state.settings
                <> maybe [] (pure ∘ errorMessage) state.message
        , modalFooter
            $ (guard (not new ∧ isNothing parent) $> btnDelete)
            <> [ progressSpinner state, btnMount state, btnCancel ]
        ]
      ]
  where
  settings ∷ MCS.MountSettings → HTML
  settings ss = case ss of
    MCS.MongoDB initialState →
      HH.slot' CS.cpMongoDB unit (MongoDB.comp initialState) unit (HE.input_ Validate)
    MCS.SQL2 initialState →
      HH.slot' CS.cpSQL unit (SQL2.comp initialState) unit (HE.input_ Validate)
    MCS.Couchbase initialState →
      HH.slot' CS.cpCouchbase unit (Couchbase.comp initialState) unit (HE.input_ Validate)
    MCS.MarkLogic initialState →
      HH.slot' CS.cpMarkLogic unit (MarkLogic.comp initialState) unit (HE.input_ Validate)
    MCS.SparkHDFS initialState →
      HH.slot' CS.cpSpark unit (Spark.comp initialState) unit (HE.input_ Validate)
    MCS.SparkLocal initialState ->
      HH.slot' CS.cpSparkLocal unit (SparkLocal.comp initialState) unit (HE.input_ Validate)

fldName ∷ String → HTML
fldName name =
  HH.div
    [ HP.classes [B.formGroup, RC.mountName] ]
    [ HH.label_
        [ HH.span_ [ HH.text "Name" ]
        , HH.input
            [ HP.class_ B.formControl
            , HE.onValueInput $ HE.input (ModifyState ∘ set MCS._name ∘ Just)
            , HP.value name
            ]
        ]
    ]

selScheme ∷ MCS.State → HTML
selScheme state =
  HH.div
    [ HP.class_ B.formGroup ]
    [ HH.label_
        [ HH.span_ [ HH.text "Mount type" ]
        , HH.select
            [ HP.class_ B.formControl
            , HE.onValueChange (HE.input SelectScheme ∘ MS.schemeFromString)
            ]
            $ [ HH.option_ [] ] <> schemeOptions state.settings
        ]
    ]
  where
  schemeOptions settings =
    map
      (\s → let string = MS.schemeToString s in HH.option [ HP.selected $ Just string ≡ (MS.schemeToString ∘ MCS.scheme <$> state.settings) ] [ HH.text string ])
      MS.schemes

errorMessage ∷ String → HTML
errorMessage msg =
  HH.div
    [ HP.classes [ B.alert, B.alertDanger ] ]
    [ HH.text msg ]

btnCancel ∷ HTML
btnCancel =
  HH.button
    [ HP.classes [B.btn]
    , HE.onClick (HE.input_ RaiseDismiss)
    ]
    [ HH.text "Cancel" ]

btnDelete ∷ HTML
btnDelete =
  HH.button
    [ HP.classes [B.btn, HH.ClassName "btn-careful" ]
    , HE.onClick (HE.input_ RaiseMountDelete)
    ]
    [ HH.text "Unmount" ]

btnMount ∷ MCS.State → HTML
btnMount state@{ new, saving } =
  HH.button
    [ HP.classes [B.btn, B.btnPrimary]
    , HP.enabled (not saving && MCS.canSave state)
    ]
    [ HH.text text ]
  where
  text = if new then "Mount" else "Save changes"

progressSpinner ∷ MCS.State → HTML
progressSpinner { saving } =
  HH.img [ HP.src "img/spin.gif", HP.class_ (RC.mountProgressSpinner saving) ]

eval ∷ Query ~> DSL
eval (ModifyState f next) = H.modify f *> validateInput $> next
eval (SelectScheme newScheme next) = do
  currentScheme ← map MCS.scheme <$> H.gets _.settings
  when (currentScheme /= newScheme) do
    H.modify (MCS._settings .~ map MCS.initialSettings newScheme)
    validateInput
  pure next
eval (RaiseDismiss next) = do
  H.raise Message.Dismiss
  pure next
eval (Save k) = do
  { new, parent, name } ← H.get
  let name' = fromMaybe "" name
  let parent' = fromMaybe rootDir parent
  H.modify (MCS._saving .~ true)
  newName ←
    if new then Api.getNewName parent' name' else pure (pure name')
  case newName of
    Left err → do
      handleQError err
      pure $ k Nothing
    Right newName' → do
      result ← querySettings (H.request (SQ.Submit parent' newName'))
      mount ← case result of
        Just (Right m) → pure (Just m)
        Just (Left err) → do
          handleQError err
          pure Nothing
        Nothing → pure Nothing
      H.modify (MCS._saving .~ false)
      pure $ k mount
eval (PreventDefaultAndNotifySave ev next) = do
  H.liftEff (DOM.preventDefault ev)
  notifySave
  pure next
eval (Validate next) = validateInput $> next
eval (RaiseMountDelete next) = H.raise Message.MountDelete $> next

notifySave ∷ DSL Unit
notifySave =
  H.raise Message.MountSave

handleQError ∷ Api.QError → DSL Unit
handleQError err =
  case GE.fromQError err of
    Left msg → H.modify (MCS._message ?~ formatError msg)
    Right ge → GE.raiseGlobalError ge

validateInput ∷ DSL Unit
validateInput = do
  state ← H.get
  message ← runExceptT do
    liftMaybe (MCS.validate state)
    liftMaybe ∘ join =<< lift (querySettings (H.request SQ.Validate))
  H.modify (MCS._message .~ either Just (const Nothing) message)
  where
  liftMaybe ∷ Maybe String → ExceptT String DSL Unit
  liftMaybe = maybe (pure unit) throwError

formatError ∷ String → String
formatError err =
  "There was a problem saving the mount: " <> extract err
  where
  extract msg =
    either (const msg) id (jsonParser msg >>= decodeJson >>= (_ .? "error"))

querySettings ∷ forall a. (forall s. SQ.SettingsQuery s a) → DSL (Maybe a)
querySettings q = (map MCS.scheme <$> H.gets _.settings) >>= \s →
  case s of
    Just MS.MongoDB → H.query' CS.cpMongoDB unit q
    Just MS.SQL2 → H.query' CS.cpSQL unit q
    Just MS.Couchbase → H.query' CS.cpCouchbase unit q
    Just MS.MarkLogic → H.query' CS.cpMarkLogic unit q
    Just MS.SparkHDFS → H.query' CS.cpSpark unit q
    Just MS.SparkLocal → H.query' CS.cpSparkLocal unit q
    Nothing → pure Nothing
