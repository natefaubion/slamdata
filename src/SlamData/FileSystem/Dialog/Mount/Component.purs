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
  ( dialog
  , module Exports
  ) where

import SlamData.Prelude

import Data.Lens (Prism', preview, review)
import Data.Maybe.First (First(..))
import Data.Variant as V
import Halogen as H
import Halogen.Component.ChildPath as CP
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Quasar.Mount.Type as QMT
import SlamData.Dialog.Component as D
import SlamData.Dialog.Render as DR
import SlamData.FileSystem.Dialog.Mount.Common.SettingsQuery as SQ
import SlamData.FileSystem.Dialog.Mount.Component.ChildSlot as CS
import SlamData.FileSystem.Dialog.Mount.Component.State (Input(..)) as Exports
import SlamData.FileSystem.Dialog.Mount.Component.State as S
import SlamData.FileSystem.Dialog.Mount.Couchbase.Component as Couchbase
import SlamData.FileSystem.Dialog.Mount.MarkLogic.Component as MarkLogic
import SlamData.FileSystem.Dialog.Mount.Mimir.Component as Mimir
import SlamData.FileSystem.Dialog.Mount.Module.Component as Module
import SlamData.FileSystem.Dialog.Mount.MongoDB.Component as MongoDB
import SlamData.FileSystem.Dialog.Mount.SparkFTP.Component as SparkFTP
import SlamData.FileSystem.Dialog.Mount.SparkHDFS.Component as SparkHDFS
import SlamData.FileSystem.Dialog.Mount.SparkLocal.Component as SparkLocal
import SlamData.FileSystem.Dialog.Mount.Unknown.Component as Unknown
import SlamData.FileSystem.Dialog.Mount.View.Component as View
import SlamData.FileSystem.Resource as R
import SlamData.GlobalError as GE
import SlamData.Monad (Slam)
import SlamData.Quasar.FS as FS
import SlamData.Quasar.Mount as QM
import SlamData.Render.ClassName as CN
import Utils.Path as UP

data Query a
  = SetName String a
  | SelectScheme (Maybe QMT.MountType) a
  | Update (SQ.SettingsMessage QM.MountConfig) a
  | Dismiss a
  | Mount (UP.AnyPath × QM.MountConfig) a
  | Unmount a

type Message o = Variant (deleted ∷ UP.AnyPath, mounted ∷ Unit | o)
type HTML = H.ParentHTML Query CS.ChildQuery CS.ChildSlot Slam
type DSL o = H.ParentDSL S.State Query CS.ChildQuery CS.ChildSlot (D.Message (Message o)) Slam

dialog ∷ ∀ o. S.Input → D.DialogSpec (Message o) Slam
dialog input =
  D.dialog
    $ D.withTitle (title input)
    >>> D.withInitialState (S.initialState input)
    >>> D.withClass (H.ClassName "sd-mount-dialog")
    >>> D.withParentRender render
    >>> D.withEval eval
    >>> D.withPending (\st → st.status /= S.Idle)
    >>> buttons
    >>> D.withButton btnSave
  where
    title = case _ of
      S.Edit { path } → "Configure mount “" <> UP.getNameStr path <> "”"
      _ → "Create mount"
    buttons = case input of
      S.Edit { fromRoot } | fromRoot →
        D.withButton btnUnmount >>> D.withButton btnCancel
      _ →
        D.withButton btnCancel
    btnUnmount =
      D.button
        $ D.withLabel "Unmount"
        >>> D.withClass CN.btnDanger
        >>> D.withAction (const (Just Unmount))
        >>> D.withPending (\st → st.status == S.Unmounting)
    btnCancel =
      D.button
        $ D.withLabel "Cancel"
        >>> D.withAction (const (Just Dismiss))
    btnSave =
      D.button
        $ D.withLabel (if S.isNew input then "Mount" else "Save changes")
        >>> D.withClass CN.btnPrimary
        >>> D.withAction (\st → Mount <$> (Tuple <$> hush st.pathValue <*> hush st.configValue))
        >>> D.withPending (\st → st.status == S.Saving)

render ∷ S.State → HTML
render state@{ input, name, scheme, pathValue, configValue, error } =
  HH.div
    [ HP.class_ (H.ClassName "sd-mount-dialog-inner") ]
    $ join
        [ foldMap (pure ∘ renderNameField) name
        , pure $ renderSelectScheme scheme
        , foldMap (pure ∘ renderSettings (_.mount <$> preview S._Edit state.input)) state.scheme
        , foldMap (pure ∘ DR.renderError) $
            ala First foldMap [extractError pathValue, extractError configValue, error]
        ]
  where
    extractError ∷ ∀ a b. Either (Maybe a) b → Maybe a
    extractError = either id (const Nothing)

renderSettings ∷ Maybe QM.MountConfig → QMT.MountType → HTML
renderSettings config =
  case _ of
    QMT.MongoDB → embed CS.cpMongoDB MongoDB.component QM._MongoDB
    QMT.View → embed CS.cpView View.component QM._View
    QMT.Module → embed CS.cpModule Module.component QM._Module
    QMT.Couchbase → embed CS.cpCouchbase Couchbase.component QM._Couchbase
    QMT.MarkLogic → embed CS.cpMarkLogic MarkLogic.component QM._MarkLogic
    QMT.SparkFTP → embed CS.cpSparkFTP SparkFTP.component QM._SparkFTP
    QMT.SparkHDFS → embed CS.cpSparkHDFS SparkHDFS.component QM._SparkHDFS
    QMT.SparkLocal → embed CS.cpSparkLocal SparkLocal.component QM._SparkLocal
    QMT.Mimir → embed CS.cpMimir Mimir.component QM._Mimir
    QMT.Unknown _ → embed CS.cpUnknown Unknown.component QM._Unknown
  where
    embed
      ∷ ∀ f a
      . CP.ChildPath f CS.ChildQuery Unit CS.ChildSlot
      → H.Component HH.HTML f (Maybe a) (SQ.SettingsMessage a) Slam
      → Prism' QM.MountConfig a
      → HTML
    embed cp comp prism =
      HH.slot' cp unit comp
        (preview prism =<< config)
        (HE.input Update ∘ map (review prism))

renderNameField ∷ String → HTML
renderNameField name =
  HH.div
    [ HP.class_ CN.formGroup ]
    [ HH.label_
        [ HH.span_ [ HH.text "Name" ]
        , HH.input
            [ HP.class_ CN.formControl
            , HE.onValueInput (HE.input SetName)
            , HP.value name
            ]
        ]
    ]

renderSelectScheme ∷ Maybe QMT.MountType → HTML
renderSelectScheme scheme =
  HH.div
    [ HP.class_ CN.formGroup ]
    [ HH.label_
        [ HH.span_ [ HH.text "Mount type" ]
        , HH.select
            [ HP.class_ CN.formControl
            , HE.onValueChange (HE.input SelectScheme ∘ typeFromString)
            ]
            $ [ HH.option_ [] ] <> map renderOption mountTypes
        ]
    ]
  where
    renderOption s =
      let
        label = typeToString s
      in
        -- We do a string comparison on the label rather than the value to
        -- allow for the `Unknown` case to match, regardless of the inner
        -- mount type
        HH.option
          [ HP.selected (typeToString <$> scheme == Just label) ]
          [ HH.text label ]

eval ∷ ∀ o. Query ~> DSL o
eval = case _ of
  SetName newName next → do
    modify (_ { name = Just newName })
    pure next
  SelectScheme newScheme next → do
    { scheme, input } ← H.get
    when (scheme /= newScheme) do
      let configValue = note Nothing (S.matchSchemeConfig input =<< newScheme)
      modify (_ { scheme = newScheme, configValue = configValue })
    pure next
  Update (SQ.Modified esm) next → do
    modify (_ { configValue = lmap Just esm })
    pure next
  Dismiss next → do
    H.raise D.Dismiss
    pure next
  Mount (path × config) next → do
    modify (_ { status = S.Saving })
    success ← handleQuasar "There was a problem saving the mount"
      =<< QM.saveMount path config
    when success $ H.raise (D.Bubble (V.inj _mounted unit))
    pure next
  Unmount next → do
    H.gets _.input >>= case _ of
      S.Edit { path } → do
        modify (_ { status = S.Unmounting })
        success ← handleQuasar "There was a problem removing the mount"
          =<< FS.delete (R.Mount (either R.Database R.View path))
        when success $ H.raise (D.Bubble (V.inj _deleted path))
      _ →
        pure unit
    pure next

handleQuasar ∷ ∀ a o. String → Either QM.QError a → DSL o Boolean
handleQuasar errorPrefix = case _ of
  Left err → do
    case GE.fromQError err of
      Left msg →
        H.modify (_
          { error = Just $ errorPrefix <> ": " <> msg
          , status = S.Idle
          })
      Right ge → do
        H.modify (_ { status = S.Idle })
        GE.raiseGlobalError ge
    pure false
  Right _ → do
    H.raise D.Dismiss
    pure true

modify ∷ ∀ o. (S.State → S.State) → DSL o Unit
modify f = H.modify (S.validate ∘ f ∘ (_ { error = Nothing }))

typeToString ∷ QMT.MountType → String
typeToString = case _ of
  QMT.MongoDB → "MongoDB"
  QMT.View → "SQL² view"
  QMT.Module → "SQL² module"
  QMT.Couchbase → "Couchbase"
  QMT.MarkLogic → "MarkLogic"
  QMT.SparkHDFS → "HDFS on Spark"
  QMT.SparkFTP → "FTP on Spark"
  QMT.SparkLocal → "Local on Spark"
  QMT.Mimir → "Mimir"
  QMT.Unknown _ → "Advanced"

typeFromString ∷ String → Maybe QMT.MountType
typeFromString = case _ of
  "MongoDB" → Just QMT.MongoDB
  "SQL² view" → Just QMT.View
  "SQL² module" → Just QMT.Module
  "Couchbase" → Just QMT.Couchbase
  "MarkLogic" → Just QMT.MarkLogic
  "HDFS on Spark" → Just QMT.SparkHDFS
  "FTP on Spark" → Just QMT.SparkFTP
  "Local on Spark" → Just QMT.SparkLocal
  "Mimir" → Just QMT.Mimir
  "Advanced" → Just (QMT.Unknown Nothing)
  _ → Nothing

mountTypes :: Array QMT.MountType
mountTypes =
  [ QMT.MongoDB
  , QMT.View
  , QMT.Module
  , QMT.Couchbase
  , QMT.MarkLogic
  , QMT.SparkHDFS
  , QMT.SparkFTP
  , QMT.SparkLocal
  , QMT.Unknown Nothing
  ]

_deleted ∷ SProxy "deleted"
_deleted = SProxy

_mounted ∷ SProxy "mounted"
_mounted = SProxy
