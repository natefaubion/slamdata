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

module SlamData.FileSystem.Dialog.Download.Component (dialog) where

import SlamData.Prelude

import Control.UI.Browser as Browser
import Data.Lens (_Left, _Right, (.~))
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Quasar.Data.CSV as CSV
import Quasar.Data.Json as Json
import SlamData.Dialog.Component as D
import SlamData.Download.Model as DM
import SlamData.Download.Render as DR
import SlamData.FileSystem.Dialog.Download.Component.State as S
import SlamData.FileSystem.Resource as R
import SlamData.Monad (Slam)
import SlamData.Quasar.Auth as QAuth
import SlamData.Render.ClassName as CN
import Utils.Path (dropWorkspaceExt)

data Query a
  = Modify (∀ r. DM.DownloadModel r → DM.DownloadModel r) a
  | Dismiss a
  | Download a

dialog ∷ ∀ o. R.Resource → D.DialogSpec o Slam
dialog res =
  D.dialog
    $ D.withTitle ("Download “" <> dropWorkspaceExt (R.resourceName res) <> "”")
    >>> D.withInitialState (S.initialState res)
    >>> D.withRender render
    >>> D.withEval eval
    >>> D.withClass (H.ClassName "sd-download-dialog")
    >>> D.withButton
          (D.button
            $ D.withLabel "Cancel"
            >>> D.withAction (const (Just Dismiss)))
    >>> D.withButton
          (D.button
            $ D.withLabel "Download"
            >>> D.withClass CN.btnPrimary
            >>> D.withAction
                (\st → if isJust st.error then Nothing else Just Download))

render ∷ S.State → H.ComponentHTML Query
render state@{ options, targetName } =
  HH.div_
    $ join
        [ pure $ HH.div
            [ HP.classes [ CN.formGroup, CN.downloadSource ] ]
            [ HH.label_
                [ HH.span_ [ HH.text "Source" ]
                , HH.text (R.resourcePath state.resource)
                ]
            ]
        , pure $ DR.fldName
            (DM.shouldCompress state)
            options
            targetName
            (\name → Modify (_ { targetName = name }))
        , guard (not DM.alwaysCompress state.resource) $>
            HH.div
              [ HP.classes [ CN.formGroup, H.ClassName "sd-download-compress-option" ] ]
              [ HH.label_
                  [ HH.input
                      [ HP.type_ HP.InputCheckbox
                      , HE.onChecked $ HE.input (\b → Modify (_ { compress = b }))
                      ]
                  , HH.text "Compress as .zip"
                  ]
              ]
        , guard (not R.isWorkspace state.resource) $> renderOptions state
        , pure $ renderError state
        ]

renderOptions ∷ S.State → H.ComponentHTML Query
renderOptions { options } =
  HH.div
    [ HP.classes [ CN.formGroup ] ]
    [ HH.ul
        [ HP.classes [ CN.nav, CN.navTabs ] ]
        [ HH.li
            (guard (isLeft options) $> HP.class_ CN.active)
            [ HH.a
                [ HE.onClick $ HE.input_ (Modify (setOutput DM.CSV)) ]
                [ HH.text "CSV" ]
            ]
        , HH.li
            (guard (isRight options) $> HP.class_ CN.active)
            [ HH.a
                [ HE.onClick $ HE.input_ (Modify (setOutput DM.JSON)) ]
                [ HH.text "JSON" ]
            ]
        ]
    , either renderOptionsCSV renderOptionsJSON options
    ]

setOutput ∷ ∀ r. DM.OutputType → DM.DownloadModel r → DM.DownloadModel r
setOutput ty st = case ty, st.options of
  DM.CSV, Left _ → st
  DM.CSV, _ → st { options = Left CSV.defaultOptions }
  DM.JSON, Right _ → st
  DM.JSON, _ → st { options = Right DM.initialJSONOptions }

renderOptionsCSV ∷ CSV.Options → H.ComponentHTML Query
renderOptionsCSV =
  DR.optionsCSV \lens v →
    Modify \st → st { options = st.options # _Left ∘ lens .~ v }

renderOptionsJSON ∷ Json.Options → H.ComponentHTML Query
renderOptionsJSON =
  DR.optionsJSON \lens v →
    Modify \st → st { options = st.options # _Right ∘ lens .~ v }

renderError ∷ S.State → H.ComponentHTML Query
renderError { error } = case error of
  Nothing → HH.text ""
  Just err →
    HH.div
      [ HP.classes [ CN.alert, CN.alertDanger ] ]
      [ HH.text err ]

eval ∷ ∀ o. Query ~> H.ComponentDSL S.State Query (D.Message o) Slam
eval = case _ of
  Modify f next → do
    newState ← H.modify (S.validate ∘ f)
    pure next
  Dismiss next → do
    H.raise D.Dismiss
    pure next
  Download next → do
    opts ← H.get
    authHeaders ← lift QAuth.authHeaders
    H.liftEff $ Browser.newTab (DM.renderURL authHeaders opts)
    H.raise D.Dismiss
    pure next
