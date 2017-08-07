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

module SlamData.Workspace.Card.DownloadOptions.Component (component) where

import SlamData.Prelude

import Data.Lens ((.~), _Left, _Right, (%~))
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Quasar.Data.CSV as CSV
import Quasar.Data.Json as Json
import SlamData.Download.Model as DL
import SlamData.Download.Render as DLR
import SlamData.Render.ClassName as CN
import SlamData.Workspace.Card.CardType as CT
import SlamData.Workspace.Card.Component as CC
import SlamData.Workspace.Card.DownloadOptions.Component.Query (Query(..))
import SlamData.Workspace.Card.DownloadOptions.Component.State (State, _options, initialState)
import SlamData.Workspace.Card.Model as Card
import SlamData.Workspace.Card.Port as Port
import SlamData.Workspace.LevelOfDetails as LOD

type DSL = CC.InnerCardDSL State Query
type HTML = CC.InnerCardHTML Query

component ∷ CC.CardOptions → CC.CardComponent
component =
  CC.makeCardComponent CT.DownloadOptions $ H.component
    { render: render
    , eval: evalCard ⨁ evalComponent
    , initialState: const initialState
    , receiver: const Nothing
    }

render ∷ State → HTML
render state =
  case state.source of
    Nothing →
      HH.div
        [ HP.classes [ CN.alert, CN.alertDanger ] ]
        [ HH.text "The current input cannot be downloaded" ]
    Just _ →
      HH.div
        [ HP.classes
            [ CN.downloadCardEditor
            , HH.ClassName "card-input-maximum-lod"
            ]
        ]
        [ renderDownloadTypeSelector state
        , renderDownloadConfiguration state
        ]

renderDownloadConfiguration ∷ State → HTML
renderDownloadConfiguration state =
  HH.div
    [ HP.classes [ CN.downloadConfiguration ] ]
    [ DLR.fldName false state.options (fromMaybe "" state.targetName) (\s → right ∘ TargetTyped s)
    , either optionsCSV optionsJSON state.options
    ]

optionsCSV ∷ CSV.Options → HTML
optionsCSV = DLR.optionsCSV (\lens v → right ∘ (ModifyCSVOpts (lens .~ v)))

optionsJSON ∷ Json.Options → HTML
optionsJSON = DLR.optionsJSON (\lens v → right ∘ (ModifyJSONOpts (lens .~ v)))

compress ∷ State → HTML
compress state =
  HH.div
    [ HP.classes [ CN.formGroup ] ]
    [ HH.label_
        [ HH.span_ [ HH.text "Compress" ]
        , HH.input
            [ HP.type_ HP.InputCheckbox
            , HP.checked compressed
            , HP.enabled $ isJust state.source
            , HE.onValueChange (HE.input_ (right ∘ ToggleCompress))
            ]
        ]
    ]
  where
  compressed ∷ Boolean
  compressed = isJust state.source && state.compress

renderDownloadTypeSelector ∷ State → HTML
renderDownloadTypeSelector state =
  HH.div
    [ HP.classes [ CN.downloadTypeSelector ] ]
    [ HH.img
        [ HP.src "img/csv.svg"
        , HP.classes (guard (isLeft state.options) $> CN.active)
        , HP.title "Comma separated values"
        , HE.onClick (HE.input_ (right ∘ SetOutput DL.CSV))
        ]
    , HH.img
        [ HP.src "img/json.svg"
        , HP.classes (guard (isRight state.options) $> CN.active)
        , HP.title "JSON"
        , HE.onClick (HE.input_ (right ∘ SetOutput DL.JSON))
        ]
    ]

evalCard ∷ CC.CardEvalQuery ~> DSL
evalCard = case _ of
  CC.Activate next →
    pure next
  CC.Deactivate next →
    pure next
  CC.Save k →
    map (k ∘ Card.DownloadOptions) H.get
  CC.Load card next → do
    case card of
      Card.DownloadOptions st → H.put st
      _ → pure unit
    pure next
  CC.ReceiveInput _ varMap next → do
    H.modify (_ { source = Port.extractAnyFilePath varMap })
    pure next
  CC.ReceiveOutput _ _ next →
    pure next
  CC.ReceiveState _ next →
    pure next
  CC.ReceiveDimensions dims reply → do
    pure $ reply
      if dims.width < 504.0 ∨ dims.height < 192.0
      then LOD.Low
      else LOD.High

evalComponent ∷ Query ~> DSL
evalComponent q = do
  n ← case q of
    SetOutput ty next → do
      options ← H.gets _.options
      case ty, options of
        DL.CSV, (Right _) → H.modify _{options = Left CSV.defaultOptions}
        DL.JSON, (Left _) → H.modify _{options = Right DL.initialJSONOptions}
        _, _ → pure unit
      pure next
    ModifyCSVOpts fn next →
      H.modify (_options ∘ _Left %~ fn) $> next
    ModifyJSONOpts fn next →
      H.modify (_options ∘ _Right %~ fn) $> next
    ToggleCompress next →
      H.modify (\st → st { compress = not st.compress }) $> next
    TargetTyped s next →
      H.modify (_ { targetName = Just s }) $> next
  H.raise CC.modelUpdate
  pure n
