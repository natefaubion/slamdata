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

module SlamData.Workspace.Card.DownloadOptions.Component (comp) where

import SlamData.Prelude

import Data.Lens ((.~), _Left, _Right, (%~))

import Halogen as H
import Halogen.HTML.Events.Indexed as HE
import Halogen.HTML.Indexed as HH
import Halogen.HTML.Properties.Indexed as HP
import Halogen.Themes.Bootstrap3 as B

import SlamData.Download.Model as DL
import SlamData.Download.Render as DLR
import SlamData.Monad (Slam)
import SlamData.Render.CSS as RC
import SlamData.Workspace.Card.CardType (CardType(DownloadOptions))
import SlamData.Workspace.Card.CardType as CT
import SlamData.Workspace.Card.Component as CC
import SlamData.Workspace.Card.Common.Render (renderLowLOD)
import SlamData.Workspace.Card.DownloadOptions.Component.Query (QueryP, Query(..))
import SlamData.Workspace.Card.DownloadOptions.Component.State (State, _compress, _options, _source, initialState, _levelOfDetails)
import SlamData.Workspace.Card.Model as Card
import SlamData.Workspace.Card.Port as Port
import SlamData.Workspace.LevelOfDetails (LevelOfDetails(..))

type HTML = H.ComponentHTML QueryP
type DSL = H.ComponentDSL State QueryP Slam

comp ∷ CC.CardOptions → CC.CardComponent
comp options = CC.makeCardComponent
  { options
  , cardType: DownloadOptions
  , component: H.component {render, eval}
  , initialState: initialState Nothing
  , _State: CC._DownloadOptionsState
  , _Query: CC.makeQueryPrism CC._DownloadOptionsQuery
  }

render ∷ State → HTML
render state =
  HH.div_
    [ renderHighLOD state
    , renderLowLOD (CT.cardIconLightImg CT.DownloadOptions) left state.levelOfDetails
    ]

renderHighLOD ∷ State → HTML
renderHighLOD state =
  case state.source of
    Nothing →
      HH.div
        [ HP.classes
            $ [ B.alert, B.alertDanger ]
            ⊕ hideClasses
        ]
        [ HH.text "The current input cannot be downloaded" ]
    Just _ →
      HH.div
        [ HP.classes
            $ [ RC.downloadCardEditor
              , HH.className "card-input-maximum-lod"
              ]
            ⊕ hideClasses
        ]
        [ renderDownloadTypeSelector state
        , renderDownloadConfiguration state
        ]
  where
  hideClasses = guard (state.levelOfDetails ≠ High) $> B.hidden

renderDownloadConfiguration ∷ State → HTML
renderDownloadConfiguration state =
  HH.div
    [ HP.classes [ RC.downloadConfiguration ] ]
    [ DLR.fldName state.options (fromMaybe "" state.targetName) (\s → right ∘ TargetTyped s)
    , either optionsCSV optionsJSON state.options
    ]

optionsCSV ∷ DL.CSVOptions → HTML
optionsCSV = DLR.optionsCSV (\lens v → right ∘ (ModifyCSVOpts (lens .~ v)))

optionsJSON ∷ DL.JSONOptions → HTML
optionsJSON = DLR.optionsJSON (\lens v → right ∘ (ModifyJSONOpts (lens .~ v)))

compress ∷ State → HTML
compress state =
  HH.div
    [ HP.classes [ B.formGroup ] ]
    [ HH.label_
        [ HH.span_ [ HH.text "Compress" ]
        , HH.input
            [ HP.inputType HP.InputCheckbox
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
    [ HP.classes [ RC.downloadTypeSelector ] ]
    [ HH.img
        [ HP.src "img/csv.svg"
        , HP.classes (guard (isLeft state.options) $> B.active)
        , HP.title "Comma separated values"
        , HE.onClick (HE.input_ (right ∘ SetOutput DL.CSV))
        ]
    , HH.img
        [ HP.src "img/json.svg"
        , HP.classes (guard (isRight state.options) $> B.active)
        , HP.title "JSON"
        , HE.onClick (HE.input_ (right ∘ SetOutput DL.JSON))
        ]
    ]

eval ∷ QueryP ~> DSL
eval = coproduct cardEval downloadOptsEval

cardEval ∷ CC.CardEvalQuery ~> DSL
cardEval = case _ of
  CC.Activate next →
    pure next
  CC.Deactivate next →
    pure next
  CC.Save k →
    map (k ∘ Card.DownloadOptions) H.get
  CC.Load card next → do
    case card of
      Card.DownloadOptions st → H.set st
      _ → pure unit
    pure next
  CC.ReceiveInput _ varMap next → do
    H.modify $ _source .~ Port.extractFilePath varMap
    pure next
  CC.ReceiveOutput _ _ next →
    pure next
  CC.ReceiveState _ next →
    pure next
  CC.ReceiveDimensions dims next → do
    H.modify
      $ _levelOfDetails
      .~ if dims.width < 504.0 ∨ dims.height < 192.0
           then Low
           else High
    pure next
  CC.ModelUpdated _ next →
    pure next
  CC.ZoomIn next →
    pure next

downloadOptsEval ∷ Query ~> DSL
downloadOptsEval q = do
  n ← case q of
    SetOutput ty next → do
      options ← H.gets _.options
      case ty, options of
        DL.CSV, (Right _) → H.modify _{options = Left DL.initialCSVOptions}
        DL.JSON, (Left _) → H.modify _{options = Right DL.initialJSONOptions}
        _, _ → pure unit
      pure next
    ModifyCSVOpts fn next →
      H.modify (_options ∘ _Left %~ fn) $> next
    ModifyJSONOpts fn next →
      H.modify (_options ∘ _Right %~ fn) $> next
    ToggleCompress next →
      H.modify (_compress %~ not) $> next
    TargetTyped s next →
      H.modify _ { targetName = Just s } $> next
  CC.raiseUpdatedC' CC.EvalModelUpdate
  pure n
