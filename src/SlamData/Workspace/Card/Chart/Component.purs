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

module SlamData.Workspace.Card.Chart.Component (chartComponent) where

import SlamData.Prelude

import Data.Array as A
import Data.Foreign (Foreign, ForeignError(TypeMismatch), readInt, readString)
import Data.Foreign.Class (readProp)
import Data.Int (toNumber, floor)
import Data.Lens ((.~), (?~))
import Data.String as Str

import ECharts.Monad (buildObj)

import Global (readFloat, isNaN)

import Halogen as H
import Halogen.ECharts as HEC
import Halogen.HTML.Events.Indexed as HE
import Halogen.HTML.Indexed as HH
import Halogen.HTML.Properties.Indexed as HP
import Halogen.HTML.Properties.Indexed.ARIA as ARIA
import Halogen.Themes.Bootstrap3 as B

import SlamData.Monad (Slam)
import SlamData.Render.CSS as RC
import SlamData.Workspace.Card.CardType as CT
import SlamData.Workspace.Card.CardType.ChartType (ChartType, chartDarkIconSrc)
import SlamData.Workspace.Card.CardType.ChartType as ChT
import SlamData.Workspace.Card.Chart.Component.ChildSlot (cpMetric, cpPivotTable, cpECharts, ChildState, ChildQuery, ChildSlot)
import SlamData.Workspace.Card.Chart.Component.State (State, initialState, _levelOfDetails, _chartType)
import SlamData.Workspace.Card.Chart.Model as Chart
import SlamData.Workspace.Card.Chart.MetricRenderer.Component as Metric
import SlamData.Workspace.Card.Chart.PivotTableRenderer.Component as Pivot
import SlamData.Workspace.Card.Component as CC
import SlamData.Workspace.Card.Model as Card
import SlamData.Workspace.Card.Port (Port(..))
import SlamData.Workspace.LevelOfDetails (LevelOfDetails(..))

type HTML =
  H.ParentHTML ChildState CC.CardEvalQuery ChildQuery Slam ChildSlot
type DSL =
  H.ParentDSL State ChildState CC.CardEvalQuery ChildQuery Slam ChildSlot

chartComponent ∷ H.Component CC.CardStateP CC.CardQueryP Slam
chartComponent = CC.makeCardComponent
  { cardType: CT.Chart
  , component: H.parentComponent { render, eval, peek: Just (peek ∘ H.runChildF) }
  , initialState: H.parentState initialState
  , _State: CC._ChartState
  , _Query: CC.makeQueryPrism CC._ChartQuery
  }

render ∷ State → HTML
render state =
  HH.div_
    [ renderHighLOD state
    , renderLowLOD state
    ]

renderHighLOD ∷ State → HTML
renderHighLOD state =
  HH.div
    [ HP.classes
        $ [ RC.chartOutput, HH.className "card-input-maximum-lod" ]
        ⊕ (guard (state.levelOfDetails ≠ High) $> B.hidden)
    ]
    case state.chartType of
      Just ChT.Metric →
        [ HH.slot' cpMetric unit \_ →
             { component: Metric.comp
             , initialState: Metric.initialState
             }
        ]
      Just ChT.PivotTable →
        [ HH.slot' cpPivotTable unit \_ →
             { component: Pivot.comp
             , initialState: Pivot.initialState
             }
        ]
      _ →
        [ HH.slot' cpECharts unit \_ →
            { component: HEC.echarts
            , initialState: HEC.initialEChartsState 600 400
            }
        ]

renderLowLOD ∷ State → HTML
renderLowLOD state =
  HH.div
    [ HP.classes
        $ [ HH.className "card-input-minimum-lod" ]
        ⊕ (guard (state.levelOfDetails ≠ Low) $> B.hidden)
    ]
    [ HH.button
      [ ARIA.label "Zoom or resize"
      , HP.title "Zoom or resize"
      , HE.onClick (HE.input_ CC.ZoomIn)
      ]
      $ foldMap renderButton state.chartType
    ]


renderButton ∷ ChartType → Array HTML
renderButton ct =
  [ HH.img [ HP.src $ chartDarkIconSrc ct ]
  , HH.text "Zoom or resize"
  ]

eval ∷ CC.CardEvalQuery ~> DSL
eval = case _ of
  CC.EvalCard value output next → do
    case value.input of
      Just (ChartInstructions _ opts chartType) → void do
        H.modify $ _chartType ?~ chartType
        H.query' cpECharts unit $ H.action $ HEC.Reset opts
        H.query' cpECharts unit $ H.action HEC.Resize
        setEChartsLOD $ buildObj opts
      Just (Metric metric) → void do
        H.modify $ _chartType ?~ ChT.Metric
        H.query' cpMetric unit $ H.action $ Metric.SetMetric metric
        setMetricLOD
      Just (PivotTable r) → void do
        H.modify $ _chartType ?~ ChT.PivotTable
        H.query' cpPivotTable unit $ H.action $ Pivot.Update r
      _ →
        void $ H.query' cpECharts unit $ H.action HEC.Clear
    pure next
  CC.Activate next →
    pure next
  CC.Deactivate next →
    pure next
  CC.Save k →
    H.gets _.chartType >>= case _ of
      Just ChT.PivotTable → do
        res ← H.query' cpPivotTable unit $ H.request Pivot.Save
        pure $ k (Card.Chart (Chart.PivotTableRenderer <$> res))
      _ →
        pure $ k (Card.Chart Nothing)
  CC.Load model next →
    case model of
      Card.Chart (Just (Chart.PivotTableRenderer m)) → do
        H.modify $ _chartType ?~ ChT.PivotTable
        H.query' cpPivotTable unit $ H.action $ Pivot.Load m
        pure next
      _ →
        pure next
  CC.SetDimensions dims next → do
    state ← H.get
    let
      heightPadding = 60
      widthPadding = 6
      intWidth = floor dims.width - widthPadding
      intHeight = floor dims.height

    H.query' cpMetric unit $ H.action $ Metric.SetDimensions {width: intWidth, height: intHeight}

    when (state.width ≠ intWidth) do
      H.query' cpECharts unit $ H.action $ HEC.SetWidth $ intWidth
      H.modify _{ width = intWidth }
    when (state.height ≠ intHeight) do
      H.query' cpECharts unit $ H.action $ HEC.SetHeight $ intHeight - heightPadding
      H.modify _{ height = intHeight }

    for_ state.chartType case _ of
      ChT.Metric →
        setMetricLOD
      _ → do
        mbOpts ← H.query' cpECharts unit $ H.request HEC.GetOptions
        for_ (join mbOpts) setEChartsLOD

    pure next
  CC.ModelUpdated _ next →
    pure next
  CC.ZoomIn next →
    pure next

setMetricLOD ∷ DSL Unit
setMetricLOD = do
  mbLod ← H.query' cpMetric unit $ H.request Metric.GetLOD
  for_ mbLod \lod → H.modify _{levelOfDetails = lod}

setEChartsLOD ∷ Foreign → DSL Unit
setEChartsLOD fOption = do
  state ← H.get
  let
    eBottom = do
      grids ← readProp "grid" fOption
      grid ← maybe (Left $ TypeMismatch "Array of grids" "Empty array") Right $ A.head grids
      readProp "bottom" grid

    eBottomPx = either (const Nothing) Just $ readInt =<< eBottom

    eBottomPct = do
      pctStr ← either (const Nothing) Just $ readString =<< eBottom
      str ← Str.stripSuffix "%" pctStr
      let num = readFloat str
      guard (not $ isNaN num)
      pure $ floor $ num / 100.0 * toNumber state.height

  for_ (eBottomPx <|> eBottomPct <|> pure zero) \bottomPx →
    H.modify
    $ _levelOfDetails
    .~ if (state.height - bottomPx) < 200 ∨ state.width < 300
         then Low
         else High

peek ∷ ∀ a. ChildQuery a → DSL Unit
peek = const (pure unit) ⨁  peekPivotTable ⨁  const (pure unit)
  where
  peekPivotTable = case _ of
    Pivot.ModelUpdated _ → CC.raiseUpdatedP CC.EvalModelUpdate
    _ → pure unit
