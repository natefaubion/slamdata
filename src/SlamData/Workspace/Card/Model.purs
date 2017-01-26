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

module SlamData.Workspace.Card.Model
  ( AnyCardModel(..)
  , encode
  , decode
  , decodeCardModel
  , cardModelOfType
  , modelCardType
  , singletonDraftboard
  , singletonTabs
  , splitDraftboard
  , mirrorInParent
  , childDeckIds
  , updatePointer
  , setupLabeledFormInput
  , _SetupLabeledInput
  , setupTextLikeInput
  , _SetupTextLikeInput
  ) where

import SlamData.Prelude

import Data.Argonaut ((:=), (~>), (.?))
import Data.Argonaut as J
import Data.Rational ((%))

import Data.Array as Array
import Data.List as L
import Data.Lens (Traversal', wander)
import Data.Path.Pathy (fileName, runFileName)

import SlamData.FileSystem.Resource as R
import SlamData.Workspace.Card.CardType as CT
import SlamData.Workspace.Card.CardType.ChartType (ChartType(..))
import SlamData.Workspace.Card.CardType.FormInputType (FormInputType(..))
import SlamData.Workspace.Card.Ace.Model as Ace
import SlamData.Workspace.Card.Variables.Model as Variables
import SlamData.Workspace.Card.Table.Model as JT
import SlamData.Workspace.Card.Markdown.Model as MD
import SlamData.Workspace.Card.Chart.Model as Chart
import SlamData.Workspace.Card.Draftboard.Layout as Layout
import SlamData.Workspace.Card.Draftboard.Model as DB
import SlamData.Workspace.Card.Draftboard.Orientation as Orn
import SlamData.Workspace.Card.Draftboard.Pane as Pane
import SlamData.Workspace.Card.DownloadOptions.Component.State as DLO
import SlamData.Workspace.Card.Setups.Chart.Metric.Model as BuildMetric
import SlamData.Workspace.Card.Setups.Chart.Sankey.Model as BuildSankey
import SlamData.Workspace.Card.Setups.Chart.Gauge.Model as BuildGauge
import SlamData.Workspace.Card.Setups.Chart.Graph.Model as BuildGraph
import SlamData.Workspace.Card.Setups.Chart.Pie.Model as BuildPie
import SlamData.Workspace.Card.Setups.Chart.Bar.Model as BuildBar
import SlamData.Workspace.Card.Setups.Chart.Line.Model as BuildLine
import SlamData.Workspace.Card.Setups.Chart.Area.Model as BuildArea
import SlamData.Workspace.Card.Setups.Chart.Scatter.Model as BuildScatter
import SlamData.Workspace.Card.Setups.Chart.PivotTable.Model as BuildPivotTable
import SlamData.Workspace.Card.Setups.Chart.Funnel.Model as BuildFunnel
import SlamData.Workspace.Card.Setups.Chart.Radar.Model as BuildRadar
import SlamData.Workspace.Card.Setups.Chart.Boxplot.Model as BuildBoxplot
import SlamData.Workspace.Card.Setups.Chart.Heatmap.Model as BuildHeatmap
import SlamData.Workspace.Card.Setups.Chart.PunchCard.Model as BuildPunchCard
import SlamData.Workspace.Card.Setups.Chart.Candlestick.Model as BuildCandlestick
import SlamData.Workspace.Card.Setups.Chart.Parallel.Model as BuildParallel
import SlamData.Workspace.Card.Query.Model as Query
import SlamData.Workspace.Card.Port as Port
import SlamData.Workspace.Deck.DeckId (DeckId)
import SlamData.Workspace.Card.Setups.FormInput.Dropdown.Model as SetupDropdown
import SlamData.Workspace.Card.Setups.FormInput.Radio.Model as SetupRadio
import SlamData.Workspace.Card.Setups.FormInput.Checkbox.Model as SetupCheckbox
import SlamData.Workspace.Card.Setups.FormInput.Text.Model as SetupText
import SlamData.Workspace.Card.Setups.FormInput.Numeric.Model as SetupNumeric
import SlamData.Workspace.Card.Setups.FormInput.Date.Model as SetupDate
import SlamData.Workspace.Card.Setups.FormInput.Time.Model as SetupTime
import SlamData.Workspace.Card.Setups.FormInput.Datetime.Model as SetupDatetime
import SlamData.Workspace.Card.Setups.FormInput.Static.Model as SetupStatic
import SlamData.Workspace.Card.Setups.FormInput.Labeled.Model as SetupLabeled
import SlamData.Workspace.Card.Setups.FormInput.TextLike.Model as SetupTextLike
import SlamData.Workspace.Card.FormInput.Model as FormInput
import SlamData.Workspace.Card.Tabs.Model as Tabs

import Test.StrongCheck.Arbitrary as SC
import Test.StrongCheck.Gen as Gen

data AnyCardModel
  = Ace CT.AceMode Ace.Model
  | Search String
  | Chart Chart.Model
  | Markdown MD.Model
  | Table JT.Model
  | Download
  | Variables Variables.Model
  | Troubleshoot
  | Cache (Maybe String)
  | Open R.Resource
  | DownloadOptions DLO.State
  | Draftboard DB.Model
  | BuildMetric BuildMetric.Model
  | BuildSankey BuildSankey.Model
  | BuildGauge BuildGauge.Model
  | BuildGraph BuildGraph.Model
  | BuildPie BuildPie.Model
  | BuildBar BuildBar.Model
  | BuildLine BuildLine.Model
  | BuildArea BuildArea.Model
  | BuildScatter BuildScatter.Model
  | BuildPivotTable BuildPivotTable.Model
  | BuildFunnel BuildFunnel.Model
  | BuildRadar BuildRadar.Model
  | BuildBoxplot BuildBoxplot.Model
  | BuildHeatmap BuildHeatmap.Model
  | BuildPunchCard BuildPunchCard.Model
  | BuildCandlestick BuildCandlestick.Model
  | BuildParallel BuildParallel.Model
  | SetupDropdown SetupDropdown.Model
  | SetupRadio SetupRadio.Model
  | SetupCheckbox SetupCheckbox.Model
  | SetupText SetupText.Model
  | SetupNumeric SetupNumeric.Model
  | SetupDate SetupDate.Model
  | SetupTime SetupTime.Model
  | SetupDatetime SetupDatetime.Model
  | SetupStatic SetupStatic.Model
  | FormInput FormInput.Model
  | Tabs Tabs.Model

instance arbitraryAnyCardModel ∷ SC.Arbitrary AnyCardModel where
  arbitrary =
    Gen.oneOf (pure Download)
      [ Ace <$> SC.arbitrary <*> Ace.genModel
      , Search <$> SC.arbitrary
      , Chart <$> Chart.genModel
      , Markdown <$> MD.genModel
      , Table <$> JT.genModel
      , Variables <$> Variables.genModel
      , pure Troubleshoot
      , Cache <$> SC.arbitrary
      , Open <$> SC.arbitrary
      , Draftboard <$> DB.genModel
      , BuildMetric <$> BuildMetric.genModel
      , BuildSankey <$> BuildSankey.genModel
      , BuildGauge <$> BuildGauge.genModel
      , BuildGraph <$> BuildGraph.genModel
      , BuildPie <$> BuildPie.genModel
      , BuildRadar <$> BuildRadar.genModel
      , BuildBar <$> BuildBar.genModel
      , BuildLine <$> BuildLine.genModel
      , BuildArea <$> BuildArea.genModel
      , BuildScatter <$> BuildScatter.genModel
      , BuildFunnel <$> BuildFunnel.genModel
      , BuildBoxplot <$> BuildBoxplot.genModel
      , BuildHeatmap <$> BuildHeatmap.genModel
      , BuildPunchCard <$> BuildPunchCard.genModel
      , BuildCandlestick <$> BuildCandlestick.genModel
      , BuildParallel <$> BuildParallel.genModel
      , SetupDropdown <$> SetupDropdown.genModel
      , SetupRadio <$> SetupRadio.genModel
      , SetupCheckbox <$> SetupCheckbox.genModel
      , SetupText <$> SetupText.genModel
      , SetupNumeric <$> SetupNumeric.genModel
      , SetupDate <$> SetupDate.genModel
      , SetupTime <$> SetupTime.genModel
      , SetupDatetime <$> SetupDatetime.genModel
      , SetupStatic <$> SetupStatic.genModel
      , FormInput <$> FormInput.genModel
      , Tabs <$> Tabs.genModel
      ]

instance eqAnyCardModel ∷ Eq AnyCardModel where
  eq = case _, _ of
    Ace x1 y1, Ace x2 y2 → x1 ≡ x2 && Ace.eqModel y1 y2
    Search s1, Search s2 → s1 ≡ s2
    Chart x, Chart y → Chart.eqModel x y
    Markdown x, Markdown y → MD.eqModel x y
    Table x, Table y → JT.eqModel x y
    Download, Download → true
    Variables x, Variables y → Variables.eqModel x y
    Troubleshoot, Troubleshoot → true
    Cache x, Cache y → x ≡ y
    Open x, Open y → x ≡ y
    DownloadOptions x, DownloadOptions y → DLO.eqState x y
    Draftboard x, Draftboard y → DB.eqModel x y
    BuildMetric x, BuildMetric y → BuildMetric.eqModel x y
    BuildSankey x, BuildSankey y → BuildSankey.eqModel x y
    BuildGauge x, BuildGauge y → BuildGauge.eqModel x y
    BuildGraph x, BuildGraph y → BuildGraph.eqModel x y
    BuildPie x, BuildPie y → BuildPie.eqModel x y
    BuildRadar x, BuildRadar y → BuildRadar.eqModel x y
    BuildBar x, BuildBar y → BuildBar.eqModel x y
    BuildLine x, BuildLine y → BuildLine.eqModel x y
    BuildArea x, BuildArea y → BuildArea.eqModel x y
    BuildScatter x, BuildScatter y → BuildScatter.eqModel x y
    BuildFunnel x, BuildFunnel y → BuildFunnel.eqModel x y
    BuildBoxplot x, BuildBoxplot y → BuildBoxplot.eqModel x y
    BuildHeatmap x, BuildHeatmap y → BuildHeatmap.eqModel x y
    BuildPunchCard x, BuildPunchCard y → BuildPunchCard.eqModel x y
    BuildCandlestick x, BuildCandlestick y → BuildCandlestick.eqModel x y
    BuildParallel x, BuildParallel y → BuildParallel.eqModel x y
    SetupDropdown x, SetupDropdown y → SetupDropdown.eqModel x y
    SetupRadio x, SetupRadio y → SetupRadio.eqModel x y
    SetupCheckbox x, SetupCheckbox y → SetupCheckbox.eqModel x y
    SetupText x, SetupText y → SetupText.eqModel x y
    SetupNumeric x, SetupNumeric y → SetupNumeric.eqModel x y
    SetupDate x, SetupDate y → SetupDate.eqModel x y
    SetupTime x, SetupTime y → SetupTime.eqModel x y
    SetupDatetime x, SetupDatetime y → SetupDatetime.eqModel x y
    SetupStatic x, SetupStatic y → SetupStatic.eqModel x y
    FormInput x, FormInput y → FormInput.eqModel x y
    Tabs x, Tabs y → Tabs.eqModel x y
    _, _ → false

instance encodeJsonCardModel ∷ J.EncodeJson AnyCardModel where
  encodeJson = encodeCardModel

modelCardType ∷ AnyCardModel → CT.CardType
modelCardType = case _ of
  Ace mode _ → CT.Ace mode
  Search _ → CT.Search
  BuildMetric _ → CT.ChartOptions Metric
  BuildSankey _ → CT.ChartOptions Sankey
  BuildGauge _ → CT.ChartOptions Gauge
  BuildGraph _ → CT.ChartOptions Graph
  BuildPie _ → CT.ChartOptions Pie
  BuildRadar _ → CT.ChartOptions Radar
  BuildBar _ → CT.ChartOptions Bar
  BuildLine _ → CT.ChartOptions Line
  BuildArea _ → CT.ChartOptions Area
  BuildScatter _ → CT.ChartOptions Scatter
  BuildPivotTable _ → CT.ChartOptions PivotTable
  BuildFunnel _ → CT.ChartOptions Funnel
  BuildBoxplot _ → CT.ChartOptions Boxplot
  BuildHeatmap _ → CT.ChartOptions Heatmap
  BuildPunchCard _ → CT.ChartOptions PunchCard
  BuildCandlestick _ → CT.ChartOptions Candlestick
  BuildParallel _ → CT.ChartOptions Parallel
  Chart _ → CT.Chart
  Markdown _ → CT.Markdown
  Table _ → CT.Table
  Download → CT.Download
  Variables _ → CT.Variables
  Troubleshoot → CT.Troubleshoot
  Cache _ → CT.Cache
  Open _ → CT.Open
  DownloadOptions _ → CT.DownloadOptions
  Draftboard _ → CT.Draftboard
  SetupDropdown _ → CT.SetupFormInput Dropdown
  SetupRadio _ → CT.SetupFormInput Radio
  SetupCheckbox _ → CT.SetupFormInput Checkbox
  SetupText _ → CT.SetupFormInput Text
  SetupNumeric _ → CT.SetupFormInput Numeric
  SetupDate _ → CT.SetupFormInput Date
  SetupTime _ → CT.SetupFormInput Time
  SetupDatetime _ → CT.SetupFormInput Datetime
  SetupStatic _ → CT.SetupFormInput Static
  FormInput _ → CT.FormInput
  Tabs _ → CT.Tabs

encode ∷ AnyCardModel → J.Json
encode card =
  "cardType" := modelCardType card
    ~> "model" := encodeCardModel card
    ~> J.jsonEmptyObject

decode ∷ J.Json → Either String AnyCardModel
decode js = do
  obj ← J.decodeJson js
  cardType ← obj .? "cardType"
  model ← obj .? "model"
  decodeCardModel cardType model

encodeCardModel ∷ AnyCardModel → J.Json
encodeCardModel = case _ of
  Ace mode model → Ace.encode model
  Search txt → J.encodeJson txt
  Chart model → Chart.encode model
  Markdown model → MD.encode model
  Table model → JT.encode model
  Download → J.jsonEmptyObject
  Variables model → Variables.encode model
  Troubleshoot → J.jsonEmptyObject
  Cache model → J.encodeJson model
  Open res → J.encodeJson res
  DownloadOptions model → DLO.encode model
  Draftboard model → DB.encode model
  BuildMetric model → BuildMetric.encode model
  BuildSankey model → BuildSankey.encode model
  BuildGauge model → BuildGauge.encode model
  BuildGraph model → BuildGraph.encode model
  BuildPie model → BuildPie.encode model
  BuildRadar model → BuildRadar.encode model
  BuildBar model → BuildBar.encode model
  BuildLine model → BuildLine.encode model
  BuildArea model → BuildArea.encode model
  BuildScatter model → BuildScatter.encode model
  BuildPivotTable model → BuildPivotTable.encode model
  BuildFunnel model → BuildFunnel.encode model
  BuildBoxplot model → BuildBoxplot.encode model
  BuildHeatmap model → BuildHeatmap.encode model
  BuildPunchCard model → BuildPunchCard.encode model
  BuildCandlestick model → BuildCandlestick.encode model
  BuildParallel model → BuildParallel.encode model
  SetupDropdown model → SetupDropdown.encode model
  SetupRadio model → SetupRadio.encode model
  SetupCheckbox model → SetupCheckbox.encode model
  SetupText model → SetupText.encode model
  SetupNumeric model → SetupNumeric.encode model
  SetupDate model → SetupDate.encode model
  SetupTime model → SetupTime.encode model
  SetupDatetime model → SetupDatetime.encode model
  SetupStatic model → SetupStatic.encode model
  FormInput model → FormInput.encode model
  Tabs model → Tabs.encode model

decodeCardModel
  ∷ CT.CardType
  → J.Json
  → Either String AnyCardModel
decodeCardModel = case _ of
  CT.Ace mode → map (Ace mode) ∘ Ace.decode
  CT.Search → map Search ∘ J.decodeJson
  CT.ChartOptions Metric → map BuildMetric ∘ BuildMetric.decode
  CT.ChartOptions Sankey → map BuildSankey ∘ BuildSankey.decode
  CT.ChartOptions Gauge → map BuildGauge ∘ BuildGauge.decode
  CT.ChartOptions Graph → map BuildGraph ∘ BuildGraph.decode
  CT.ChartOptions Pie → map BuildPie ∘ BuildPie.decode
  CT.ChartOptions Radar → map BuildRadar ∘ BuildRadar.decode
  CT.ChartOptions Bar → map BuildBar ∘ BuildBar.decode
  CT.ChartOptions Line → map BuildLine ∘ BuildLine.decode
  CT.ChartOptions Area → map BuildArea ∘ BuildArea.decode
  CT.ChartOptions Scatter → map BuildScatter ∘ BuildScatter.decode
  CT.ChartOptions PivotTable → map BuildPivotTable ∘ BuildPivotTable.decode
  CT.ChartOptions Funnel → map BuildFunnel ∘ BuildFunnel.decode
  CT.ChartOptions Boxplot → map BuildBoxplot ∘ BuildBoxplot.decode
  CT.ChartOptions Heatmap → map BuildHeatmap ∘ BuildHeatmap.decode
  CT.ChartOptions PunchCard → map BuildPunchCard ∘ BuildPunchCard.decode
  CT.ChartOptions Candlestick → map BuildCandlestick ∘ BuildCandlestick.decode
  CT.ChartOptions Parallel → map BuildParallel ∘ BuildParallel.decode
  CT.SetupFormInput Dropdown → map SetupDropdown ∘ SetupDropdown.decode
  CT.SetupFormInput Radio → map SetupRadio ∘ SetupRadio.decode
  CT.SetupFormInput Checkbox → map SetupCheckbox ∘ SetupCheckbox.decode
  CT.SetupFormInput Text → map SetupText ∘ SetupText.decode
  CT.SetupFormInput Numeric → map SetupNumeric ∘ SetupNumeric.decode
  CT.SetupFormInput Date → map SetupDate ∘ SetupDate.decode
  CT.SetupFormInput Time → map SetupTime ∘ SetupTime.decode
  CT.SetupFormInput Datetime → map SetupDatetime ∘ SetupDatetime.decode
  CT.SetupFormInput Static → map SetupStatic ∘ SetupStatic.decode
  CT.FormInput → map FormInput ∘ FormInput.decode
  CT.Chart → map Chart ∘ Chart.decode
  CT.Markdown → map Markdown ∘ MD.decode
  CT.Table → map Table ∘ JT.decode
  CT.Download → const $ pure Download
  CT.Variables → map Variables ∘ Variables.decode
  CT.Troubleshoot → const $ pure Troubleshoot
  CT.Cache → map Cache ∘ J.decodeJson
  CT.Open → map Open ∘ decodeOpen
  CT.DownloadOptions → map DownloadOptions ∘ DLO.decode
  CT.Draftboard → map Draftboard ∘ DB.decode
  CT.Tabs → map Tabs ∘ Tabs.decode

  where
    -- For backwards compat
    decodeOpen j =
      J.decodeJson j
      <|> (map (fromMaybe R.root) $ J.decodeJson j)

cardModelOfType ∷ Port.Out → CT.CardType → AnyCardModel
cardModelOfType (port × varMap) = case _ of
  CT.Ace CT.SQLMode → Ace CT.SQLMode (Query.initialModel port)
  CT.Ace mode → Ace mode Ace.emptyModel
  CT.Search → Search ""
  CT.ChartOptions Metric → BuildMetric BuildMetric.initialModel
  CT.ChartOptions Sankey → BuildSankey BuildSankey.initialModel
  CT.ChartOptions Gauge → BuildGauge BuildGauge.initialModel
  CT.ChartOptions Graph → BuildGraph BuildGraph.initialModel
  CT.ChartOptions Pie → BuildPie BuildPie.initialModel
  CT.ChartOptions Radar → BuildRadar BuildRadar.initialModel
  CT.ChartOptions Bar → BuildBar BuildBar.initialModel
  CT.ChartOptions Line → BuildLine BuildLine.initialModel
  CT.ChartOptions Area → BuildArea BuildArea.initialModel
  CT.ChartOptions Scatter → BuildScatter BuildScatter.initialModel
  CT.ChartOptions PivotTable → BuildPivotTable BuildPivotTable.initialModel
  CT.ChartOptions Funnel → BuildFunnel BuildFunnel.initialModel
  CT.ChartOptions Boxplot → BuildBoxplot BuildBoxplot.initialModel
  CT.ChartOptions Heatmap → BuildHeatmap BuildHeatmap.initialModel
  CT.ChartOptions PunchCard → BuildPunchCard BuildPunchCard.initialModel
  CT.ChartOptions Candlestick → BuildCandlestick BuildCandlestick.initialModel
  CT.ChartOptions Parallel → BuildParallel BuildParallel.initialModel
  CT.SetupFormInput Dropdown → SetupDropdown SetupDropdown.initialModel
  CT.SetupFormInput Radio → SetupRadio SetupRadio.initialModel
  CT.SetupFormInput Checkbox → SetupCheckbox SetupCheckbox.initialModel
  CT.SetupFormInput Text → SetupText SetupText.initialModel
  CT.SetupFormInput Numeric → SetupNumeric SetupNumeric.initialModel
  CT.SetupFormInput Date → SetupDate SetupDate.initialModel
  CT.SetupFormInput Time → SetupTime SetupTime.initialModel
  CT.SetupFormInput Datetime → SetupDatetime SetupDatetime.initialModel
  CT.SetupFormInput Static → SetupStatic SetupStatic.initialModel
  CT.FormInput → FormInput FormInput.initialModel
  CT.Chart → Chart Chart.emptyModel
  CT.Markdown → Markdown MD.emptyModel
  CT.Table → Table JT.emptyModel
  CT.Download → Download
  CT.Variables → Variables Variables.emptyModel
  CT.Troubleshoot → Troubleshoot
  CT.Cache → Cache Nothing
  CT.Open → Open R.root
  CT.DownloadOptions → DownloadOptions $ DLO.initialState (runFileName ∘ fileName <$> Port.extractFilePath varMap)
  CT.Draftboard → Draftboard DB.emptyModel
  CT.Tabs → Tabs Tabs.initialModel

singletonDraftboard ∷ DeckId → AnyCardModel
singletonDraftboard deckId =
  Draftboard { layout: Pane.Cell (Just deckId) }

singletonTabs ∷ DeckId → AnyCardModel
singletonTabs deckId =
  Tabs { tabs: pure deckId }

splitDraftboard ∷ Orn.Orientation → L.List DeckId → AnyCardModel
splitDraftboard orn deckIds =
  Draftboard { layout: Pane.Split orn (mkCell <$> deckIds) }
  where
    size =
      L.length deckIds

    mkCell deckId =
      (1 % size) × Pane.wrap (Orn.reverse orn) (Pane.Cell (Just deckId))

mirrorInParent ∷ DeckId → DeckId → AnyCardModel → Maybe AnyCardModel
mirrorInParent oldId newId = case _ of
  Draftboard { layout } → do
    cursor ← Pane.getCursorFor (Just oldId) layout
    let
      cursor' = fromMaybe L.Nil (L.tail cursor)
      orn = case Pane.getAt cursor' layout of
        Just (Pane.Split o _) → o
        _ → Orn.Vertical
    layout' ←
      Layout.insertSplit
        (Pane.Cell (Just newId)) orn (1 % 2) Layout.SideB cursor' layout
    pure (Draftboard { layout: layout' })
  Tabs { tabs } →
    let
      tabs' = do
        deckId ← tabs
        if deckId ≡ oldId
          then [ deckId, newId ]
          else [ deckId ]
    in
      pure (Tabs { tabs: tabs' })
  _ → Nothing

childDeckIds ∷ AnyCardModel → L.List DeckId
childDeckIds = case _ of
  Draftboard { layout } → L.catMaybes (L.fromFoldable layout)
  Tabs { tabs } → L.fromFoldable tabs
  s → mempty

updatePointer
  ∷ DeckId
  → Maybe DeckId
  → AnyCardModel
  → AnyCardModel
updatePointer old new = case _ of
  Draftboard { layout } →
    Draftboard { layout: update <$> layout}
  Tabs { tabs } →
    Tabs { tabs: Array.mapMaybe (update ∘ Just) tabs }
  card → card

  where
    update (Just deckId) | deckId ≡ old = new
    update a = a

-- TODO: this should live somewhere else and actually, FormInputType
-- is LabeledInputType ⊹ TextLikeInputType ⊹ Static. They all are handled differently
_SetupLabeledInput ∷ Traversal' AnyCardModel SetupLabeled.Model
_SetupLabeledInput = wander \f s → case s of
  SetupDropdown m → map SetupDropdown $ f m
  SetupRadio m → map SetupRadio $ f m
  SetupCheckbox m → map SetupCheckbox $ f m
  _ → pure s

setupLabeledFormInput ∷ FormInputType → SetupLabeled.Model → AnyCardModel
setupLabeledFormInput fit m = case fit of
  Dropdown → SetupDropdown m
  Radio → SetupRadio m
  Checkbox → SetupCheckbox m
  _ → SetupDropdown m

_SetupTextLikeInput ∷ Traversal' AnyCardModel SetupTextLike.Model
_SetupTextLikeInput = wander \f s → case s of
  SetupText m → map SetupText $ f m
  SetupNumeric m → map SetupNumeric $ f m
  SetupDate m → map SetupDate $ f m
  SetupTime m → map SetupTime $ f m
  SetupDatetime m → map SetupDatetime $ f m
  _ → pure s

setupTextLikeInput ∷ FormInputType → SetupTextLike.Model → AnyCardModel
setupTextLikeInput fit m = case fit of
  Text → SetupText m
  Numeric → SetupNumeric m
  Date → SetupDate m
  Time → SetupTime m
  Datetime → SetupDatetime m
  _ → SetupText m
