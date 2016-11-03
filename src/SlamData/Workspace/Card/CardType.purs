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

module SlamData.Workspace.Card.CardType
  ( CardType(..)
  , AceMode(..)
  , cardName
  , darkCardGlyph
  , lightCardGlyph
  , cardClasses
  , aceCardName
  , aceCardClasses
  , aceMode
  , controllable
  , blocking
  ) where

import SlamData.Prelude

import Data.Argonaut (class EncodeJson, class DecodeJson, encodeJson, decodeJson)
import Data.String as Str

import Halogen.HTML as H
import Halogen.HTML.Indexed as HH
import Halogen.HTML.Properties.Indexed as HP

import SlamData.Workspace.Card.CardType.ChartType
  ( ChartType(..)
  , printChartType
  , parseChartType
  , chartName
  )

import Test.StrongCheck.Arbitrary as SC

data CardType
  = Ace AceMode
  | Search
  | ChartOptions ChartType
  | Chart
  | Markdown
  | Table
  | Download
  | Variables
  | Troubleshoot
  | NextAction
  | Cache
  | Open
  | DownloadOptions
  | Draftboard
  | ErrorCard
  | PendingCard

derive instance eqCardType ∷ Eq CardType
derive instance ordCardType ∷ Ord CardType

data AceMode
  = MarkdownMode
  | SQLMode

derive instance eqAceMode ∷ Eq AceMode
derive instance ordAceMode ∷ Ord AceMode

instance showAceMode ∷ Show AceMode where
  show MarkdownMode = "MarkdownMode"
  show SQLMode = "SQLMode"

instance arbitraryAceMode ∷ SC.Arbitrary AceMode where
  arbitrary = do
    b ← SC.arbitrary
    pure $ if b then MarkdownMode else SQLMode

instance encodeJsonCardType ∷ EncodeJson CardType where
  encodeJson = encodeJson ∘ case _ of
    Ace MarkdownMode → "ace-markdown"
    Ace SQLMode → "ace-sql"
    Search → "search"
    ChartOptions chty → printChartType chty ⊕ "-options"
    Chart → "chart"
    Markdown → "markdown"
    Table → "table"
    Download → "download"
    Variables → "variables"
    Troubleshoot → "troubleshoot"
    NextAction → "next-action"
    Cache → "cache"
    Open → "open"
    DownloadOptions → "download-options"
    Draftboard → "draftboard"
    ErrorCard → "error"
    PendingCard → "pending"

instance decodeJsonCardType ∷ DecodeJson CardType where
  decodeJson json = do
    str ← decodeJson json
    case str of
      "ace-markdown" → pure $ Ace MarkdownMode
      "ace-sql" → pure $ Ace SQLMode
      "search" → pure Search
      "chart" → pure Chart
      "markdown" → pure Markdown
      "table" → pure Table
      "download" → pure Download
      "variables" → pure Variables
      "troubleshoot" → pure Troubleshoot
      "next-action" → pure NextAction
      "cache" → pure Cache
      "open" → pure Open
      "download-options" → pure DownloadOptions
      "draftboard" → pure Draftboard
      "error" → pure ErrorCard
      "pending" → pure PendingCard
      name → do
        let
          chartName = fromMaybe "" $ Str.stripSuffix "-options" name
        chty ← lmap (const $ "unknown card type '" ⊕ name ⊕ "'") $ parseChartType chartName
        pure $ ChartOptions chty

cardName ∷ CardType → String
cardName = case _ of
  Ace at → aceCardName at
  Search → "Search"
  ChartOptions chty → chartName chty
  Chart → "Show Chart"
  Markdown → "Show Markdown"
  Table → "Preview Table"
  Download → "Show Download"
  Variables → "Setup Variables"
  Troubleshoot → "Troubleshoot"
  NextAction → "Next Action"
  Cache → "Cache"
  Open → "Open"
  DownloadOptions → "Setup Download"
  Draftboard → "Setup Dashboard"
  ErrorCard → "Error"
  PendingCard → "Pending"

darkCardGlyph ∷ ∀ s f. CardType → H.HTML s f
darkCardGlyph = case _ of
  Ace MarkdownMode →
    HH.img [ HP.src "img/cardsDark/setupMarkdown.svg" ]
  Ace SQLMode →
    HH.img [ HP.src "img/cardsDark/query.svg" ]
  Search →
    HH.img [ HP.src "img/cardsDark/search.svg" ]
  ChartOptions chty →
    let
      src = case chty of
        Pie → "img/cardsDark/buildChart/pie.svg"
        Line → "img/cardsDark/buildChart/line.svg"
        Bar → "img/cardsDark/buildChart/bar.svg"
        Area → "img/cardsDark/buildChart/area.svg"
        Scatter → "img/cardsDark/buildChart/scatter.svg"
        Radar → "img/cardsDark/buildChart/radar.svg"
        Funnel → "img/cardsDark/buildChart/funnel.svg"
        Graph → "img/cardsDark/buildChart/graph.svg"
        Heatmap → "img/cardsDark/buildChart/heatmap.svg"
        Sankey → "img/cardsDark/buildChart/sankey.svg"
        Gauge → "img/cardsDark/buildChart/gauge.svg"
        Boxplot → "img/cardsDark/buildChart/boxplot.svg"
        Metric → "img/cardsDark/buildChart/metric.svg"
        PivotTable → "img/cardsDark/buildChart/pivot-table.svg"
        PunchCard → "img/cardsDark/buildChart/punch-card.svg"
        Candlestick → "img/cardsDark/buildChart/candlestick.svg"
        Parallel → "img/cardsDark/buildChart/parallel.svg"
    in HH.img [ HP.src src ]
  Download →
    HH.img [ HP.src "img/cardsDark/showDownload.svg" ]
  Variables →
    HH.img [ HP.src "img/cardsDark/setupVariables.svg" ]
  Troubleshoot →
    HH.img [ HP.src "img/cardsDark/troubleshoot.svg" ]
  Chart →
    HH.img [ HP.src "img/cardsDark/showChart.svg" ]
  Markdown →
    HH.img [ HP.src "img/cardsDark/showMarkdown.svg" ]
  Table →
    HH.img [ HP.src "img/cardsDark/table.svg" ]
  NextAction →
    HH.text ""
  Cache →
    HH.img [ HP.src "img/cardsDark/cache.svg" ]
  Open →
    HH.img [ HP.src "img/cardsDark/open.svg" ]
  DownloadOptions →
    HH.img [ HP.src "img/cardsDark/setupDownload.svg" ]
  Draftboard →
    HH.img [ HP.src "img/cardsDark/draftboard.svg" ]
  ErrorCard →
    HH.text ""
  PendingCard →
    HH.text ""

lightCardGlyph ∷ ∀ s f. CardType → H.HTML s f
lightCardGlyph = case _ of
  Ace MarkdownMode →
    HH.img [ HP.src "img/cardsLight/setupMarkdown.svg" ]
  Ace SQLMode →
    HH.img [ HP.src "img/cardsLight/query.svg" ]
  Search →
    HH.img [ HP.src "img/cardsLight/search.svg" ]
  ChartOptions chty →
    let
      src = case chty of
        Pie → "img/cardsLight/buildChart/pie.svg"
        Line → "img/cardsLight/buildChart/line.svg"
        Bar → "img/cardsLight/buildChart/bar.svg"
        Area → "img/cardsLight/buildChart/area.svg"
        Scatter → "img/cardsLight/buildChart/scatter.svg"
        Radar → "img/cardsLight/buildChart/radar.svg"
        Funnel → "img/cardsLight/buildChart/funnel.svg"
        Graph → "img/cardsLight/buildChart/graph.svg"
        Heatmap → "img/cardsLight/buildChart/heatmap.svg"
        Sankey → "img/cardsLight/buildChart/sankey.svg"
        Gauge → "img/cardsLight/buildChart/gauge.svg"
        Boxplot → "img/cardsLight/buildChart/boxplot.svg"
        Metric → "img/cardsLight/buildChart/metric.svg"
        PivotTable → "img/cardsLight/buildChart/pivot-table.svg"
        PunchCard → "img/cardsLight/buildChart/punch-card.svg"
        Candlestick → "img/cardsLight/buildChart/candlestick.svg"
        Parallel → "img/cardsLight/buildChart/parallel.svg"
    in HH.img [ HP.src src ]
  Download →
    HH.img [ HP.src "img/cardsLight/showDownload.svg" ]
  Variables →
    HH.img [ HP.src "img/cardsLight/setupVariables.svg" ]
  Troubleshoot →
    HH.img [ HP.src "img/cardsLight/troubleshoot.svg" ]
  Chart →
    HH.img [ HP.src "img/cardsLight/showChart.svg" ]
  Markdown →
    HH.img [ HP.src "img/cardsLight/showMarkdown.svg" ]
  Table →
    HH.img [ HP.src "img/cardsLight/table.svg" ]
  NextAction →
    HH.text ""
  Cache →
    HH.img [ HP.src "img/cardsLight/cache.svg" ]
  Open →
    HH.img [ HP.src "img/cardsLight/open.svg" ]
  DownloadOptions →
    HH.img [ HP.src "img/cardsLight/setupDownload.svg" ]
  Draftboard →
    HH.img [ HP.src "img/cardsLight/draftboard.svg" ]
  ErrorCard →
    HH.text ""
  PendingCard →
    HH.text ""

cardClasses ∷ CardType → Array H.ClassName
cardClasses = case _ of
  Ace at → [ H.className "sd-card-ace" ] <> aceCardClasses at
  Search → [ H.className "sd-card-search" ]
  ChartOptions _ → [ H.className "sd-card-chart-options" ]
  Chart → [ H.className "sd-card-chart" ]
  Markdown → [ H.className "sd-card-markdown" ]
  Table → [ H.className "sd-card-table" ]
  Download → [ H.className "sd-card-download" ]
  DownloadOptions → [ H.className "sd-card-download-options" ]
  Variables → [ H.className "sd-card-variables" ]
  Troubleshoot → [ H.className "sd-card-troubleshoot" ]
  NextAction → [ H.className "sd-card-next-action" ]
  Cache → [ H.className "sd-card-cache" ]
  Open → [ H.className "sd-card-open" ]
  Draftboard → [ H.className "sd-card-draftboard" ]
  ErrorCard → [ H.className "sd-card-error" ]
  PendingCard → [ H.className "sd-card-pending" ]

aceCardName ∷ AceMode → String
aceCardName MarkdownMode = "Setup Markdown"
aceCardName SQLMode = "Query"

aceCardClasses ∷ AceMode → Array H.ClassName
aceCardClasses MarkdownMode = [ H.className "sd-card-markdown" ]
aceCardClasses SQLMode = [ H.className "sd-card-sql" ]

aceMode ∷ AceMode → String
aceMode MarkdownMode = "ace/mode/markdown"
aceMode SQLMode = "ace/mode/sql"

controllable ∷ CardType → Boolean
controllable NextAction = false
controllable _ = true

blocking ∷ CardType → Boolean
blocking ErrorCard = true
blocking _ = false
