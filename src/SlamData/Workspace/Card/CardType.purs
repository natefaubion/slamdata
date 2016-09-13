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
  , insertableCardTypes

  , blocking
  ) where

import SlamData.Prelude

import Data.Argonaut (class EncodeJson, class DecodeJson, encodeJson, decodeJson)

import Halogen.HTML as H
import Halogen.HTML.Indexed as HH
import Halogen.HTML.Properties.Indexed as HP

import Test.StrongCheck.Arbitrary as SC

data CardType
  = Ace AceMode
  | Search
  | ChartOptions
  | Chart
  | Markdown
  | Table
  | PivotTable
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

insertableCardTypes ∷ Array CardType
insertableCardTypes =
  [ Ace SQLMode
  , Open
  , Search
  , Table
  , PivotTable
  , Variables
  , Ace MarkdownMode
  , Markdown
  , Download
  , DownloadOptions
  , ChartOptions
  , Chart
  , Draftboard
  , Troubleshoot
  , Cache
  ]

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
  encodeJson =
    encodeJson ∘
      case _ of
        Ace MarkdownMode → "ace-markdown"
        Ace SQLMode → "ace-sql"
        Search → "search"
        ChartOptions → "chart-options"
        Chart → "chart"
        Markdown → "markdown"
        Table → "table"
        PivotTable → "pivot-table"
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
      "chart-options" → pure ChartOptions
      "chart" → pure Chart
      "markdown" → pure Markdown
      "table" → pure Table
      "pivot-table" → pure PivotTable
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
      name → throwError $ "unknown card type '" ⊕ name ⊕ "'"

cardName ∷ CardType → String
cardName =
  case _ of
    Ace at → aceCardName at
    Search → "Search"
    ChartOptions → "Setup Chart"
    Chart → "Show Chart"
    Markdown → "Show Markdown"
    Table → "Show Table"
    PivotTable → "Pivot Table"
    Download → "Show Download"
    Variables → "Setup Variables"
    Troubleshoot → "Troubleshoot"
    NextAction → "Next Action"
    Cache → "Cache"
    Open → "Open"
    DownloadOptions → "Setup Download"
    Draftboard → "Setup Draftboard"
    ErrorCard → "Error"
    PendingCard → "Pending"

darkCardGlyph ∷ ∀ s f. CardType → H.HTML s f
darkCardGlyph =
  case _ of
    Ace MarkdownMode → HH.img [ HP.src "img/cardsDark/setupMarkdown.svg" ]
    Ace SQLMode → HH.img [ HP.src "img/cardsDark/query.svg" ]
    Search →  HH.img [ HP.src "img/cardsDark/search.svg" ]
    ChartOptions → HH.img [ HP.src "img/cardsDark/setupChart.svg" ]
    Download → HH.img [ HP.src "img/cardsDark/showDownload.svg" ]
    Variables → HH.img [ HP.src "img/cardsDark/setupVariables.svg" ]
    Troubleshoot → HH.img [ HP.src "img/cardsDark/troubleshoot.svg" ]
    Chart → HH.img [ HP.src "img/cardsDark/showChart.svg" ]
    Markdown → HH.img [ HP.src "img/cardsDark/showMarkdown.svg" ]
    Table → HH.img [ HP.src "img/cardsDark/table.svg" ]
    PivotTable → HH.img [ HP.src "img/cardsDark/table.svg" ]
    NextAction → HH.text ""
    Cache → HH.img [ HP.src "img/cardsDark/cache.svg" ]
    Open → HH.img [ HP.src "img/cardsDark/open.svg" ]
    DownloadOptions → HH.img [ HP.src "img/cardsDark/setupDownload.svg" ]
    Draftboard → HH.img [ HP.src "img/cardsDark/draftboard.svg" ]
    ErrorCard → HH.text ""
    PendingCard → HH.text ""

lightCardGlyph ∷ ∀ s f. CardType → H.HTML s f
lightCardGlyph =
  case _ of
    Ace MarkdownMode → HH.img [ HP.src "img/cardsLight/setupMarkdown.svg" ]
    Ace SQLMode → HH.img [ HP.src "img/cardsLight/query.svg" ]
    Search →  HH.img [ HP.src "img/cardsLight/search.svg" ]
    ChartOptions → HH.img [ HP.src "img/cardsLight/setupChart.svg" ]
    Download → HH.img [ HP.src "img/cardsLight/showDownload.svg" ]
    Variables → HH.img [ HP.src "img/cardsLight/setupVariables.svg" ]
    Troubleshoot → HH.img [ HP.src "img/cardsLight/troubleshoot.svg" ]
    Chart → HH.img [ HP.src "img/cardsLight/showChart.svg" ]
    Markdown → HH.img [ HP.src "img/cardsLight/showMarkdown.svg" ]
    Table → HH.img [ HP.src "img/cardsLight/table.svg" ]
    PivotTable → HH.img [ HP.src "img/cardsLight/table.svg" ]
    NextAction → HH.text ""
    Cache → HH.img [ HP.src "img/cardsLight/cache.svg" ]
    Open → HH.img [ HP.src "img/cardsLight/open.svg" ]
    DownloadOptions → HH.img [ HP.src "img/cardsLight/setupDownload.svg" ]
    Draftboard → HH.img [ HP.src "img/cardsLight/draftboard.svg" ]
    ErrorCard → HH.text ""
    PendingCard → HH.text ""

cardClasses ∷ CardType → Array H.ClassName
cardClasses =
  case _ of
    Ace at → [ H.className "sd-card-ace" ] <> aceCardClasses at
    Search → [ H.className "sd-card-search" ]
    ChartOptions → [ H.className "sd-card-chart-options" ]
    Chart → [ H.className "sd-card-chart" ]
    Markdown → [ H.className "sd-card-markdown" ]
    Table → [ H.className "sd-card-table" ]
    PivotTable → [ H.className "sd-card-pivot-table" ]
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
