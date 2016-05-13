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
  , cardGlyph
  , cardClasses
  , aceCardName
  , aceCardGlyph
  , aceCardClasses
  , aceMode
  , nextCardTypes
  , controllable
  , insertableCardTypes

  , blocking
  ) where

import SlamData.Prelude

import Control.Monad.Error.Class (throwError)

import Data.Argonaut (class EncodeJson, class DecodeJson, encodeJson, decodeJson)
import Data.Array as Arr

import Halogen.HTML as H
import Halogen.HTML.Indexed as HH
import Halogen.Themes.Bootstrap3 as B
import Halogen.HTML.Properties.Indexed as HP

import SlamData.Render.Common (glyph)
import SlamData.Render.CSS as Rc

data CardType
  = Ace AceMode
  | Search
  | Viz
  | Chart
  | Markdown
  | JTable
  | Download
  | API
  | APIResults
  | NextAction
  | Save
  | OpenResource
  | DownloadOptions
  | Draftboard
  | ErrorCard

insertableCardTypes ∷ Array CardType
insertableCardTypes =
  [ Ace SQLMode
  , Ace MarkdownMode
  , Search
  , Viz
  , Chart
  , Markdown
  , JTable
  , Download
  , API
  , APIResults
  , Save
  , OpenResource
  , DownloadOptions
  ]

derive instance eqCardType ∷ Eq CardType
derive instance ordCardType ∷ Ord CardType

data AceMode
  = MarkdownMode
  | SQLMode

derive instance eqAceMode ∷ Eq AceMode
derive instance ordAceMode ∷ Ord AceMode

instance encodeJsonCardType ∷ EncodeJson CardType where
  encodeJson (Ace MarkdownMode) = encodeJson "ace-markdown"
  encodeJson (Ace SQLMode) = encodeJson "ace-sql"
  encodeJson Search = encodeJson "search"
  encodeJson Viz = encodeJson "viz"
  encodeJson Chart = encodeJson "chart"
  encodeJson Markdown = encodeJson "markdown"
  encodeJson JTable = encodeJson "jtable"
  encodeJson Download = encodeJson "download"
  encodeJson API = encodeJson "api"
  encodeJson APIResults = encodeJson "api-results"
  encodeJson NextAction = encodeJson "next-action"
  encodeJson Save = encodeJson "save"
  encodeJson OpenResource = encodeJson "open-resource"
  encodeJson DownloadOptions = encodeJson "download-options"
  encodeJson Draftboard = encodeJson "draftboard"
  encodeJson ErrorCard = encodeJson "error"

instance decodeJsonCardType ∷ DecodeJson CardType where
  decodeJson json = do
    str ← decodeJson json
    case str of
      "ace-markdown" → pure $ Ace MarkdownMode
      "ace-sql" → pure $ Ace SQLMode
      "search" → pure Search
      "viz" → pure Viz
      "chart" → pure Chart
      "markdown" → pure Markdown
      "jtable" → pure JTable
      "download" → pure Download
      "api" → pure API
      "api-results" → pure APIResults
      "next-action" → pure NextAction
      "save" → pure Save
      "open-resource" → pure OpenResource
      "download-options" → pure DownloadOptions
      "draftboard" → pure Draftboard
      "error" → pure ErrorCard
      name → throwError $ "unknown card type '" ⊕ name ⊕ "'"

cardName ∷ CardType → String
cardName (Ace at) = aceCardName at
cardName Search = "Search"
cardName Viz = "Visualize"
cardName Chart = "Chart"
cardName Markdown = "Form"
cardName JTable = "Table"
cardName Download = "Link"
cardName API = "API"
cardName APIResults = "API Results"
cardName NextAction = "Next Action"
cardName Save = "Save"
cardName OpenResource = "Explore"
cardName DownloadOptions = "Download"
cardName Draftboard = "Draftboard"
cardName ErrorCard = "Error"

cardGlyph ∷ ∀ s f. CardType → H.HTML s f
cardGlyph (Ace at) = glyph $ aceCardGlyph at
cardGlyph Search = glyph B.glyphiconSearch
cardGlyph Viz = glyph B.glyphiconPicture
cardGlyph Download = glyph B.glyphiconDownloadAlt
cardGlyph API = glyph B.glyphiconOpenFile
cardGlyph APIResults = glyph B.glyphiconTasks
cardGlyph Chart = HH.div [ HP.classes [ Rc.glyphImage, Rc.chartGlyph ] ] [ ]
cardGlyph Markdown = HH.div [ HP.classes [ Rc.glyphImage, Rc.codeGlyph ] ] [ ]
cardGlyph JTable = glyph B.glyphiconThList
cardGlyph NextAction = glyph B.glyphiconStop
cardGlyph Save = glyph B.glyphiconFloppyDisk
cardGlyph OpenResource = glyph B.glyphiconFolderOpen
cardGlyph DownloadOptions = glyph B.glyphiconDownload
cardGlyph Draftboard = glyph B.glyphiconTh
cardGlyph ErrorCard = glyph B.glyphiconAlert

cardClasses ∷ CardType → Array H.ClassName
cardClasses (Ace at) = [ H.className "sd-card-ace" ] <> aceCardClasses at
cardClasses Search = [ H.className "sd-card-search" ]
cardClasses Viz = [ H.className "sd-card-viz" ]
cardClasses Chart = [ H.className "sd-card-chart" ]
cardClasses Markdown = [ H.className "sd-card-markdown" ]
cardClasses JTable = [ H.className "sd-card-table" ]
cardClasses Download = [ H.className "sd-card-download" ]
cardClasses DownloadOptions = [ H.className "sd-card-download-options" ]
cardClasses API = [ H.className "sd-card-api" ]
cardClasses APIResults = [ H.className "sd-card-api-results" ]
cardClasses NextAction = [ H.className "sd-card-next-action" ]
cardClasses Save = [ H.className "sd-card-save" ]
cardClasses OpenResource = [ H.className "sd-card-open-resource" ]
cardClasses Draftboard = [ H.className "sd-card-draftboard" ]
cardClasses ErrorCard = [ H.className "sd-card-error" ]

aceCardName ∷ AceMode → String
aceCardName MarkdownMode = "Markdown"
aceCardName SQLMode = "Query"

aceCardGlyph ∷ AceMode → HH.ClassName
aceCardGlyph MarkdownMode = B.glyphiconEdit
aceCardGlyph SQLMode = B.glyphiconQuestionSign

aceCardClasses ∷ AceMode → Array H.ClassName
aceCardClasses MarkdownMode = [ H.className "sd-card-markdown" ]
aceCardClasses SQLMode = [ H.className "sd-card-sql" ]

aceMode ∷ AceMode → String
aceMode MarkdownMode = "ace/mode/markdown"
aceMode SQLMode = "ace/mode/sql"

nextCardTypes ∷ Maybe CardType → Array CardType
nextCardTypes Nothing =
  [ Ace SQLMode
  , Ace MarkdownMode
  , OpenResource
  , API
  ]
nextCardTypes (Just ct) = case ct of
  Search → dataSourceCards
  Ace SQLMode → dataSourceCards
  Viz → [ Chart ]
  API → [ APIResults ]
  Ace MarkdownMode → [ Markdown ]
  Markdown → [ Ace SQLMode ]
  JTable → dataSourceOutput `Arr.snoc` Save
  Download → [ ]
  APIResults →  [ Ace SQLMode ]
  Chart → [ ]
  NextAction → [ ]
  Save → dataSourceOutput `Arr.snoc` JTable
  OpenResource → dataSourceCards
  DownloadOptions → [ Download ]
  Draftboard → [ ]
  ErrorCard → [ ]
  where
  dataSourceOutput =
    [ DownloadOptions, Search, Ace SQLMode, Viz
    ]
  dataSourceCards =
    (dataSourceOutput `Arr.snoc` JTable) `Arr.snoc` Save

controllable ∷ CardType → Boolean
controllable NextAction = false
controllable _ = true

blocking ∷ CardType → Boolean
blocking ErrorCard = true
blocking _ = false
