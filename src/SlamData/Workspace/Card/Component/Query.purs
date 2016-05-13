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

module SlamData.Workspace.Card.Component.Query
  ( CardQuery(..)
  , CardQueryP
  , InnerCardQuery
  , _CardEvalQuery
  , _AnyCardQuery
  , AnyCardQuery(..)
  , _AceQuery
  , _MarkdownQuery
  , _SearchQuery
  , _JTableQuery
  , _VizQuery
  , _ChartQuery
  , _DownloadQuery
  , _APIQuery
  , _APIResultsQuery
  , _NextQuery
  , _SaveQuery
  , _OpenResourceQuery
  , _DownloadOptionsQuery
  , _DraftboardQuery
  , _ErrorQuery
  , module SlamData.Workspace.Card.Common.EvalQuery
  ) where

import SlamData.Prelude

import Data.Lens (PrismP, prism')
import Data.Lens.Prism.Coproduct (_Left, _Right)
import Data.Time (Milliseconds)

import DOM.HTML.Types (HTMLElement)

import Halogen (ChildF)

import SlamData.Workspace.AccessType as Na

import SlamData.Workspace.Card.Ace.Component.Query as Ace
import SlamData.Workspace.Card.API.Component.Query as API
import SlamData.Workspace.Card.APIResults.Component.Query as APIResults
import SlamData.Workspace.Card.CardId (CardId)
import SlamData.Workspace.Card.CardType (CardType)
import SlamData.Workspace.Card.Chart.Component.Query as Chart
import SlamData.Workspace.Card.Common.EvalQuery (CardEvalQuery(..), CardEvalInput)
import SlamData.Workspace.Card.Download.Component.Query as Download
import SlamData.Workspace.Card.DownloadOptions.Component.Query as DOpts
import SlamData.Workspace.Card.Draftboard.Component.Query as Draftboard
import SlamData.Workspace.Card.Error.Component.Query as Error
import SlamData.Workspace.Card.JTable.Component.Query as JTable
import SlamData.Workspace.Card.Markdown.Component.Query as Markdown
import SlamData.Workspace.Card.Model as Card
import SlamData.Workspace.Card.Next.Component.Query as Next
import SlamData.Workspace.Card.OpenResource.Component.Query as Open
import SlamData.Workspace.Card.Port (Port)
import SlamData.Workspace.Card.Save.Component.Query as Save
import SlamData.Workspace.Card.Search.Component.Query as Search
import SlamData.Workspace.Card.Viz.Component.Query as Viz

-- | The common query algebra for a card.
-- |
-- | - `RunCard` is captured by the deck and used to call `UpdateCard` on
-- |   the card that raised it, passing in an input value if one is required.
-- | - `UpdateCard` accepts an input value from a parent card if one is
-- |   required, performs any necessary actions to evalute the card and update
-- |   its state, and then returns its own output value.
-- | - `RefreshCard` is captured by the deck and goes to the root of the
-- |   current card's dependencies and updates the cards downwards from there.
-- | - `ToggleMessages` is used to toggle the visibility of the status/error
-- |   messages generated while evaluating the card.
data CardQuery a
  = RunCard a
  | StopCard a
  | UpdateCard CardEvalInput (Maybe Port → a)
  | RefreshCard a
  | ToggleMessages a
  | Tick Milliseconds a
  | GetOutput (Maybe Port → a)
  | SaveCard CardId CardType (Card.Model → a)
  | LoadCard Card.Model a
  | SetCardAccessType Na.AccessType a
  | SetHTMLElement (Maybe HTMLElement) a
  | UpdateDimensions a

type CardQueryP = Coproduct CardQuery (ChildF Unit InnerCardQuery)

type InnerCardQuery = Coproduct CardEvalQuery AnyCardQuery

_CardEvalQuery ∷ ∀ a. PrismP (InnerCardQuery a) (CardEvalQuery a)
_CardEvalQuery = _Left

_AnyCardQuery ∷ ∀ a. PrismP (InnerCardQuery a) (AnyCardQuery a)
_AnyCardQuery = _Right

data AnyCardQuery a
  = AceQuery (Ace.QueryP a)
  | MarkdownQuery (Markdown.QueryP a)
  | SearchQuery (Search.Query a)
  | JTableQuery (JTable.QueryP a)
  | VizQuery (Viz.QueryP a)
  | ChartQuery (Chart.QueryP a)
  | DownloadQuery (Download.QueryP a)
  | APIQuery (API.QueryP a)
  | APIResultsQuery (APIResults.QueryP a)
  | NextQuery (Next.QueryP a)
  | SaveQuery (Save.QueryP a)
  | OpenResourceQuery (Open.QueryP a)
  | DownloadOptionsQuery (DOpts.QueryP a)
  | DraftboardQuery (Draftboard.QueryP a)
  | ErrorQuery (Error.QueryP a)

_AceQuery ∷ ∀ a. PrismP (AnyCardQuery a) (Ace.QueryP a)
_AceQuery = prism' AceQuery \q → case q of
  AceQuery q' → Just q'
  _ → Nothing

_MarkdownQuery ∷ ∀ a. PrismP (AnyCardQuery a) (Markdown.QueryP a)
_MarkdownQuery = prism' MarkdownQuery \q → case q of
  MarkdownQuery q' → Just q'
  _ → Nothing

_SearchQuery ∷ ∀ a. PrismP (AnyCardQuery a) (Search.Query a)
_SearchQuery = prism' SearchQuery \q → case q of
  SearchQuery q' → Just q'
  _ → Nothing

_JTableQuery ∷ ∀ a. PrismP (AnyCardQuery a) (JTable.QueryP a)
_JTableQuery = prism' JTableQuery \q → case q of
  JTableQuery q' → Just q'
  _ → Nothing

_VizQuery ∷ ∀ a. PrismP (AnyCardQuery a) (Viz.QueryP a)
_VizQuery = prism' VizQuery \q → case q of
  VizQuery q' → Just q'
  _ → Nothing

_ChartQuery ∷ ∀ a. PrismP (AnyCardQuery a) (Chart.QueryP a)
_ChartQuery = prism' ChartQuery \q → case q of
  ChartQuery q' → Just q'
  _ → Nothing

_DownloadQuery ∷ ∀ a. PrismP (AnyCardQuery a) (Download.QueryP a)
_DownloadQuery = prism' DownloadQuery \q → case q of
  DownloadQuery q' → Just q'
  _ → Nothing

_APIQuery ∷ ∀ a. PrismP (AnyCardQuery a) (API.QueryP a)
_APIQuery = prism' APIQuery \q → case q of
  APIQuery q' → Just q'
  _ → Nothing

_APIResultsQuery ∷ ∀ a. PrismP (AnyCardQuery a) (APIResults.QueryP a)
_APIResultsQuery = prism' APIResultsQuery \q → case q of
  APIResultsQuery q' → Just q'
  _ → Nothing

_NextQuery ∷ ∀ a. PrismP (AnyCardQuery a) (Next.QueryP a)
_NextQuery = prism' NextQuery \q → case q of
  NextQuery q' → Just q'
  _ → Nothing

_SaveQuery ∷ ∀ a. PrismP (AnyCardQuery a) (Save.QueryP a)
_SaveQuery = prism' SaveQuery \q → case q of
  SaveQuery q' → Just q'
  _ → Nothing

_OpenResourceQuery ∷ ∀ a. PrismP (AnyCardQuery a) (Open.QueryP a)
_OpenResourceQuery = prism' OpenResourceQuery \q → case q of
  OpenResourceQuery q' → Just q'
  _ → Nothing

_DownloadOptionsQuery ∷ ∀ a. PrismP (AnyCardQuery a) (DOpts.QueryP a)
_DownloadOptionsQuery = prism' DownloadOptionsQuery \q → case q of
  DownloadOptionsQuery q' → Just q'
  _ → Nothing

_DraftboardQuery ∷ ∀ a. PrismP (AnyCardQuery a) (Draftboard.QueryP a)
_DraftboardQuery = prism' DraftboardQuery \q → case q of
  DraftboardQuery q' → Just q'
  _ → Nothing

_ErrorQuery ∷ ∀ a. PrismP (AnyCardQuery a) (Error.QueryP a)
_ErrorQuery = prism' ErrorQuery \q → case q of
  ErrorQuery q' → Just q'
  _ → Nothing
