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
  , _TableQuery
  , _ChartOptionsQuery
  , _ChartQuery
  , _DownloadQuery
  , _VariablesQuery
  , _TroubleshootQuery
  , _NextQuery
  , _CacheQuery
  , _OpenQuery
  , _DownloadOptionsQuery
  , _DraftboardQuery
  , _ErrorQuery
  , _PendingQuery
  , module SlamData.Workspace.Card.Common.EvalQuery
  ) where

import SlamData.Prelude

import Data.Lens (PrismP, prism')
import Data.Lens.Prism.Coproduct (_Left, _Right)

import DOM.HTML.Types (HTMLElement)

import Halogen (ChildF)

import SlamData.Workspace.Card.Ace.Component.Query as Ace
import SlamData.Workspace.Card.Variables.Component.Query as Variables
import SlamData.Workspace.Card.Troubleshoot.Component.Query as Troubleshoot
import SlamData.Workspace.Card.Cache.Component.Query as Cache
import SlamData.Workspace.Card.CardId (CardId)
import SlamData.Workspace.Card.CardType (CardType)
import SlamData.Workspace.Card.Chart.Component.Query as Chart
import SlamData.Workspace.Card.Common.EvalQuery (CardEvalInput, CardEvalT, CardEvalQuery(..), ModelUpdateType(..), raiseUpdatedC, raiseUpdatedC', raiseUpdatedP, raiseUpdatedP')
import SlamData.Workspace.Card.Download.Component.Query as Download
import SlamData.Workspace.Card.DownloadOptions.Component.Query as DOpts
import SlamData.Workspace.Card.Draftboard.Component.Query as Draftboard
import SlamData.Workspace.Card.Error.Component.Query as Error
import SlamData.Workspace.Card.Table.Component.Query as Table
import SlamData.Workspace.Card.Markdown.Component.Query as Markdown
import SlamData.Workspace.Card.Model as Card
import SlamData.Workspace.Card.Next.Component.Query as Next
import SlamData.Workspace.Card.Open.Component.Query as Open
import SlamData.Workspace.Card.Pending.Component.Query as Pending
import SlamData.Workspace.Card.Port (Port)
import SlamData.Workspace.Card.Search.Component.Query as Search
import SlamData.Workspace.Card.ChartOptions.Component.Query as ChartOptions

-- | The common query algebra for a card.
-- |
-- | - `UpdateCard` accepts an input value from a parent card if one is
-- |   required, performs any necessary actions to evalute the card and update
-- |   its state, and then returns its own output value.
-- | - `RefreshCard` is captured by the deck and goes to the root of the
-- |   current card's dependencies and updates the cards downwards from there.
data CardQuery a
  = UpdateCard CardEvalInput (Maybe Port) a
  | SaveCard CardId CardType (Card.Model → a)
  | ActivateCard a
  | LoadCard Card.Model a
  | UpdateDimensions a
  | SetHTMLElement (Maybe HTMLElement) a

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
  | TableQuery (Table.QueryP a)
  | ChartOptionsQuery (ChartOptions.QueryP a)
  | ChartQuery (Chart.QueryP a)
  | DownloadQuery (Download.QueryP a)
  | VariablesQuery (Variables.QueryP a)
  | TroubleshootQuery (Troubleshoot.QueryP a)
  | NextQuery (Next.QueryP a)
  | CacheQuery (Cache.QueryP a)
  | OpenQuery (Open.QueryP a)
  | DownloadOptionsQuery (DOpts.QueryP a)
  | DraftboardQuery (Draftboard.QueryP a)
  | ErrorQuery (Error.QueryP a)
  | PendingQuery (Pending.QueryP a)

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

_TableQuery ∷ ∀ a. PrismP (AnyCardQuery a) (Table.QueryP a)
_TableQuery = prism' TableQuery \q → case q of
  TableQuery q' → Just q'
  _ → Nothing

_ChartOptionsQuery ∷ ∀ a. PrismP (AnyCardQuery a) (ChartOptions.QueryP a)
_ChartOptionsQuery = prism' ChartOptionsQuery \q → case q of
  ChartOptionsQuery q' → Just q'
  _ → Nothing

_ChartQuery ∷ ∀ a. PrismP (AnyCardQuery a) (Chart.QueryP a)
_ChartQuery = prism' ChartQuery \q → case q of
  ChartQuery q' → Just q'
  _ → Nothing

_DownloadQuery ∷ ∀ a. PrismP (AnyCardQuery a) (Download.QueryP a)
_DownloadQuery = prism' DownloadQuery \q → case q of
  DownloadQuery q' → Just q'
  _ → Nothing

_VariablesQuery ∷ ∀ a. PrismP (AnyCardQuery a) (Variables.QueryP a)
_VariablesQuery = prism' VariablesQuery \q → case q of
  VariablesQuery q' → Just q'
  _ → Nothing

_TroubleshootQuery ∷ ∀ a. PrismP (AnyCardQuery a) (Troubleshoot.QueryP a)
_TroubleshootQuery = prism' TroubleshootQuery \q → case q of
  TroubleshootQuery q' → Just q'
  _ → Nothing

_NextQuery ∷ ∀ a. PrismP (AnyCardQuery a) (Next.QueryP a)
_NextQuery = prism' NextQuery \q → case q of
  NextQuery q' → Just q'
  _ → Nothing

_CacheQuery ∷ ∀ a. PrismP (AnyCardQuery a) (Cache.QueryP a)
_CacheQuery = prism' CacheQuery \q → case q of
  CacheQuery q' → Just q'
  _ → Nothing

_OpenQuery ∷ ∀ a. PrismP (AnyCardQuery a) (Open.QueryP a)
_OpenQuery = prism' OpenQuery \q → case q of
  OpenQuery q' → Just q'
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

_PendingQuery ∷ ∀ a. PrismP (AnyCardQuery a) (Pending.QueryP a)
_PendingQuery = prism' PendingQuery \q → case q of
  PendingQuery q' → Just q'
  _ → Nothing
