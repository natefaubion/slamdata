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

module SlamData.Workspace.Card.Component.State
  ( CardState(..)
  , CardStateP
  , initialCardState
  , _element
  , AnyCardState
  , _AceState
  , _MarkdownState
  , _SearchState
  , _TableState
  , _PivotTableState
  , _ChartOptionsState
  , _ChartState
  , _DownloadState
  , _VariablesState
  , _TroubleshootState
  , _NextState
  , _CacheState
  , _OpenState
  , _DownloadOptionsState
  , _DraftboardState
  , _ErrorState
  , _PendingState
  ) where

import SlamData.Prelude

import Data.Lens (LensP, lens, PrismP, prism')

import DOM.HTML.Types (HTMLElement)

import Halogen (ParentState)

import SlamData.Monad (Slam)
import SlamData.Workspace.Card.Ace.Component.State as Ace
import SlamData.Workspace.Card.Variables.Component.State as Variables
import SlamData.Workspace.Card.Troubleshoot.Component.State as Troubleshoot
import SlamData.Workspace.Card.Cache.Component.State as Cache
import SlamData.Workspace.Card.Chart.Component.State as Chart
import SlamData.Workspace.Card.Component.Query (CardQuery, InnerCardQuery)
import SlamData.Workspace.Card.Download.Component.State as Download
import SlamData.Workspace.Card.DownloadOptions.Component.State as DOpts
import SlamData.Workspace.Card.Draftboard.Component.State as Draftboard
import SlamData.Workspace.Card.Error.Component.State as Error
import SlamData.Workspace.Card.Pending.Component.State as Pending
import SlamData.Workspace.Card.Table.Component.State as Table
import SlamData.Workspace.Card.PivotTable.Component.State as PivotTable
import SlamData.Workspace.Card.Markdown.Component.State as Markdown
import SlamData.Workspace.Card.Next.Component.State as Next
import SlamData.Workspace.Card.Open.Component.State as Open
import SlamData.Workspace.Card.Search.Component.State as Search
import SlamData.Workspace.Card.ChartOptions.Component.State as ChartOptions

-- | The common state value for deck cards.
type CardState =
  { element ∷ Maybe HTMLElement
  }

type CardStateP = ParentState CardState AnyCardState CardQuery InnerCardQuery Slam Unit

-- | Creates an initial `CardState` value for an editor card.
initialCardState ∷ CardState
initialCardState =
  { element: Nothing
  }

_element ∷ LensP CardState (Maybe HTMLElement)
_element = lens _.element _{element = _}

data AnyCardState
  = AceState Ace.StateP
  | MarkdownState Markdown.StateP
  | SearchState Search.State
  | TableState Table.State
  | PivotTableState PivotTable.StateP
  | ChartOptionsState ChartOptions.StateP
  | ChartState Chart.StateP
  | DownloadState Download.State
  | VariablesState Variables.StateP
  | TroubleshootState Troubleshoot.State
  | NextState Next.State
  | CacheState Cache.State
  | OpenState Open.State
  | DownloadOptionsState DOpts.State
  | DraftboardState Draftboard.StateP
  | ErrorState Error.State
  | PendingState Pending.State

_AceState ∷ PrismP AnyCardState Ace.StateP
_AceState = prism' AceState \s → case s of
  AceState s' → Just s'
  _ → Nothing

_MarkdownState ∷ PrismP AnyCardState Markdown.StateP
_MarkdownState = prism' MarkdownState \s → case s of
  MarkdownState s' → Just s'
  _ → Nothing

_SearchState ∷ PrismP AnyCardState Search.State
_SearchState = prism' SearchState \s → case s of
  SearchState s' → Just s'
  _ → Nothing

_TableState ∷ PrismP AnyCardState Table.State
_TableState = prism' TableState \s → case s of
  TableState s' → Just s'
  _ → Nothing

_PivotTableState ∷ PrismP AnyCardState PivotTable.StateP
_PivotTableState = prism' PivotTableState \s → case s of
  PivotTableState s' → Just s'
  _ → Nothing

_ChartOptionsState ∷ PrismP AnyCardState ChartOptions.StateP
_ChartOptionsState = prism' ChartOptionsState \s → case s of
  ChartOptionsState s' → Just s'
  _ → Nothing

_ChartState ∷ PrismP AnyCardState Chart.StateP
_ChartState = prism' ChartState \s → case s of
  ChartState s' → Just s'
  _ → Nothing

_DownloadState ∷ PrismP AnyCardState Download.State
_DownloadState = prism' DownloadState \s → case s of
  DownloadState s' → Just s'
  _ → Nothing

_VariablesState ∷ PrismP AnyCardState Variables.StateP
_VariablesState = prism' VariablesState \s → case s of
  VariablesState s' → Just s'
  _ → Nothing

_TroubleshootState ∷ PrismP AnyCardState Troubleshoot.State
_TroubleshootState = prism' TroubleshootState \s → case s of
  TroubleshootState s' → Just s'
  _ → Nothing

_NextState ∷ PrismP AnyCardState Next.State
_NextState = prism' NextState \s → case s of
  NextState s' → Just s'
  _ → Nothing

_CacheState ∷ PrismP AnyCardState Cache.State
_CacheState = prism' CacheState \s → case s of
  CacheState s' → Just s'
  _ → Nothing

_OpenState ∷ PrismP AnyCardState Open.State
_OpenState = prism' OpenState \s → case s of
  OpenState s' → Just s'
  _ → Nothing

_DownloadOptionsState ∷ PrismP AnyCardState DOpts.State
_DownloadOptionsState = prism' DownloadOptionsState \s → case s of
  DownloadOptionsState s' → Just s'
  _ → Nothing

_DraftboardState ∷ PrismP AnyCardState Draftboard.StateP
_DraftboardState = prism' DraftboardState \s → case s of
  DraftboardState s' → Just s'
  _ → Nothing

_ErrorState ∷ PrismP AnyCardState Error.State
_ErrorState = prism' ErrorState \s → case s of
  ErrorState s' → Just s'
  _ → Nothing

_PendingState ∷ PrismP AnyCardState Pending.State
_PendingState = prism' PendingState \s → case s of
  PendingState s' → Just s'
  _ → Nothing
