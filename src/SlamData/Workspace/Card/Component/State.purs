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
  , _accessType
  , _visibility
  , _runState
  , _tickStopper
  , _messages
  , _messageVisibility
  , _hasResults
  , _output
  , _canceler
  , _element
  , AnyCardState
  , _AceState
  , _MarkdownState
  , _SearchState
  , _JTableState
  , _VizState
  , _ChartState
  , _DownloadState
  , _APIState
  , _APIResultsState
  , _NextState
  , _SaveState
  , _OpenResourceState
  , _DownloadOptionsState
  , _DraftboardState
  , _ErrorState
  ) where

import SlamData.Prelude

import Control.Monad.Aff (Canceler)

import Data.Lens (LensP, lens, PrismP, prism')
import Data.Visibility (Visibility(..))

import DOM.HTML.Types (HTMLElement)

import Halogen (ParentState)

import SlamData.Effects (Slam, SlamDataEffects)
import SlamData.Workspace.AccessType (AccessType(..))
import SlamData.Workspace.Card.Ace.Component.State as Ace
import SlamData.Workspace.Card.API.Component.State as API
import SlamData.Workspace.Card.APIResults.Component.State as APIResults
import SlamData.Workspace.Card.Chart.Component.State as Chart
import SlamData.Workspace.Card.Component.Query (CardQuery, InnerCardQuery)
import SlamData.Workspace.Card.Download.Component.State as Download
import SlamData.Workspace.Card.DownloadOptions.Component.State as DOpts
import SlamData.Workspace.Card.Draftboard.Component.State as Draftboard
import SlamData.Workspace.Card.Error.Component.State as Error
import SlamData.Workspace.Card.JTable.Component.State as JTable
import SlamData.Workspace.Card.Markdown.Component.State as Markdown
import SlamData.Workspace.Card.Next.Component.State as Next
import SlamData.Workspace.Card.OpenResource.Component.State as Open
import SlamData.Workspace.Card.Port (Port)
import SlamData.Workspace.Card.RunState (RunState(..))
import SlamData.Workspace.Card.Save.Component.State as Save
import SlamData.Workspace.Card.Search.Component.State as Search
import SlamData.Workspace.Card.Viz.Component.State as Viz

-- | The common state value for deck cards.
-- |
-- | - `accessType` tracks whether the card is in an editable or read-only
-- |   deck. In the case of read-only decks editor cards are hidden.
-- | - `visibility` is used to specify whether the card should be rendered at
-- |   all - used when embedding a single card in another page.
-- | - `runState` tracks whether the card has run yet, is running, or has
-- |   completed running.
-- | - `messages` is the informational messages generated
-- |   during evaluation.
-- | - `messageVisibility` determines whether the messages should be shown or
-- |   not.
-- | - `hasResults` tracks whether the card has been evaluated successfully and
-- |   produced a result.
type CardState =
  { accessType ∷ AccessType
  , visibility ∷ Visibility
  , runState ∷ RunState
  , tickStopper ∷ Slam Unit
  , messages ∷ Array String
  , messageVisibility ∷ Visibility
  , hasResults ∷ Boolean
  , output ∷ Maybe Port
  , canceler ∷ Canceler SlamDataEffects
  , element ∷ Maybe HTMLElement
  }

type CardStateP = ParentState CardState AnyCardState CardQuery InnerCardQuery Slam Unit

-- | Creates an initial `CardState` value for an editor card.
initialCardState ∷ CardState
initialCardState =
  { accessType: Editable
  , visibility: Visible
  , runState: RunInitial
  , tickStopper: pure unit
  , messages: []
  , messageVisibility: Invisible
  , hasResults: false
  , output: Nothing
  , canceler: mempty
  , element: Nothing
  }

_accessType ∷ LensP CardState AccessType
_accessType = lens _.accessType (_ { accessType = _ })

_visibility ∷ LensP CardState Visibility
_visibility = lens _.visibility (_ { visibility = _ })

_runState ∷ LensP CardState RunState
_runState = lens _.runState (_ { runState = _ })

_tickStopper ∷ LensP CardState (Slam Unit)
_tickStopper = lens _.tickStopper (_ { tickStopper = _ })

_messages ∷ LensP CardState (Array String)
_messages = lens _.messages (_ { messages = _ })

_messageVisibility ∷ LensP CardState Visibility
_messageVisibility = lens _.messageVisibility (_ { messageVisibility = _ })

_hasResults ∷ LensP CardState Boolean
_hasResults = lens _.hasResults (_ { hasResults = _ })

-- | The last output value computed for the card. This may not be up to date
-- | with the exact state of the card, but is the most recent result from when
-- | the card was evaluated.
_output ∷ LensP CardState (Maybe Port)
_output = lens _.output (_ { output = _ })

_canceler ∷ LensP CardState (Canceler SlamDataEffects)
_canceler = lens _.canceler _{canceler = _}

_element ∷ LensP CardState (Maybe HTMLElement)
_element = lens _.element _{element = _}

data AnyCardState
  = AceState Ace.StateP
  | MarkdownState Markdown.StateP
  | SearchState Search.State
  | JTableState JTable.State
  | VizState Viz.StateP
  | ChartState Chart.StateP
  | DownloadState Download.State
  | APIState API.StateP
  | APIResultsState APIResults.State
  | NextState Next.State
  | SaveState Save.State
  | OpenResourceState Open.State
  | DownloadOptionsState DOpts.State
  | DraftboardState Draftboard.State
  | ErrorState Error.State

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

_JTableState ∷ PrismP AnyCardState JTable.State
_JTableState = prism' JTableState \s → case s of
  JTableState s' → Just s'
  _ → Nothing

_VizState ∷ PrismP AnyCardState Viz.StateP
_VizState = prism' VizState \s → case s of
  VizState s' → Just s'
  _ → Nothing

_ChartState ∷ PrismP AnyCardState Chart.StateP
_ChartState = prism' ChartState \s → case s of
  ChartState s' → Just s'
  _ → Nothing

_DownloadState ∷ PrismP AnyCardState Download.State
_DownloadState = prism' DownloadState \s → case s of
  DownloadState s' → Just s'
  _ → Nothing

_APIState ∷ PrismP AnyCardState API.StateP
_APIState = prism' APIState \s → case s of
  APIState s' → Just s'
  _ → Nothing

_APIResultsState ∷ PrismP AnyCardState APIResults.State
_APIResultsState = prism' APIResultsState \s → case s of
  APIResultsState s' → Just s'
  _ → Nothing

_NextState ∷ PrismP AnyCardState Next.State
_NextState = prism' NextState \s → case s of
  NextState s' → Just s'
  _ → Nothing

_SaveState ∷ PrismP AnyCardState Save.State
_SaveState = prism' SaveState \s → case s of
  SaveState s' → Just s'
  _ → Nothing

_OpenResourceState ∷ PrismP AnyCardState Open.State
_OpenResourceState = prism' OpenResourceState \s → case s of
  OpenResourceState s' → Just s'
  _ → Nothing

_DownloadOptionsState ∷ PrismP AnyCardState DOpts.State
_DownloadOptionsState = prism' DownloadOptionsState \s → case s of
  DownloadOptionsState s' → Just s'
  _ → Nothing

_DraftboardState ∷ PrismP AnyCardState Draftboard.State
_DraftboardState = prism' DraftboardState \s → case s of
  DraftboardState s' → Just s'
  _ → Nothing

_ErrorState ∷ PrismP AnyCardState Error.State
_ErrorState = prism' ErrorState \s → case s of
  ErrorState s' → Just s'
  _ → Nothing
