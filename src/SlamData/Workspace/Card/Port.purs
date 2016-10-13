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

module SlamData.Workspace.Card.Port
  ( Port(..)
  , TaggedResourcePort
  , DownloadPort
  , MetricPort
  , PivotTablePort
  , tagPort
  , eqTaggedResourcePort
  , _SlamDown
  , _VarMap
  , _Resource
  , _ResourceTag
  , _DownloadOptions
  , _Draftboard
  , _CardError
  , _Blocked
  , _Metric
  , _ChartInstructions
  , _ChartAxes
  , _PivotTable
  , module SlamData.Workspace.Card.Port.VarMap
  ) where

import SlamData.Prelude

import Data.Argonaut (Json)
import Data.Lens (PrismP, prism', TraversalP, wander)

import ECharts.Monad (DSL)
import ECharts.Types.Phantom (OptionI)

import SlamData.Workspace.Card.Port.VarMap (VarMap, URLVarMap, VarMapValue(..), renderVarMapValue, emptyVarMap)
import SlamData.Workspace.Card.BuildChart.Axis (Axes)
import SlamData.Workspace.Card.BuildChart.PivotTable.Model as PTM
import SlamData.Workspace.Card.CardType.ChartType (ChartType)
import SlamData.Download.Model (DownloadOptions)
import Text.Markdown.SlamDown as SD
import Utils.Path as PU

type DownloadPort =
  { resource ∷ PU.FilePath
  , compress ∷ Boolean
  , options ∷ DownloadOptions
  }

type TaggedResourcePort =
  { resource ∷ PU.FilePath
  , tag ∷ Maybe String
  , varMap ∷ Maybe VarMap
  }

type MetricPort =
  { label ∷ Maybe String
  , value ∷ String
  , axes ∷ Axes
  }

type PivotTablePort =
  { records ∷ Array Json
  , options ∷ PTM.Model
  , taggedResource ∷ TaggedResourcePort
  , axes ∷ Axes
  , query ∷ String
  }

data Port
  = SlamDown (VarMap × (SD.SlamDownP VarMapValue))
  | VarMap VarMap
  | CardError String
  | ChartInstructions Axes (DSL OptionI) ChartType
  | TaggedResource TaggedResourcePort
  | DownloadOptions DownloadPort
  | Metric MetricPort
  | PivotTable PivotTablePort
  | Draftboard
  | Blocked

tagPort ∷ Maybe Port → String
tagPort Nothing = "Nothing"
tagPort (Just p) = case p of
  SlamDown sd → "SlamDown: " ⊕ show sd
  VarMap vm → "VarMap: " ⊕ show vm
  CardError str → "CardError: " ⊕ show str
  TaggedResource p → "TaggedResource: " ⊕ show p.resource ⊕ " " ⊕ show p.tag
  DownloadOptions p → "DownloadOptions"
  Draftboard → "Draftboard"
  Blocked → "Blocked"
  ChartInstructions _ _ _ → "ChartInstructions"
  Metric _ → "Metric"
  PivotTable _ → "PivotTable"

eqTaggedResourcePort ∷ TaggedResourcePort → TaggedResourcePort → Boolean
eqTaggedResourcePort tr1 tr2 =
  tr1.resource ≡ tr2.resource
  && tr1.tag ≡ tr2.tag
  && tr1.varMap ≡ tr2.varMap

_SlamDown ∷ TraversalP Port (SD.SlamDownP VarMapValue)
_SlamDown = wander \f s → case s of
  SlamDown (vm × sd) → SlamDown ∘ (vm × _) <$> f sd
  _ → pure s

_VarMap ∷ TraversalP Port VarMap
_VarMap = wander \f s → case s of
  VarMap x → VarMap <$> f x
  SlamDown (vm × sd) → SlamDown ∘ (_ × sd) <$> f vm
  _ → pure s

_CardError ∷ PrismP Port String
_CardError = prism' CardError \p → case p of
  CardError x → Just x
  _ → Nothing

_ResourceTag ∷ TraversalP Port String
_ResourceTag = wander \f s → case s of
  TaggedResource o@{tag: Just tag} →
    TaggedResource ∘ o{tag = _} ∘ Just <$> f tag
  _ → pure s

_Resource ∷ TraversalP Port PU.FilePath
_Resource = wander \f s → case s of
  TaggedResource o → TaggedResource ∘ o{resource = _} <$> f o.resource
  _ → pure s

_Blocked ∷ PrismP Port Unit
_Blocked = prism' (const Blocked) \p → case p of
  Blocked → Just unit
  _ → Nothing

_DownloadOptions ∷ PrismP Port DownloadPort
_DownloadOptions = prism' DownloadOptions \p → case p of
  DownloadOptions p' → Just p'
  _ → Nothing

_Draftboard ∷ PrismP Port Unit
_Draftboard = prism' (const Draftboard) \p → case p of
  Draftboard → Just unit
  _ → Nothing

_ChartInstructions ∷ TraversalP Port (DSL OptionI)
_ChartInstructions = wander \f s → case s of
  ChartInstructions axes opts chty → (\opts' → ChartInstructions axes opts' chty) <$> f opts
  _ → pure s

_ChartAxes ∷ TraversalP Port Axes
_ChartAxes = wander \f s → case s of
  ChartInstructions axes opts chty → (\axes' → ChartInstructions axes' opts chty) <$> f axes
  Metric r@{ axes } → (\axes' → Metric r { axes = axes' }) <$> f axes
  PivotTable r@{ axes } → (\axes' → PivotTable r { axes = axes' }) <$> f axes
  _ → pure s

_Metric ∷ PrismP Port MetricPort
_Metric = prism' Metric case _ of
  Metric u → Just u
  _ → Nothing

_PivotTable ∷ PrismP Port PivotTablePort
_PivotTable = prism' PivotTable case _ of
  PivotTable u → Just u
  _ → Nothing
