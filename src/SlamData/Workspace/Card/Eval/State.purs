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

module SlamData.Workspace.Card.Eval.State
  ( EvalState(..)
  , initialEvalState
  , AnalysisR
  , AutoSelectR
  , TableR
  , GeoR
  , PivotTableR
  , _Analysis
  , _Axes
  , _Records
  , _Resource
  , _LastUsedResource
  , _AutoSelect
  , _ActiveTab
  , _Table
  , _PivotTable
  , _ChartOptions
  , _Geo
  , _SlamDown
  , _Leaflet
  , _BuildLeaflet
  , _Layers
  , _Controls
  ) where

import SlamData.Prelude

import Control.Monad.Aff (Aff)
import Data.Argonaut (Json)
import Data.Array as Array
import Data.Lens (Prism', prism', Traversal', wander, lens)
import Data.Set as Set
import ECharts.Monad (DSL)
import ECharts.Types.Phantom (OptionI)
import Leaflet.Core as LC
import SlamData.Effects (SlamDataEffects)
import SlamData.Workspace.Card.Chart.PivotTableRenderer.Common (PTree)
import SlamData.Workspace.Card.Model as CM
import SlamData.Workspace.Card.Markdown.Model (MarkdownExpr)
import SlamData.Workspace.Card.Port (Resource, PivotTablePort)
import SlamData.Workspace.Card.Setups.Axis (Axes)
import SlamData.Workspace.Card.Setups.Semantics as Sem
import Text.Markdown.SlamDown.Halogen.Component as SDH

type AnalysisR =
  { resource ∷ Resource
  , axes ∷ Axes
  , records ∷ Array Json
  }

type AutoSelectR =
  { lastUsedResource ∷ Resource
  , autoSelect ∷ Set.Set Sem.Semantics
  }

type TableR =
  { resource ∷ Resource
  , result ∷ Array Json
  , page ∷ Int
  , pageSize ∷ Int
  , size ∷ Int
  }

type GeoR =
  { leaflet ∷ Maybe LC.Leaflet
  , build ∷ LC.Leaflet → Aff SlamDataEffects (Array LC.Layer × Array LC.Control)
  , layers ∷ Array LC.Layer
  , controls ∷ Array LC.Control
  }

type PivotTableR =
  { resource ∷ Resource
  , result ∷ Array Json
  , buckets ∷ PTree Json Json
  , pageIndex ∷ Int
  , pageSize ∷ Int
  , pageCount ∷ Int
  , count ∷ Int
  , options ∷ PivotTablePort
  }

data EvalState
  = Analysis AnalysisR
  | AutoSelect AutoSelectR
  | ActiveTab Int
  | Table TableR
  | PivotTable PivotTableR
  | ChartOptions (DSL OptionI)
  | Geo GeoR
  | SlamDown (SDH.SlamDownFormState MarkdownExpr)

initialEvalState ∷ CM.AnyCardModel → Maybe EvalState
initialEvalState = case _ of
  CM.Tabs { tabs } → ActiveTab <$> (guard (Array.length tabs > 0) $> 0)
  CM.Geo _ →
    Just $ Geo { leaflet: Nothing, build: const $ pure $ [ ] × [ ] , layers: [ ], controls: [ ] }
  _ → Nothing

_Analysis ∷ Prism' EvalState AnalysisR
_Analysis = prism' Analysis case _ of
  Analysis x → Just x
  _ → Nothing

_Axes ∷ Traversal' EvalState Axes
_Axes = wander \f s → case s of
  Analysis r@{ axes } → Analysis ∘ r { axes = _} <$> f axes
  _ → pure s

_Records ∷ Traversal' EvalState (Array Json)
_Records = wander \f s → case s of
  Analysis r@{ records } → Analysis ∘ r { records = _} <$> f records
  _ → pure s

_Resource ∷ Traversal' EvalState Resource
_Resource = wander \f s → case s of
  Analysis r@{ resource } → Analysis ∘ r { resource = _} <$> f resource
  Table r@{ resource } → Table ∘ r { resource = _ } <$> f resource
  _ → pure s

_LastUsedResource ∷ Traversal' EvalState Resource
_LastUsedResource = wander \f s → case s of
  AutoSelect r@{ lastUsedResource } → AutoSelect ∘ r { lastUsedResource = _ } <$> f lastUsedResource
  _ → pure s

_AutoSelect ∷ Traversal' EvalState (Set.Set Sem.Semantics)
_AutoSelect = wander \f s → case s of
  AutoSelect r@{ autoSelect } → AutoSelect ∘ r { autoSelect = _ } <$> f autoSelect
  _ → pure s

_ActiveTab ∷ Prism' EvalState Int
_ActiveTab = prism' ActiveTab case _ of
  ActiveTab n → Just n
  _ → Nothing

_Table ∷ Prism' EvalState TableR
_Table = prism' Table case _ of
  Table r → Just r
  _ → Nothing

_PivotTable ∷ Prism' EvalState PivotTableR
_PivotTable = prism' PivotTable case _ of
  PivotTable r → Just r
  _ → Nothing

_ChartOptions ∷ Prism' EvalState (DSL OptionI)
_ChartOptions = prism' ChartOptions case _ of
  ChartOptions r → Just r
  _ → Nothing

_ResourceSize ∷ Traversal' EvalState Int
_ResourceSize = wander \f s → case s of
  Table r@{ size } → Table ∘ r { size = _ } <$> f size
  _ → pure s

_Geo ∷ Prism' EvalState GeoR
_Geo = prism' Geo case _ of
  Geo r → Just r
  _ → Nothing

_SlamDown ∷ Prism' EvalState (SDH.SlamDownFormState MarkdownExpr)
_SlamDown = prism' SlamDown case _ of
  SlamDown r → Just r
  _ → Nothing

_Leaflet ∷ Traversal' EvalState (Maybe LC.Leaflet)
_Leaflet = _Geo ∘ lens _.leaflet _{ leaflet = _ }

_BuildLeaflet
  ∷ Traversal' EvalState (LC.Leaflet → Aff SlamDataEffects (Array LC.Layer × Array LC.Control))
_BuildLeaflet = _Geo ∘ lens _.build _{ build = _ }

_Layers ∷ Traversal' EvalState (Array LC.Layer)
_Layers = _Geo ∘ lens _.layers _{ layers = _ }

_Controls ∷ Traversal' EvalState (Array LC.Control)
_Controls = _Geo ∘ lens _.controls _{ controls = _ }
