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
  , Out
  , DownloadPort
  , MetricPort
  , ChartInstructionsPort
  , PivotTablePort
  , SetupLabeledFormInputPort
  , SetupTextLikeFormInputPort
  , GeoChartPort
  , tagPort
  , emptyOut
  , varMapOut
  , resourceOut
  , defaultResourceVar
  , filterResources
  , extractResource
  , extractResourceVar
  , extractResourcePair
  , extractAnyFilePath
  , _Initial
  , _Terminal
  , _Variables
  , _SlamDown
  , _DownloadOptions
  , _Metric
  , _ChartInstructions
  , _PivotTable
  , _GeoChartPort
  , _osmURI
  , filePath
  , module SlamData.Workspace.Card.Port.VarMap
  ) where

import SlamData.Prelude

import Control.Monad.Aff (Aff)

import Data.Argonaut (JCursor, Json)
import Data.Lens (Prism', prism', Traversal', wander, lens)
import Data.List as List
import Data.Map as Map
import Data.Set as Set
import Data.URI (URIRef)

import ECharts.Monad (DSL)
import ECharts.Types.Phantom (OptionI)

import Leaflet.Core as LC

import SlamData.Effects (SlamDataEffects)
import SlamData.Download.Model (DownloadOptions)
import SlamData.Workspace.Card.CardId as CID
import SlamData.Workspace.Card.Error as CE
import SlamData.Workspace.Card.Setups.Chart.PivotTable.Model as PTM
import SlamData.Workspace.Card.Setups.Semantics as Sem
import SlamData.Workspace.Card.CardType.ChartType (ChartType)
import SlamData.Workspace.Card.CardType.FormInputType (FormInputType)
import SlamData.Workspace.Card.Markdown.Model as MD
import SlamData.Workspace.Card.Port.VarMap (Var(..), VarMap, URLVarMap, VarMapValue(..), Resource(..))
import SlamData.Workspace.Card.Port.VarMap as VM

import Text.Markdown.SlamDown as SD
import Utils.Path as PU

type Out = Port × VarMap

type DownloadPort =
  { resource ∷ VM.Resource
  , compress ∷ Boolean
  , options ∷ DownloadOptions
  , targetName ∷ String
  }

type MetricPort =
  { label ∷ Maybe String
  , value ∷ String
  }

type ChartInstructionsPort =
  { options ∷ Array Json → DSL OptionI
  , chartType ∷ ChartType
  }

type PivotTablePort =
  { dimensions ∷ Array (String × PTM.GroupByDimension)
  , columns ∷ Array (String × PTM.ColumnDimension)
  , isSimpleQuery ∷ Boolean
  }

type SetupLabeledFormInputPort =
  { name ∷ String
  , valueLabelMap ∷ Map.Map Sem.Semantics (Maybe String)
  , cursor ∷ JCursor
  , selectedValues ∷ Set.Set Sem.Semantics
  , formInputType ∷ FormInputType
  }

type SetupTextLikeFormInputPort =
  { name ∷ String
  , cursor ∷ JCursor
  , formInputType ∷ FormInputType
  }

type GeoChartPort =
  { build ∷ LC.Leaflet → Array Json → Aff SlamDataEffects (Array LC.Layer × Array LC.Control)
  , osmURI ∷ URIRef
  }

data Port
  = Initial
  | Terminal
  | Variables
  | CardError CE.CardError
  | ResourceKey String
  | SetupLabeledFormInput SetupLabeledFormInputPort
  | SetupTextLikeFormInput SetupTextLikeFormInputPort
  | SlamDown (SD.SlamDownP MD.MarkdownExpr)
  | ChartInstructions ChartInstructionsPort
  | DownloadOptions DownloadPort
  | ValueMetric MetricPort
  | CategoricalMetric MetricPort
  | PivotTable PivotTablePort
  | GeoChart GeoChartPort

tagPort ∷ Port → String
tagPort  = case _ of
  Initial → "Initial"
  Terminal → "Terminal"
  Variables → "Variables"
  CardError err → "CardError: " ⊕ CE.showCardError err
  ResourceKey str → "ResourceKey: " ⊕ show str
  SetupLabeledFormInput _ → "SetupLabeledFormInput"
  SetupTextLikeFormInput _ → "SetupTextLikeFormInput"
  SlamDown sd → "SlamDown"
  ChartInstructions _ → "ChartInstructions"
  DownloadOptions _ → "DownloadOptions"
  ValueMetric _ → "ValueMetric"
  CategoricalMetric _ → "CategoricalMetric"
  PivotTable _ → "PivotTable"
  GeoChart _ → "GeoChart"

filterResources ∷ VarMap → List.List (Var × Resource)
filterResources = List.mapMaybe (uncurry go) ∘ Map.toUnfoldable ∘ VM.snapshot
  where
    go key (Resource res) = Just (key × res)
    go _ _ = Nothing

extractResource ∷ VarMap → Maybe Resource
extractResource = map snd ∘ extractResourcePair

extractResourceVar ∷ VarMap → Maybe Var
extractResourceVar = map fst ∘ extractResourcePair

extractResourcePair ∷ VarMap → Maybe (Var × Resource)
extractResourcePair = List.head ∘ filterResources

extractAnyFilePath ∷ VarMap → Maybe PU.AnyFilePath
extractAnyFilePath = map filePath ∘ extractResource

defaultResourceVar ∷ String
defaultResourceVar = "results"

emptyOut ∷ Out
emptyOut = Initial × VM.empty

varMapOut ∷ VarMap → Out
varMapOut v = Variables × v

resourceOut ∷ CID.CardId → Resource → VarMap → Out
resourceOut cid r vm = ResourceKey defaultResourceVar × VM.insert cid (VM.Var defaultResourceVar) (Resource r) vm

_Initial ∷ Prism' Port Unit
_Initial = prism' (const Initial) case _ of
  Initial → Just unit
  _ → Nothing

_Terminal ∷ Prism' Port Unit
_Terminal = prism' (const Terminal) case _ of
  Terminal → Just unit
  _ → Nothing

_Variables ∷ Prism' Port Unit
_Variables = prism' (const Variables) case _ of
  Variables → Just unit
  _ → Nothing

_SlamDown ∷ Traversal' Port (SD.SlamDownP MD.MarkdownExpr)
_SlamDown = wander \f s → case s of
  SlamDown sd → SlamDown <$> f sd
  _ → pure s

_DownloadOptions ∷ Prism' Port DownloadPort
_DownloadOptions = prism' DownloadOptions $ case _ of
  DownloadOptions p' → Just p'
  _ → Nothing

_ChartInstructions ∷ Traversal' Port (Array Json → DSL OptionI)
_ChartInstructions = wander \f s → case s of
  ChartInstructions o → ChartInstructions ∘ o{options = _} <$> f o.options
  _ → pure s

_Metric ∷ Traversal' Port MetricPort
_Metric = wander \f s → case s of
  ValueMetric m → map ValueMetric $ f m
  CategoricalMetric m → map CategoricalMetric $ f m
  _ → pure s

_PivotTable ∷ Prism' Port PivotTablePort
_PivotTable = prism' PivotTable case _ of
  PivotTable u → Just u
  _ → Nothing

_GeoChartPort ∷ Prism' Port GeoChartPort
_GeoChartPort = prism' GeoChart case _ of
  GeoChart u → Just u
  _ → Nothing

_osmURI ∷ Traversal' Port URIRef
_osmURI = _GeoChartPort ∘ lens _.osmURI _{ osmURI = _ }

filePath ∷ Resource → Either PU.FilePath PU.RelFilePath
filePath = case _ of
  Path fp → Left fp
  View fp _ _ → Right fp
  Process fp _ _ → Right fp
