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
  , Resource(..)
  , DataMap
  , Out
  , DownloadPort
  , MetricPort
  , ChartInstructionsPort
  , PivotTablePort
  , SetupLabeledFormInputPort
  , SetupTextLikeFormInputPort
  , tagPort
  , emptyOut
  , terminalOut
  , varMapOut
  , resourceOut
  , portOut
  , defaultResourceVar
  , filterResources
  , extractResource
  , extractFilePath
  , flattenResources
  , resourceToVarMapValue
  , _Initial
  , _Terminal
  , _Variables
  , _SlamDown
  , _DownloadOptions
  , _CardError
  , _Metric
  , _ChartInstructions
  , _PivotTable
  , _filePath
  , module SlamData.Workspace.Card.Port.VarMap
  ) where

import SlamData.Prelude

import Data.Argonaut (JCursor)
import Data.Lens (Prism', prism', Traversal', wander, Lens', lens, (^.), view)
import Data.List as List
import Data.Map as Map
import Data.Path.Pathy as Path
import Data.Set as Set
import Data.StrMap as SM

import ECharts.Monad (DSL)
import ECharts.Types.Phantom (OptionI)

import SlamData.Download.Model (DownloadOptions)
import SlamData.Workspace.Card.Setups.Chart.PivotTable.Model as PTM
import SlamData.Workspace.Card.Setups.Semantics as Sem
import SlamData.Workspace.Card.CardType.ChartType (ChartType)
import SlamData.Workspace.Card.CardType.FormInputType (FormInputType)
import SlamData.Workspace.Card.Port.VarMap (VarMap, URLVarMap, VarMapValue(..), renderVarMapValue, emptyVarMap, escapeIdentifier)
import Text.Markdown.SlamDown as SD
import Utils.Path as PU

data Resource
  = Path PU.FilePath
  | View PU.FilePath String DataMap

derive instance eqResource ∷ Eq Resource

type DataMap = SM.StrMap (Either Resource VarMapValue)

type Out = Port × DataMap

type DownloadPort =
  { resource ∷ PU.FilePath
  , compress ∷ Boolean
  , options ∷ DownloadOptions
  , targetName ∷ String
  }

type MetricPort =
  { label ∷ Maybe String
  , value ∷ String
  }

type ChartInstructionsPort =
  { options ∷ DSL OptionI
  , chartType ∷ ChartType
  }

type PivotTablePort =
  { dimensions ∷ Array (String × JCursor)
  , columns ∷ Array (String × PTM.Column)
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

data Port
  = Initial
  | Terminal
  | Variables
  | CardError String
  | ResourceKey String
  | SetupLabeledFormInput SetupLabeledFormInputPort
  | SetupTextLikeFormInput SetupTextLikeFormInputPort
  | SlamDown (SD.SlamDownP VarMapValue)
  | ChartInstructions ChartInstructionsPort
  | DownloadOptions DownloadPort
  | ValueMetric MetricPort
  | CategoricalMetric MetricPort
  | PivotTable PivotTablePort

tagPort ∷ Port → String
tagPort  = case _ of
  Initial → "Initial"
  Terminal → "Terminal"
  Variables → "Variables"
  CardError str → "CardError: " ⊕ show str
  ResourceKey str → "ResourceKey: " ⊕ show str
  SetupLabeledFormInput _ → "SetupLabeledFormInput"
  SetupTextLikeFormInput _ → "SetupTextLikeFormInput"
  SlamDown sd → "SlamDown: " ⊕ show sd
  ChartInstructions _ → "ChartInstructions"
  DownloadOptions _ → "DownloadOptions"
  ValueMetric _ → "ValueMetric"
  CategoricalMetric _ → "CategoricalMetric"
  PivotTable _ → "PivotTable"

filterResources ∷ DataMap → SM.StrMap Resource
filterResources = SM.fold go SM.empty
  where
    go m key (Left res) = SM.insert key res m
    go m _ _ = m

extractResource ∷ DataMap → Maybe Resource
extractResource = map snd ∘ List.head ∘ SM.toList ∘ filterResources

extractFilePath ∷ DataMap → Maybe PU.FilePath
extractFilePath = map (view _filePath) ∘ extractResource

flattenResources ∷ DataMap → VarMap
flattenResources = map go
  where
    go (Left val) = resourceToVarMapValue val
    go (Right val) = val

resourceToVarMapValue ∷ Resource → VarMapValue
resourceToVarMapValue r = QueryExpr (escapeIdentifier (Path.printPath (r ^. _filePath)))

defaultResourceVar ∷ String
defaultResourceVar = "results"

emptyOut ∷ Out
emptyOut = Initial × SM.empty

terminalOut ∷ Out
terminalOut = Terminal × SM.empty

varMapOut ∷ DataMap → Out
varMapOut v = Variables × v

resourceOut ∷ Resource → Out
resourceOut r = ResourceKey defaultResourceVar × SM.singleton defaultResourceVar (Left r)

portOut ∷ Port → Out
portOut p = p × SM.empty

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

_SlamDown ∷ Traversal' Port (SD.SlamDownP VarMapValue)
_SlamDown = wander \f s → case s of
  SlamDown sd → SlamDown <$> f sd
  _ → pure s

_CardError ∷ Prism' Port String
_CardError = prism' CardError \p → case p of
  CardError x → Just x
  _ → Nothing

_DownloadOptions ∷ Prism' Port DownloadPort
_DownloadOptions = prism' DownloadOptions \p → case p of
  DownloadOptions p' → Just p'
  _ → Nothing

_ChartInstructions ∷ Traversal' Port (DSL OptionI)
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

_filePath ∷ Lens' Resource PU.FilePath
_filePath = lens get set
  where
    get (Path fp) = fp
    get (View fp _ _) = fp

    set (Path _) fp = Path fp
    set (View _ a b) fp = View fp a b
