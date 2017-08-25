{-
Copyright 2017 SlamData, Inc.

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

module SlamData.Workspace.Card.Setups.Viz.Eval
  ( eval
  ) where

import SlamData.Prelude

import Control.Monad.State (get, put)
import Data.Argonaut as J
import Data.Array as A
import Data.Lens ((^?))
import Data.List as L
import Data.ListMap as LM
import Data.Variant (on)
import Data.Variant as V
import SlamData.Workspace.Card.CardType as CT
import SlamData.Workspace.Card.CardType.VizType as VT
import SlamData.Workspace.Card.Error as CE
import SlamData.Workspace.Card.Eval.Monad as CEM
import SlamData.Workspace.Card.Port as Port
import SlamData.Workspace.Card.Port.VarMap as VM
import SlamData.Workspace.Card.Setups.Auxiliary as Aux
import SlamData.Workspace.Card.Setups.Common.Eval as BCE
import SlamData.Workspace.Card.Setups.Dimension as D
import SlamData.Workspace.Card.Setups.DimensionMap.Package as PS
import SlamData.Workspace.Card.Setups.DimensionMap.Projection as P
import SlamData.Workspace.Card.Setups.PivotTable.Eval as PivotTable
import SlamData.Workspace.Card.Setups.Semantics as Sem
import SlamData.Workspace.Card.Setups.Viz.Error as VE
import SlamData.Workspace.Card.Setups.Viz.Eval.Area as Area
import SlamData.Workspace.Card.Setups.Viz.Eval.Bar as Bar
import SlamData.Workspace.Card.Setups.Viz.Eval.Boxplot as Boxplot
import SlamData.Workspace.Card.Setups.Viz.Eval.Candlestick as Candlestick
import SlamData.Workspace.Card.Setups.Viz.Eval.Common (VizEval)
import SlamData.Workspace.Card.Setups.Viz.Eval.Funnel as Funnel
import SlamData.Workspace.Card.Setups.Viz.Eval.Gauge as Gauge
import SlamData.Workspace.Card.Setups.Viz.Eval.GeoHeatmap as GeoHeatmap
import SlamData.Workspace.Card.Setups.Viz.Eval.GeoMarker as GeoMarker
import SlamData.Workspace.Card.Setups.Viz.Eval.Graph as Graph
import SlamData.Workspace.Card.Setups.Viz.Eval.Heatmap as Heatmap
import SlamData.Workspace.Card.Setups.Viz.Eval.Line as Line
import SlamData.Workspace.Card.Setups.Viz.Eval.Parallel as Parallel
import SlamData.Workspace.Card.Setups.Viz.Eval.Pie as Pie
import SlamData.Workspace.Card.Setups.Viz.Eval.PunchCard as PunchCard
import SlamData.Workspace.Card.Setups.Viz.Eval.Radar as Radar
import SlamData.Workspace.Card.Setups.Viz.Eval.Sankey as Sankey
import SlamData.Workspace.Card.Setups.Viz.Eval.Scatter as Scatter
import SlamData.Workspace.Card.Setups.Viz.Eval.Select as Select
import SlamData.Workspace.Card.Setups.Viz.Model (Model)

lm ∷ ∀ a. LM.Module VT.VizType a
lm = LM.openModule eq


eval ∷ ∀ m. VizEval m (Model → Port.Port → m Port.Out)
eval m port = do
  initEvalState
  checkMissingProjections
  case CT.contractToPivot m.vizType of
    Just _ → evalPivot
    Nothing → do
      iOut ← for (CT.contractToInput m.vizType) evalInput
      sOut ← for (CT.contractToSelect m.vizType) evalSelect
      gOut ← for (CT.contractToGeo m.vizType) evalGeo
      cOut ← for (CT.contractToChart m.vizType) evalChart
      stOut ← for (CT.contractToStatic m.vizType) evalStatic
      maybe throwUnexpected pure
        $ iOut <|> sOut <|> gOut <|> cOut <|> stOut

  where
  initEvalState ∷ m Unit
  initEvalState = do
    var × resource ← CEM.extractResourcePair port
    records × axes ← BCE.analyze resource =<< get
    put $ Just $ CEM.Analysis { resource, records, axes }

  handleEvalState ∷ m (Port.Resource × Array J.Json)
  handleEvalState = do
    _ × resource ← CEM.extractResourcePair port
    records × _ ← BCE.analyze resource =<< get
    pure $ resource × records

  throwUnexpected ∷ ∀ a. m a
  throwUnexpected =
    CE.throw "Unexpected visualization type, please contact support"

  getDimMap ∷ m P.DimMap
  getDimMap = maybe
    ( CE.throw
       $ "Setup Visualization model has no field for "
       ⊕ VT.name m.vizType )
    pure
    $ lm.lookup m.vizType m.dimMaps

  getAux ∷ m Aux.State
  getAux = maybe
    ( CE.throw
        $ "Setup Visualization model has no auxiliary model for "
        ⊕ VT.name m.vizType
    )
    pure
    $ lm.lookup m.vizType m.auxes

  evalPivot ∷ m Port.Out
  evalPivot = do
    aux ← getAux
    model ←
      maybe
        ( CE.throw "Incorrect pivot table model" )
        pure
        $ V.prj CT._pivot aux
    PivotTable.eval model port

  evalInput ∷ CT.Input () → m Port.Out
  evalInput formInputType = do
    _ × resource ← CEM.extractResourcePair port
    dimMap ← getDimMap
    -- Already checked presented in DSL interpretation
    let
      projection = unsafePartial fromJust $ P.lookup P.formValue dimMap
      p = Port.SetupInput { projection, formInputType }
    wrapPort p resource

  wrapPort ∷ Port.Port → Port.Resource → m Port.Out
  wrapPort p r = do
    CEM.CardEnv { varMap, cardId } ← ask
    pure (p × VM.insert cardId (VM.Var Port.defaultResourceVar) (VM.Resource r) varMap)

  evalStatic ∷ CT.Static () → m Port.Out
  evalStatic _ = do
    resource × records ← handleEvalState
    wrapPort (p records) resource
    where
    p records = Port.CategoricalMetric { value: value records, label: Nothing }
    value records = fromMaybe "" do
      record ← A.head records
      dimMap ← lm.lookup m.vizType m.dimMaps
      dim ← P.lookup P.value dimMap
      cursor ← dim ^? D._value ∘ D._projection
      Sem.getMaybeString record cursor

  evalSelect ∷ CT.Select () → m Port.Out
  evalSelect formInputType = do
    resource × records ← handleEvalState
    dimMap ← getDimMap
    p ← Select.eval dimMap records formInputType
    wrapPort p resource

  evalGeo ∷ CT.Geo () → m Port.Out
  evalGeo gct = do
    dimMap ← getDimMap
    aux ← getAux
    case_
      # on CT._geoMarker (const $ GeoMarker.eval dimMap aux port)
      # on CT._geoHeatmap (const $ GeoHeatmap.eval dimMap aux port)
      $ gct

  evalChart ∷ CT.Chart () → m Port.Out
  evalChart cht = do
    dimMap ← getDimMap
    case_
      # on CT._area (const $ getAux >>= \aux → Area.eval dimMap aux port)
      # on CT._bar (const $ getAux >>= \aux → Bar.eval dimMap aux port)
      # on CT._boxplot (const $ Boxplot.eval dimMap port)
      # on CT._candlestick (const $ Candlestick.eval dimMap port)
      # on CT._funnel (const $ getAux >>= \aux → Funnel.eval dimMap aux port)
      # on CT._gauge (const $ getAux >>= \aux → Gauge.eval dimMap aux port)
      # on CT._graph (const $ getAux >>= \aux → Graph.eval dimMap aux port)
      # on CT._heatmap (const $ getAux >>= \aux → Heatmap.eval dimMap aux port)
      # on CT._line (const $ getAux >>= \aux → Line.eval dimMap aux port)
      # on CT._metric (const $ handleEvalState >>= \(_ × records) → evalMetric records)
      # on CT._parallel (const $ Parallel.eval dimMap port)
      # on CT._pie (const $ Pie.eval dimMap port)
      # on CT._punchCard (const $ getAux >>= \aux → PunchCard.eval dimMap aux port)
      # on CT._radar (const $ Radar.eval dimMap port)
      # on CT._sankey (const $ Sankey.eval dimMap port)
      # on CT._scatter (const $ getAux >>= \aux → Scatter.eval dimMap aux port)
      # on CT._pivot (const $ CE.throw "Pivot table case should be handled separately")
      $ cht

  evalMetric ∷ Array J.Json → m Port.Out
  evalMetric records = do
    _ × resource ← CEM.extractResourcePair port
    wrapPort p resource
    where
    p = Port.CategoricalMetric { value, label: Nothing }
    value = fromMaybe "" do
      record ← A.head records
      dimMap ← lm.lookup m.vizType m.dimMaps
      dim ← P.lookup P.value dimMap
      cursor ← dim ^? D._value ∘ D._projection
      Sem.getMaybeString record cursor


  checkMissingProjections ∷ m Unit
  checkMissingProjections = do
    let
      dimMap = fromMaybe P.empty $ lm.lookup m.vizType m.dimMaps
      missingProjections = L.filter (not ∘ flip P.member dimMap) requiredProjections
    unless (L.null missingProjections)
      $ VE.throw $ { missingProjections, vizType: m.vizType }

  requiredProjections ∷ L.List P.Projection
  requiredProjections = foldMap _.requiredFields $ lm.lookup m.vizType PS.packages
