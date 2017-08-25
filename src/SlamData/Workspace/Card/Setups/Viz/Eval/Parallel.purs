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

module SlamData.Workspace.Card.Setups.Viz.Eval.Parallel where

import SlamData.Prelude

import Data.Argonaut (JArray, Json, decodeJson, (.?))
import Data.Array as A
import Data.List as L
import Data.StrMap as Sm
import Data.String as S
import ECharts.Commands as E
import ECharts.Monad (DSL)
import ECharts.Types as ET
import ECharts.Types.Phantom (OptionI)
import SlamData.Workspace.Card.CardType as CT
import SlamData.Workspace.Card.Port as Port
import SlamData.Workspace.Card.Setups.ColorScheme (colors)
import SlamData.Workspace.Card.Setups.Common as SC
import SlamData.Workspace.Card.Setups.Common.Eval as BCE
import SlamData.Workspace.Card.Setups.Dimension as D
import SlamData.Workspace.Card.Setups.DimensionMap.Projection as P
import SlamData.Workspace.Card.Setups.Semantics as Sem
import SlamData.Workspace.Card.Setups.Viz.Eval.Common (VizEval)
import SqlSquared as Sql

type Item =
  { series ∷ String
  , dims ∷ Array Number
  }

decodeItem ∷ Json → String ⊹ Item
decodeItem = decodeJson >=> \obj → do
  series ← map (fromMaybe "" ∘ Sem.maybeString) $ obj .? "series"
  let
    ks ∷ Array String
    ks = map ("measure" ⊕ _) $ A.mapMaybe (S.stripPrefix $ S.Pattern "measure") $ Sm.keys obj

  dims ← for ks \k →
    map (fromMaybe zero ∘ Sem.maybeNumber) $ obj .? k

  pure { dims, series }

eval ∷ ∀ m. VizEval m (P.DimMap → Port.Port → m Port.Out)
eval dimMap =
  BCE.chartSetupEval buildSql buildPort $ Just unit
  where
  buildPort r axes = Port.ChartInstructions
    { options: options dimMap axes r ∘ buildData
    , chartType: CT.parallel
    }

  buildSql = SC.buildBasicSql (buildProjections dimMap) (buildGroupBy dimMap)

buildProjections ∷ ∀ a. P.DimMap → a → L.List (Sql.Projection Sql.Sql)
buildProjections dimMap _ = L.fromFoldable $ A.concat
  $ [ SC.dimensionProjection P.series dimMap "series" ]
  ⊕ ( foldMap mkProjection $ P.dims dimMap )
  where
  mkProjection (ix × _) = [ SC.measureProjection (P.dimIx ix) dimMap $ "measure" ⊕ show ix ]

buildGroupBy ∷ ∀ a. P.DimMap → a → Maybe (Sql.GroupBy Sql.Sql)
buildGroupBy dimMap _ = SC.groupBy $ SC.sqlProjection P.series dimMap

buildData ∷ JArray → Array Item
buildData =
  foldMap $ foldMap A.singleton ∘ decodeItem

options ∷ ∀ a ax. P.DimMap → ax → a → Array Item → DSL OptionI
options dimMap _ _ pData = do
  E.parallel do
    E.left $ ET.Percent 5.0
    E.right $ ET.Percent 18.0
    E.bottom $ ET.Pixel 100

  E.colors colors
  E.series series

  E.parallelAxes axes

  when (A.length serieNames < 30) $ E.legend do
    E.topBottom
    E.textStyle $ E.fontFamily "Ubuntu, sans"
    E.items $ map ET.strItem serieNames

  where
  serieNames = map _.series pData

  series = for_ pData \serie → E.parallelSeries do
    E.name serie.series
    E.buildItems
      $ E.addItem
      $ E.buildValues
      $ for_ serie.dims E.addValue

  axes = for_ (P.dims dimMap) \(dimIx × dim) → E.addParallelAxis do
    E.dim dimIx
    E.name $ D.jcursorLabel dim
