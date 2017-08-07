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

module SlamData.Workspace.Card.Setups.DimensionMap.Defaults
  ( ProjectionDefaults
  , getDefaults
  , dynamicMeasure
  , isFlat
  , availableTransforms
  ) where

import SlamData.Prelude
import Data.Argonaut (JCursor)
import Data.Int as Int
import Data.Lens ((?~), (^.))
import Data.StrMap as SM
import SlamData.Workspace.Card.Setups.Dimension as D
import SlamData.Workspace.Card.Setups.Package.DSL as T
import SlamData.Workspace.Card.Setups.Package.Projection as Pr
import SlamData.Workspace.Card.Setups.Transform as Tr
import SlamData.Workspace.Card.Setups.Transform.Aggregation as Ag

type ProjectionDefaults =
  { dimension ∷ JCursor → D.LabeledJCursor
  , label ∷ String
  , value ∷ String
  , select ∷ String
  , deselectable ∷ Boolean
  }

jcursorProjection ∷ JCursor → D.Dimension Void JCursor
jcursorProjection = D.projection

getDefaults ∷ T.Projection → ProjectionDefaults
getDefaults prj = case defaults ^. T.unpackProjection prj of
  Just a  → a
  Nothing → dynamicMeasure prj

defaults ∷ SM.StrMap ProjectionDefaults
defaults = foldr insertProjection SM.empty statics
  where
  insertProjection (prj × val) =
    T.unpackProjection prj ?~ val

statics ∷ Array (T.Projection × ProjectionDefaults)
statics =
  [ Pr._dimension ×
      { dimension: jcursorProjection
      , label: "Dimension"
      , value: "Choose dimension"
      , select: "Choose dimension"
      , deselectable: true
      }
  , Pr._high ×
      { dimension: D.projectionWithAggregation $ Just Ag.Sum
      , label: "High"
      , value: "Choose high"
      , select: "Choose measure for high position"
      , deselectable: false
      }
  , Pr._low ×
      { dimension: D.projectionWithAggregation $ Just Ag.Sum
      , label: "Low"
      , value: "Choose low"
      , select: "Choose measure for low position"
      , deselectable: false
      }
  , Pr._open ×
      { dimension: D.projectionWithAggregation $ Just Ag.Sum
      , label: "Open"
      , value: "Choose open"
      , select: "Choose measure for open position"
      , deselectable: false
      }
  , Pr._close ×
      { dimension: D.projectionWithAggregation $ Just Ag.Sum
      , label: "Close"
      , value: "Choose close"
      , select: "Choose measure for close position"
      , deselectable: false
      }
  , Pr._parallel ×
      { dimension: jcursorProjection
      , label: "Parallel"
      , value: "Choose parallel"
      , select: "Choose parallel"
      , deselectable: true
      }
  , Pr._value ×
      { dimension: D.projectionWithAggregation $ Just Ag.Sum
      , label: "Measure #1"
      , value: "Choose measure"
      , select: "Choose measure"
      , deselectable: false
      }
  , Pr._series ×
      { dimension: jcursorProjection
      , label: "Series"
      , value: "Choose series"
      , select: "Choose series"
      , deselectable: true
      }
  , Pr._category ×
      { dimension: jcursorProjection
      , label: "Category"
      , value: "Choose category"
      , select: "Choose category"
      , deselectable: true
      }
  , Pr._stack ×
      { dimension: jcursorProjection
      , label: "Stack"
      , value: "Choose stack"
      , select: "Choose stack"
      , deselectable: true
      }
  , Pr._source ×
      { dimension: jcursorProjection
      , label: "Source"
      , value: "Choose source"
      , select: "Choose source"
      , deselectable: true
      }
  , Pr._target ×
      { dimension: jcursorProjection
      , label: "Target"
      , value: "Choose target"
      , select: "Choose target"
      , deselectable: true
      }
  , Pr._abscissa ×
      { dimension: jcursorProjection
      , label: "X-axis"
      , value: "Choose x-axis"
      , select: "Choose x-axis"
      , deselectable: true
      }
  , Pr._ordinate ×
      { dimension: D.projectionWithAggregation $ Just Ag.Sum
      , label: "Y-axis"
      , value: "Choose y-axis"
      , select: "Choose y-axis"
      , deselectable: false
      }
  , Pr._secondValue ×
      { dimension: D.projectionWithAggregation $ Just Ag.Sum
      , label: "Measure #2"
      , value: "Choose measure"
      , select: "Choose measure"
      , deselectable: false
      }
  , Pr._donut ×
      { dimension: jcursorProjection
      , label: "Donut"
      , value: "Choose donut"
      , select: "Choose donut"
      , deselectable: true
      }
  , Pr._multiple ×
      { dimension: jcursorProjection
      , label: "Multiple"
      , value: "Choose multiple"
      , select: "Choose multiple"
      , deselectable: true
      }
  , Pr._size ×
      { dimension: D.projectionWithAggregation $ Just Ag.Sum
      , label: "Size"
      , value: "Choose size"
      , select: "Choose size"
      , deselectable: false
      }
  , Pr._color ×
      { dimension: jcursorProjection
      , label: "Color"
      , value: "Choose color"
      , select: "Choose color"
      , deselectable: true
      }
  , Pr._scatterOrdinate ×
      { dimension: D.projectionWithAggregation $ Just Ag.Sum
      , label: "Y-axis"
      , value: "Choose y-axis"
      , select: "Choose y-axis"
      , deselectable: true
      }
  , Pr._scatterSize ×
      { dimension: D.projectionWithAggregation $ Just Ag.Sum
      , label: "Size"
      , value: "Choose size"
      , select: "Choose size"
      , deselectable: true
      }
  , Pr._lat ×
      { dimension: jcursorProjection
      , label: "Latitude"
      , value: "Choose latitude"
      , select: "Choose latitude"
      , deselectable: true
      }
  , Pr._lng ×
      { dimension: jcursorProjection
      , label: "Longitude"
      , value: "Choose longitude"
      , select: "Choose longitude"
      , deselectable: true
      }
  , Pr._intensity ×
      { dimension: D.projectionWithAggregation $ Just Ag.Sum
      , label: "Intensity"
      , value: "Choose intensity"
      , select: "Choose intensity"
      , deselectable: false
      }
  ]

dynamicMeasure ∷ T.Projection → ProjectionDefaults
dynamicMeasure prj =
  { dimension: D.projectionWithAggregation $ Just Ag.Sum
  , label: "Measure" <> maybe "" (\o → " #" <> show o) offset
  , value: "Choose measure"
  , select: "Choose measure"
  , deselectable: false
  }
  where
  offset = map (add 1) ∘ Int.fromString =<< T.idReflection ^. T.unpackProjection prj

-- | Some projections shouldn't be aggregated
isFlat ∷ T.Projection → Boolean
isFlat prj = case T.idReflection ^. T.unpackProjection prj of
  Just "flatValue" → true
  _ → false

isMeasure ∷ T.Projection → Boolean
isMeasure prj = case T.idReflection ^. T.unpackProjection prj of
  Nothing → false
  Just s
    | isJust $ Int.fromString s → true
    | otherwise →
      s ≡ "value"
        ∨ s ≡ "flatValue"
        ∨ s ≡ "intensity"
        ∨ s ≡ "size"
        ∨ s ≡ "open"
        ∨ s ≡ "close"
        ∨ s ≡ "high"
        ∨ s ≡ "low"
        ∨ s ≡ "secondValue"

availableTransforms ∷ T.Projection → Maybe Tr.Transform → Array Tr.Transform
availableTransforms prj mbTr
  | isMeasure prj = Tr.aggregationTransforms
  | otherwise = fold
      [ Tr.dateTransforms
      , Tr.timeTransforms
      , Tr.dateTimeTransforms
      , Tr.numericTransforms mbTr
      , Tr.stringTransforms
      ]
