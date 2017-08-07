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

module SlamData.Workspace.Card.Setups.Chart.Parallel.Component
  ( parallelBuilderComponent
  ) where

import SlamData.Prelude

import Data.Argonaut (JCursor)
import Data.Array as A
import Data.List as L
import Data.Lens ((^?), _Just, (^.), (?~))
import Data.Set as Set
import Data.StrMap as SM

import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP

import SlamData.Workspace.Card.CardType as CT
import SlamData.Workspace.Card.CardType.ChartType as CHT
import SlamData.Workspace.Card.Component as CC
import SlamData.Workspace.Card.Eval.State as ES
import SlamData.Workspace.Card.Model as M
import SlamData.Workspace.Card.Setups.CSS as CSS
import SlamData.Workspace.Card.Setups.Chart.Parallel.Component.ChildSlot as CS
import SlamData.Workspace.Card.Setups.Chart.Parallel.Component.Query as Q
import SlamData.Workspace.Card.Setups.Chart.Parallel.Component.State as ST
import SlamData.Workspace.Card.Setups.Chart.Parallel.Model as PM
import SlamData.Workspace.Card.Setups.Dimension as D
import SlamData.Workspace.Card.Setups.DimensionMap.Component as DM
import SlamData.Workspace.Card.Setups.DimensionMap.Component.Query as DQ
import SlamData.Workspace.Card.Setups.DimensionMap.Component.State as DS
import SlamData.Workspace.Card.Setups.Package.Types as TP
import SlamData.Workspace.Card.Setups.Package.DSL as P
import SlamData.Workspace.Card.Setups.Package.Projection as PP
import SlamData.Workspace.LevelOfDetails (LevelOfDetails(..))

type DSL = CC.InnerCardParentDSL ST.State Q.Query CS.ChildQuery CS.ChildSlot
type HTML = CC.InnerCardParentHTML Q.Query CS.ChildQuery CS.ChildSlot


-- This one is special because it cannot be encoded using PackageM due to
-- dynamic nature of available fields
package ∷ DS.Package
package = P.onPrism (M._BuildParallel ∘ _Just)
  { allFields
  , cursorMap
  , save
  , load
  }
  where
  allFields ∷ TP.DimensionMap → TP.Axes → L.List TP.Projection
  allFields dm _ =
    let
      flds = map PP._dimIx $ L.range 0 $ L.length $ dm ^. PP._dims
    in
      L.singleton PP._series <> flds <> L.singleton (PP._dimIx $ L.length flds)

  cursorMap ∷ TP.DimensionMap → TP.Axes → SM.StrMap (Set.Set JCursor)
  cursorMap dm axes =
    let
      seried ∷ SM.StrMap (Set.Set JCursor)
      seried =
        SM.empty
        # (TP.unpackProjection PP._series)
        ?~ (axes.category <> axes.time <> axes.date <> axes.datetime <> axes.value)

      flds ∷ L.List TP.Projection
      flds = map PP._dimIx $ L.range 0 $ L.length $ dm ^. PP._dims

      foldFn ∷ SM.StrMap (Set.Set JCursor) → TP.Projection → SM.StrMap (Set.Set JCursor)
      foldFn acc fld =
        let
          indices ∷ L.List Int
          indices = maybe L.Nil (flip sub one ⋙ L.range zero) $ PP.mbDimIx fld

          cursorSet ∷ Set.Set JCursor
          cursorSet =
            Set.fromFoldable
            $ L.catMaybes
            $ indices <#> \ind →
              dm ^? (TP.unpackProjection $ PP._dimIx ind) ∘ _Just ∘ D._value ∘ D._projection
        in
          acc # (TP.unpackProjection fld) ?~ (Set.difference axes.value cursorSet)
    in
     foldl foldFn seried flds


  save ∷ TP.DimensionMap → PM.ModelR → Maybe PM.ModelR
  save dm _ =
    { series: _
    , dims
    }
    <$> series
    where
    series = dm ^. (TP.unpackProjection PP._series)
    flds = map PP._dimIx $ L.range 0 $ L.length $ dm ^. PP._dims
    dims = A.catMaybes $ A.fromFoldable $ flds <#> \fld → dm ^. (TP.unpackProjection fld)

  load ∷ Maybe PM.ModelR → TP.DimensionMap → TP.DimensionMap
  load Nothing dm = dm
  load (Just m) dm =
    let
      dimsIxed ∷ Array (Int × D.LabeledJCursor)
      dimsIxed = A.mapWithIndex (\i a → i × a) m.dims

      seried ∷ TP.DimensionMap
      seried = dm # (TP.unpackProjection PP._series) ?~ m.series

      foldFn ∷ TP.DimensionMap → Int × D.LabeledJCursor → TP.DimensionMap
      foldFn acc (i × v) =
        acc # (TP.unpackProjection $ PP._dimIx i) ?~ v
    in
      foldl foldFn seried dimsIxed

parallelBuilderComponent ∷ CC.CardOptions → CC.CardComponent
parallelBuilderComponent =
  CC.makeCardComponent (CT.ChartOptions CHT.Parallel) $ H.parentComponent
    { render
    , eval: cardEval ⨁ setupEval
    , initialState: const ST.initialState
    , receiver: const Nothing
    }

render ∷ ST.State → HTML
render state =
  HH.div
    [ HP.classes [ CSS.chartEditor ] ]
    [ HH.slot' CS.cpDims unit (DM.component package) unit
        $ HE.input \l → right ∘ Q.HandleDims l
    ]

cardEval ∷ CC.CardEvalQuery ~> DSL
cardEval = case _ of
  CC.Activate next →
    pure next
  CC.Deactivate next →
    pure next
  CC.Save k → do
    let
      inp = M.BuildParallel $ Just
        { series: D.topDimension
        , dims: [ ]
        }
    out ← H.query' CS.cpDims unit $ H.request $ DQ.Save inp
    pure $ k case join out of
      Nothing → M.BuildParallel Nothing
      Just a → a
  CC.Load m next → do
    _ ← H.query' CS.cpDims unit $ H.action $ DQ.Load $ Just m
    pure next
  CC.ReceiveInput _ _ next →
    pure next
  CC.ReceiveOutput _ _ next →
    pure next
  CC.ReceiveState evalState next → do
    for_ (evalState ^? ES._Axes) \axes → do
      H.query' CS.cpDims unit $ H.action $ DQ.SetAxes axes
    pure next
  CC.ReceiveDimensions dims reply → do
    pure $ reply
      if dims.width < 576.0 ∨ dims.height < 416.0
      then Low
      else High

raiseUpdate ∷ DSL Unit
raiseUpdate =
  H.raise CC.modelUpdate

setupEval ∷ Q.Query ~> DSL
setupEval = case _ of
  Q.HandleDims q next → do
    case q of
      DQ.Update _ → raiseUpdate
    pure next
