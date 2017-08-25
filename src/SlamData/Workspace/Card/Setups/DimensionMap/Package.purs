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

module SlamData.Workspace.Card.Setups.DimensionMap.Package where

import SlamData.Prelude

import Data.Argonaut as J
import Data.Lens ((^?))
import Data.List ((:))
import Data.List as L
import Data.ListMap as LM
import Data.Set as Set
import SlamData.Workspace.Card.CardType as CT
import SlamData.Workspace.Card.CardType.VizType as VT
import SlamData.Workspace.Card.Setups.Axis as Ax
import SlamData.Workspace.Card.Setups.Dimension as D
import SlamData.Workspace.Card.Setups.DimensionMap.DSL (field, addSource, isFilteredBy, isActiveWhen, optional, addAll)
import SlamData.Workspace.Card.Setups.DimensionMap.DSL as DSL
import SlamData.Workspace.Card.Setups.DimensionMap.Projection as Pr

lm ∷ ∀ v. LM.Module VT.VizType v
lm = LM.openModule eq

type Package = DSL.Package (Set.Set J.JCursor) ()

interpret ∷ DSL.DimensionM Unit → Package
interpret = DSL.interpret axesComposer

axesComposer ∷ Pr.AxesComposer (Set.Set J.JCursor)
axesComposer = { filter, guard }
  where
  filter ∷ Pr.Projection → Pr.DimMap → Set.Set J.JCursor → Set.Set J.JCursor
  filter prj dimMap s = maybe s (flip Set.delete s) do
    jc ← Pr.lookup prj dimMap
    jc ^? D._value ∘ D._projection

  guard ∷ Pr.Projection → Pr.DimMap → Set.Set J.JCursor → Set.Set J.JCursor
  guard prj dimMap s =  foldMap id do
    jc ← Pr.lookup prj dimMap
    _ ← jc ^? D._value ∘ D._projection
    pure s

lookup ∷ VT.VizType → Maybe Package
lookup vt = lm.lookup vt packages

getPackage ∷ VT.VizType → Maybe Package
getPackage vt = lm.lookup vt packages

packages ∷ LM.ListMap VT.VizType Package
packages = lm.union customMap $ map interpret mapFromFree
  where
  geoMarkerPack =
    { allFields: \dm _ →
       Pr.lat : Pr.lng : Pr.size : Pr.series
       : ( map Pr.dimIx $ L.range 0 $ 1 + L.length ( Pr.dims dm ) )

    , axesRequirements:
        DSL.noneRequirements { value = 2 }

    , requiredFields: Pr.lat : Pr.lng : L.Nil

    , cursorMap: \dm axes →
       let
         lattify
           ∷ Pr.ProjectionMap (Set.Set J.JCursor)
           → Pr.ProjectionMap (Set.Set J.JCursor)
         lattify =
           Pr.insert Pr.lat axes.value

         longify
           ∷ Pr.ProjectionMap (Set.Set J.JCursor)
           → Pr.ProjectionMap (Set.Set J.JCursor)
         longify =
           Pr.insert Pr.lng $ axesComposer.filter Pr.lat dm axes.value

         latLngFilter
           ∷ Set.Set J.JCursor
           → Set.Set J.JCursor
         latLngFilter =
           axesComposer.guard Pr.lat dm
           ∘ axesComposer.guard Pr.lng dm
           ∘ axesComposer.filter Pr.lat dm
           ∘ axesComposer.filter Pr.lng dm

         seriify
           ∷ Pr.ProjectionMap (Set.Set J.JCursor)
           → Pr.ProjectionMap (Set.Set J.JCursor)
         seriify =
           Pr.insert Pr.series
           $ latLngFilter
           $ axes.category <> axes.date <> axes.datetime <> axes.time

         sizify
           ∷ Pr.ProjectionMap (Set.Set J.JCursor)
           → Pr.ProjectionMap (Set.Set J.JCursor)
         sizify =
           Pr.insert Pr.size $ latLngFilter axes.value

         prepared ∷ Pr.ProjectionMap (Set.Set J.JCursor)
         prepared =
           Pr.empty # lattify ∘ longify ∘ seriify ∘ sizify

         flds ∷ L.List Pr.Projection
         flds = map Pr.dimIx $ L.range 0 $ L.length $ Pr.dims dm

         foldFn
           ∷ Pr.ProjectionMap (Set.Set J.JCursor)
           → Pr.Projection
           → Pr.ProjectionMap (Set.Set J.JCursor)
         foldFn acc fld =
           let
             indices ∷ L.List Int
             indices = maybe L.Nil (L.range zero ∘ flip sub one) $ Pr.mbDimIx fld

             cursorSet ∷ Set.Set J.JCursor
             cursorSet =
               Set.fromFoldable
               $ L.catMaybes
               $ indices <#> \ind → do
                 jc ← Pr.lookup (Pr.dimIx ind) dm
                 jc ^? D._value ∘ D._projection
           in
            Pr.insert fld (latLngFilter $ Set.difference axes.value cursorSet) acc
       in
        foldl foldFn prepared flds
    }

  parallelPack =
    { allFields: \dm _ →
       Pr.series : ( map Pr.dimIx $ L.range 0 $ 1 + (L.length $ Pr.dims dm) )

    , axesRequirements:
        DSL.noneRequirements { value = 2 }

    , requiredFields:
        Pr.dimIx 0 : Pr.dimIx 1 : L.Nil

    , cursorMap: \dm axes →
       let
         seried
           ∷ Pr.ProjectionMap (Set.Set J.JCursor)
         seried =
           Pr.insert Pr.series (axes.category <> axes.time <> axes.date <> axes.datetime) Pr.empty

         flds
           ∷ L.List Pr.Projection
         flds = map Pr.dimIx $ L.range 0 $ L.length $ Pr.dims dm

         foldFn
           ∷ Pr.ProjectionMap (Set.Set J.JCursor)
           → Pr.Projection
           → Pr.ProjectionMap (Set.Set J.JCursor)
         foldFn acc fld =
           let
             indices ∷ L.List Int
             indices = maybe L.Nil (flip sub one ⋙ L.range zero) $ Pr.mbDimIx fld

             cursorSet ∷ Set.Set J.JCursor
             cursorSet =
               Set.fromFoldable
               $ L.catMaybes
               $ indices <#> \ind → do
                 jc ← Pr.lookup (Pr.dimIx ind) dm
                 jc ^? D._value ∘ D._projection
           in
            Pr.insert fld (Set.difference axes.value cursorSet) acc
       in
        foldl foldFn seried flds
    }

  customMap = LM.fromFoldable
    [ CT.geoMarker × geoMarkerPack
    , CT.parallel × parallelPack
    ]

  mapFromFree = LM.fromFoldable [
    CT.text × do
      _ ←
        field Pr.formValue
          >>= addSource Ax._value
          >>= addSource Ax._category
          >>= addSource Ax._time
          >>= addSource Ax._date
          >>= addSource Ax._datetime
      pure unit

  , CT.numeric × do
      _ ←
        field Pr.formValue
          >>= addSource Ax._value
      pure unit

  , CT.date × do
      _ ←
        field Pr.formValue
          >>= addSource Ax._date
      pure unit

  , CT.time × do
      _ ←
        field Pr.formValue
          >>= addSource Ax._time
      pure unit

  , CT.datetime × do
      _ ←
        field Pr.formValue
          >>= addSource Ax._time
      pure unit

  , CT.dropdown × do
      value ←
        field Pr.formValue
          >>= addSource Ax._value
          >>= addSource Ax._category
          >>= addSource Ax._time
          >>= addSource Ax._date
          >>= addSource Ax._datetime

      label ←
        optional Pr.formLabel
          >>= addSource Ax._category
          >>= isFilteredBy value

      selected ←
        optional Pr.formSelected
          >>= addSource Ax._category
          >>= addSource Ax._value
          >>= addSource Ax._date
          >>= addSource Ax._time
          >>= addSource Ax._datetime
          >>= isFilteredBy value
          >>= isFilteredBy label
          >>= isActiveWhen value

      pure unit

  , CT.radio × do
      value ←
        field Pr.formValue
          >>= addSource Ax._value
          >>= addSource Ax._category
          >>= addSource Ax._time
          >>= addSource Ax._date
          >>= addSource Ax._datetime

      label ←
        optional Pr.formLabel
          >>= addSource Ax._category
          >>= isFilteredBy value

      selected ←
        optional Pr.formSelected
          >>= addSource Ax._category
          >>= addSource Ax._value
          >>= addSource Ax._date
          >>= addSource Ax._time
          >>= addSource Ax._datetime
          >>= isFilteredBy value
          >>= isFilteredBy label
          >>= isActiveWhen value
      pure unit

  , CT.checkbox × do
      value ←
        field Pr.formValue
          >>= addSource Ax._value
          >>= addSource Ax._category
          >>= addSource Ax._time
          >>= addSource Ax._date
          >>= addSource Ax._datetime

      label ←
        optional Pr.formLabel
          >>= addSource Ax._category
          >>= isFilteredBy value

      selected ←
        optional Pr.formSelected
          >>= addSource Ax._category
          >>= addSource Ax._value
          >>= addSource Ax._date
          >>= addSource Ax._time
          >>= addSource Ax._datetime
          >>= isFilteredBy value
          >>= isFilteredBy label
          >>= isActiveWhen value
      pure unit

  , CT.geoHeatmap × do
      lat ←
        field Pr.lat
          >>= addSource Ax._value

      lng ←
        field Pr.lng
          >>= addSource Ax._value
          >>= isFilteredBy lat

      intensity ←
        field Pr.intensity
          >>= addSource Ax._value
          >>= isFilteredBy lat
          >>= isFilteredBy lng
      pure unit

  , CT.pie × do
      category ←
        field Pr.category
          >>= addAll

      value ←
        field Pr.value
          >>= addSource Ax._value

      donut ←
        optional Pr.donut
          >>= addAll
          >>= addSource Ax._time
          >>= isFilteredBy category
          >>= isActiveWhen category
          >>= isFilteredBy value

      parallel ←
        optional Pr.parallel
          >>= addAll
          >>= isFilteredBy value
          >>= isFilteredBy category
          >>= isFilteredBy donut
          >>= isActiveWhen category
      pure unit

 , CT.line × do
      dimension ←
        field Pr.dimension
          >>= addAll

      value ←
        field Pr.value
          >>= addSource Ax._value
          >>= isFilteredBy dimension

      secondValue ←
        optional Pr.secondValue
          >>= addSource Ax._value
          >>= isFilteredBy value

      size ←
        optional Pr.size
          >>= addSource Ax._value
          >>= isFilteredBy dimension
          >>= isFilteredBy value
          >>= isFilteredBy secondValue
          >>= isActiveWhen value

      series ←
        optional Pr.series
          >>= addAll
          >>= isFilteredBy value
          >>= isFilteredBy secondValue
          >>= isFilteredBy size
          >>= isFilteredBy dimension
          >>= isActiveWhen dimension
      pure unit

  , CT.bar × do
      category ←
        field Pr.category
          >>= addAll

      value ←
        field Pr.value
          >>= addSource Ax._value
          >>= isFilteredBy category

      stack ←
        optional Pr.stack
          >>= addAll
          >>= isFilteredBy category
          >>= isFilteredBy value
          >>= isActiveWhen category

      parallel ←
        optional Pr.parallel
          >>= addAll
          >>= isFilteredBy category
          >>= isFilteredBy stack
          >>= isFilteredBy value
          >>= isActiveWhen category
      pure unit

  , CT.area × do
      dimension ←
        field Pr.dimension
          >>= addAll

      value ←
        field Pr.value
          >>= isFilteredBy dimension
          >>= addSource Ax._value

      series ←
        optional Pr.series
          >>= addAll
          >>= isFilteredBy value
          >>= isFilteredBy dimension
          >>= isActiveWhen dimension
      pure unit

  , CT.scatter × do
      abscissa ←
        field Pr.abscissa
          >>= addSource Ax._value

      ordinate ←
        field Pr.scatterOrdinate
          >>= addSource Ax._value
          >>= isFilteredBy abscissa

      size ←
        optional Pr.scatterSize
          >>= addSource Ax._value
          >>= isFilteredBy abscissa
          >>= isFilteredBy ordinate

      series ←
        optional Pr.series
          >>= addAll
          >>= isFilteredBy ordinate
          >>= isFilteredBy abscissa
          >>= isFilteredBy size
          >>= isActiveWhen ordinate
          >>= isActiveWhen abscissa

      parallel ←
        optional Pr.parallel
          >>= addAll
          >>= isFilteredBy series
          >>= isActiveWhen series

      pure unit

  , CT.radar × do
      category ←
        field Pr.category
          >>= addAll

      value ←
        field  Pr.value
          >>= addSource Ax._value

      multiple ←
        optional Pr.multiple
          >>= addAll
          >>= isFilteredBy category
          >>= isFilteredBy value
          >>= isActiveWhen category

      parallel ←
        optional Pr.parallel
          >>= addAll
          >>= isFilteredBy category
          >>= isFilteredBy multiple
          >>= isFilteredBy value
          >>= isActiveWhen category

      pure unit

  , CT.funnel × do
      category ←
        field Pr.category
          >>= addAll

      value ←
        field Pr.value
          >>= addSource Ax._value

      series ←
        optional Pr.series
          >>= addAll
          >>= isFilteredBy value
          >>= isFilteredBy category
          >>= isActiveWhen category

      pure unit

  , CT.graph × do
      source ←
        field Pr.source
          >>= addSource Ax._category

      target ←
        field Pr.target
          >>= addSource Ax._category
          >>= isFilteredBy source

      size ←
        optional Pr.size
          >>= addSource Ax._value

      color ←
        optional Pr.color
          >>= addAll
          >>= isFilteredBy size
          >>= isFilteredBy source
          >>= isFilteredBy target

      pure unit

  , CT.heatmap × do
      abscissa ←
        field Pr.abscissa
          >>= addAll

      ordinate ←
        field Pr.ordinate
          >>= addAll
          >>= isFilteredBy abscissa

      value ←
        field Pr.value
          >>= addSource Ax._value
          >>= isFilteredBy abscissa
          >>= isFilteredBy ordinate

      series ←
        optional Pr.series
          >>= addAll
          >>= isFilteredBy value
          >>= isActiveWhen value
          >>= isFilteredBy abscissa
          >>= isFilteredBy ordinate
          >>= isActiveWhen abscissa
          >>= isActiveWhen ordinate
      pure unit

  , CT.sankey × do
      source ←
        field Pr.source
          >>= addSource Ax._category

      target ←
        field Pr.target
          >>= addSource Ax._category
          >>= isFilteredBy source

      value ←
        field Pr.value
          >>= addSource Ax._value

      pure unit

  , CT.gauge × do
      value ←
        field Pr.value
          >>= addSource Ax._value

      parallel ←
        optional Pr.parallel
          >>= addAll
          >>= isFilteredBy value
          >>= isActiveWhen value

      multiple ←
        optional Pr.multiple
          >>= addAll
          >>= isFilteredBy value
          >>= isActiveWhen value
          >>= isFilteredBy parallel

      pure unit

  , CT.boxplot × do
      dimension ←
        field Pr.dimension
          >>= addAll

      value ←
        field Pr.flatValue
          >>= addSource Ax._value
          >>= isFilteredBy dimension

      series ←
        optional Pr.series
          >>= addAll
          >>= isFilteredBy dimension
          >>= isFilteredBy dimension
          >>= isActiveWhen dimension


      parallel ←
        optional Pr.parallel
          >>= addAll
          >>= isFilteredBy value
          >>= isFilteredBy dimension
          >>= isFilteredBy series
          >>= isActiveWhen dimension
      pure unit

  , CT.punchCard × do
      abscissa ←
        field Pr.abscissa
          >>= addAll

      ordinate ←
        field Pr.ordinate
          >>= addAll

      value ←
        field Pr.value
          >>= addSource Ax._value
          >>= isFilteredBy abscissa
          >>= isFilteredBy ordinate

      pure unit

  , CT.candlestick × do
      dimension ←
        field Pr.dimension
          >>= addAll

      open ←
        field Pr.open
          >>= addSource Ax._value

      close ←
        field Pr.close
          >>= addSource Ax._value
          >>= isFilteredBy open

      high ←
        field Pr.high
          >>= addSource Ax._value
          >>= isFilteredBy open
          >>= isFilteredBy close

      low ←
        field Pr.low
          >>= addSource Ax._value
          >>= isFilteredBy open
          >>= isFilteredBy close
          >>= isFilteredBy high

      parallel ←
        optional Pr.parallel
          >>= addAll
          >>= isFilteredBy dimension
          >>= isActiveWhen dimension
          >>= isFilteredBy open
          >>= isFilteredBy close
          >>= isFilteredBy high
          >>= isFilteredBy low

      pure unit

  , CT.metric × do
      _ ←
        field Pr.value
          >>= addSource Ax._value
      pure unit
  , CT.static × do
      _ ←
        field Pr.value
          >>= addSource Ax._category
      pure unit
  , CT.pivot × pure unit
  ]
