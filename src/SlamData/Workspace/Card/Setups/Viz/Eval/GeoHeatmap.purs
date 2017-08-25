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

module SlamData.Workspace.Card.Setups.Viz.Eval.GeoHeatmap where


import SlamData.Prelude

import Control.Monad.Aff (Aff)
import Control.Monad.Eff.Ref (readRef)
import Data.Argonaut (Json, decodeJson, (.?))
import Data.Array ((!!))
import Data.Array as A
import Data.Foldable as F
import Data.Int as Int
import Data.List as L
import Data.Variant (prj)
import Graphics.Canvas as G
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Halogen.VDom.DOM.StringRenderer as VDS
import Leaflet.Core as LC
import Leaflet.Plugin.Heatmap as LH
import Math ((%), log)
import SlamData.Effects (SlamDataEffects)
import SlamData.Workspace.Card.CardType as CT
import SlamData.Workspace.Card.Port as Port
import SlamData.Workspace.Card.Setups.Auxiliary as Aux
import SlamData.Workspace.Card.Setups.Auxiliary.GeoHeatmap (State)
import SlamData.Workspace.Card.Setups.Common as SC
import SlamData.Workspace.Card.Setups.Common.Eval as BCE
import SlamData.Workspace.Card.Setups.Dimension as D
import SlamData.Workspace.Card.Setups.DimensionMap.Projection as P
import SlamData.Workspace.Card.Setups.Semantics as Sem
import SlamData.Workspace.Card.Setups.Viz.Eval.Common (VizEval)
import SqlSquared as Sql
import Unsafe.Coerce (unsafeCoerce)

eval ∷ ∀ m. VizEval m (P.DimMap → Aux.State → Port.Port → m Port.Out)
eval dimMap aux = BCE.chartSetupEval buildSql buildPort aux'
  where
  aux' = prj CT._geoHeatmap aux
  buildSql = SC.buildBasicSql (buildProjections dimMap) (buildGroupBy dimMap)
  buildPort r axes = Port.GeoChart { build: build dimMap r, osmURI: r.osm.uri }

buildProjections ∷ ∀ a. P.DimMap → a → L.List (Sql.Projection Sql.Sql)
buildProjections dimMap _ = L.fromFoldable $ A.concat
  [ SC.dimensionProjection P.lat dimMap "lat"
  , SC.dimensionProjection P.lng dimMap "lng"
  , SC.measureProjection P.intensity dimMap "intensity"
  ]

buildGroupBy ∷ ∀ a. P.DimMap → a → Maybe (Sql.GroupBy Sql.Sql)
buildGroupBy dimMap _ = SC.groupBy
  $ SC.sqlProjection P.lat dimMap
  <|> SC.sqlProjection P.lng dimMap

type Item =
  { lat ∷ LC.Degrees
  , lng ∷ LC.Degrees
  , i ∷ Number
  }

decodeItem ∷ Json → String ⊹ Item
decodeItem = decodeJson >=> \obj → do
  latSem ← obj .? "lat"
  lngSem ← obj .? "lng"
  latNum ← maybe (Left "lat has incorrect semantics") Right $ Sem.maybeNumber latSem
  lngNum ← maybe (Left "lng has incorrect semantics") Right $ Sem.maybeNumber lngSem
  lat ← maybe (Left "incorrect degrees, should be impossible") Right $ LC.mkDegrees $ latNum % 360.0
  lng ← maybe (Left "incorrect degrees, should be impossible") Right $ LC.mkDegrees $ lngNum % 360.0
  i ← map (fromMaybe zero ∘ Sem.maybeNumber) $ obj .? "intensity"
  pure { lat, lng, i }

build
  ∷ P.DimMap
  → State
  → LC.Leaflet
  → Array Json
  → Aff SlamDataEffects (Array LC.Layer × Array LC.Control)
build dimMap r leaf records = do
  heatmap ← LC.layer
  let
    items = mkItems records
    minLng = mkMinLng items
    maxLng = mkMaxLng items
    minLat = mkMinLat items
    maxLat = mkMaxLat items
    latDiff = maxLat - minLat
    lngDiff = maxLng - minLng
    avgLat = mkAvgLat items
    avgLng = mkAvgLng items
    zoomLat = 360.0 / latDiff
    zoomLng = 360.0 / lngDiff
    zoomInt = Int.ceil $ (log $ min zoomLat zoomLng) / log 2.0
    maxI = maxIntensity items
    asNumArray = unsafeCoerce
    onClickHandler cvsRef e = do
      mcnv ← readRef cvsRef
      for_ mcnv \cnvs → do
        mbPt ← LC.eventContainerPoint e
        for_ mbPt \(ex × ey) → do
          width ← G.getCanvasWidth cnvs
          height ← G.getCanvasHeight cnvs
          ctx ← G.getContext2D cnvs
          imgData ← G.getImageData ctx 0.0 0.0 width height
          mll ← LC.eventLatLng e
          for mll \ll → do
            let
              intArr = asNumArray $ G.imageDataBuffer imgData
              redIx = (ey * Int.floor width + ey) * 4
              alpha = Int.toNumber $ fromMaybe zero $ intArr !! (redIx + 3)
              mkTableRow mbDim strVal = flip foldMap mbDim \dim →
                [ HH.tr_ [ HH.td_ [ HH.text $ D.jcursorLabel dim ]
                         , HH.td_ [ HH.text strVal ]
                         ]
                ]
              content = VDS.render absurd $ unwrap
                $ HH.table [ HP.class_ $ HH.ClassName "sd-chart-tooltip-table" ]
                $ mkTableRow (P.lookup P.lat dimMap) (show $ LC.degreesToNumber ll.lat)
                ⊕ mkTableRow (P.lookup P.lng dimMap) (show $ LC.degreesToNumber ll.lng)
                ⊕ mkTableRow (P.lookup P.intensity dimMap) (show $ alpha * maxI / 256.0)

            popup ← LC.popup { minHeight: 32 }
            _ ← LC.setLatLng ll popup
            _ ← LC.setContent content popup
            _ ← LC.openOn leaf popup
            pure unit


  zoom ← LC.mkZoom zoomInt
  view ← LC.mkLatLng avgLat avgLng
  _ ← LC.setZoom zoom leaf
  _ ← LC.setView view leaf
  cvs ← LH.mkHeatmap LH.defaultOptions{maxIntensity = maxI} items heatmap leaf
  LC.on "click" (onClickHandler cvs) $ LC.mapToEvented leaf

  pure $ [ heatmap ] × [ ]
  where
  mkItems ∷ Array Json → Array Item
  mkItems = foldMap (foldMap A.singleton ∘ decodeItem)

  maxIntensity ∷ Array Item → Number
  maxIntensity = fromMaybe one ∘ map _.i ∘ A.head ∘ A.sortBy (\a b → compare b.i a.i)

  mkMaxLat ∷ Array Item → Number
  mkMaxLat = fromMaybe zero ∘ A.head ∘ A.reverse ∘ A.sort ∘ map (LC.degreesToNumber ∘ _.lat)

  mkMaxLng ∷ Array Item → Number
  mkMaxLng = fromMaybe zero ∘ A.head ∘ A.reverse ∘ A.sort ∘ map (LC.degreesToNumber ∘ _.lng)

  mkMinLat ∷ Array Item → Number
  mkMinLat = fromMaybe zero ∘ A.head ∘ A.sort ∘ map (LC.degreesToNumber ∘ _.lat)

  mkMinLng ∷ Array Item → Number
  mkMinLng = fromMaybe zero ∘ A.head ∘ A.sort ∘ map (LC.degreesToNumber ∘ _.lng)

  mkAvgLat ∷ Array Item → Number
  mkAvgLat items =
    F.sum lats / (Int.toNumber $ A.length lats)
    where
    lats = map (LC.degreesToNumber ∘ _.lat) items

  mkAvgLng ∷ Array Item → Number
  mkAvgLng items =
    F.sum lngs / (Int.toNumber $ A.length lngs)
    where
    lngs = map (LC.degreesToNumber ∘ _.lng) items
