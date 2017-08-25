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

module SlamData.Workspace.Card.Setups.Viz.Eval.GeoMarker where


import SlamData.Prelude


import CSS as CSS
import Color as C
import Control.Monad.Aff (Aff)
import Data.Argonaut (Json, decodeJson, (.?))
import Data.Array as A
import Data.Foldable as F
import Data.Int as Int
import Data.List ((!!))
import Data.List as L
import Data.Path.Pathy (currentDir, file, dir, (</>), (<.>))
import Data.StrMap as Sm
import Data.String as S
import Data.URI (URIRef)
import Data.URI as URI
import Data.Variant (prj)
import Halogen.HTML as HH
import Halogen.HTML.CSS as HC
import Halogen.HTML.Properties as HP
import Halogen.VDom.DOM.StringRenderer as VDS
import Leaflet.Core as LC
import Math ((%), log)
import SlamData.Effects (SlamDataEffects)
import SlamData.Workspace.Card.CardType as CT
import SlamData.Workspace.Card.Port as Port
import SlamData.Workspace.Card.Setups.Auxiliary as Aux
import SlamData.Workspace.Card.Setups.Auxiliary.GeoMarker (State)
import SlamData.Workspace.Card.Setups.ColorScheme (colors)
import SlamData.Workspace.Card.Setups.Common as SC
import SlamData.Workspace.Card.Setups.Common.Eval as BCE
import SlamData.Workspace.Card.Setups.Dimension as D
import SlamData.Workspace.Card.Setups.DimensionMap.Projection as P
import SlamData.Workspace.Card.Setups.Semantics as Sem
import SlamData.Workspace.Card.Setups.Viz.Eval.Common (VizEval)
import SqlSquared as Sql
import Utils.Array (enumerate)

eval ∷ ∀ m. VizEval m (P.DimMap → Aux.State → Port.Port → m Port.Out)
eval dimMap aux = BCE.chartSetupEval buildSql buildPort aux'
  where
  aux' = prj CT._geoMarker aux
  buildSql = SC.buildBasicSql (buildProjections dimMap) (buildGroupBy dimMap)
  buildPort r axes = Port.GeoChart { build: build dimMap r ,osmURI: r.osm.uri }

buildProjections ∷ P.DimMap → State → L.List (Sql.Projection Sql.Sql)
buildProjections dimMap r = L.fromFoldable $ A.concat
  $ [ SC.dimensionProjection P.lat dimMap "lat"
    , SC.dimensionProjection P.lng dimMap "lng"
    , SC.dimensionProjection P.series dimMap "series"
    , SC.measureProjection P.size dimMap "size"
    ]
  ⊕ ( A.mapWithIndex (\ix _ → SC.measureProjection (P.dimIx ix) dimMap $ "measure" ⊕ show ix)
      $ A.fromFoldable $ P.dims dimMap
    )

buildGroupBy ∷ P.DimMap → State → Maybe (Sql.GroupBy Sql.Sql)
buildGroupBy dimMap r =
  SC.groupBy
  $ SC.sqlProjection P.lat dimMap
  <|> SC.sqlProjection P.lng dimMap
  <|> SC.sqlProjection P.series dimMap

type Item =
  { lat ∷ LC.Degrees
  , lng ∷ LC.Degrees
  , size ∷ Number
  , series ∷ String
  , dims ∷ Array Number
  }

decodeItem ∷ Json → String ⊹ Item
decodeItem = decodeJson >=> \obj → do
  latSem ← obj .? "lat"
  lngSem ← obj .? "lng"
  latNum ← maybe (Left "lat has incorrect semantics") Right $ Sem.maybeNumber latSem
  lngNum ← maybe (Left "lng has incorrect semantics") Right $ Sem.maybeNumber lngSem
  lat ← maybe (Left "incorrect degrees, should be impossible") Right $ LC.mkDegrees $ latNum % 360.0
  lng ← maybe (Left "incorrect degrees, should be impossible") Right $ LC.mkDegrees $ lngNum % 360.0
  size ← map (fromMaybe zero ∘ Sem.maybeNumber) $ obj .? "size"
  series ← map (fromMaybe "" ∘ Sem.maybeString) $ obj .? "series"
  let
    ks ∷ Array String
    ks = map ("measure" <> _) $ A.mapMaybe (S.stripPrefix $ S.Pattern "measure") $ Sm.keys obj
  dims ← for ks \k →
    map (fromMaybe zero ∘ Sem.maybeNumber) $ obj .? k
  pure { lat
       , lng
       , size
       , series
       , dims
       }


iconConf ∷ { iconUrl ∷ URIRef, iconSize ∷ LC.Point }
iconConf =
  { iconUrl: Right $ URI.RelativeRef
      (URI.RelativePart Nothing $ Just $ Right
       $ currentDir </> dir "img" </> file "marker" <.> "svg"
      )
      Nothing
      Nothing
  , iconSize: 40 × 40
  }

build
  ∷ P.DimMap
  → State
  → LC.Leaflet
  → Array Json
  → Aff SlamDataEffects (Array LC.Layer × Array LC.Control)
build dimMap r leaf records = do
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
    series = mkSeries items
    minSize = mkMinSize items
    maxSize = mkMaxSize items
    sizeDistance = r.size.max - r.size.min
    distance = maxSize - minSize
    mkRadius size
      | distance ≡ 0.0 = r.size.min
      | not (P.member P.size dimMap) = r.size.min
      | otherwise = r.size.max - sizeDistance / distance * (maxSize - size)
    foldFn icon {layers, overlays} item@{lat, lng} = do
      let
        color s = unsafePartial fromJust $ Sm.lookup s series
      layer
        -- Renderer is too slow for png/svg icons :(
        ← if (P.member P.series dimMap) ∨ (P.member P.size dimMap) ∨ A.length items > 1000
          then
            map LC.circleMarkerToLayer
              $ LC.circleMarker
                {lat, lng}
                { radius: mkRadius item.size
                , color: color item.series
                }
          else map LC.markerToLayer $ LC.marker {lat, lng} >>= LC.setIcon icon

      let
        mkTableRow mbDim strVal = flip foldMap mbDim \dim →
          [ HH.tr_ [ HH.td_ [ HH.text $ D.jcursorLabel dim ]
                   , HH.td_ [ HH.text strVal ]
                   ]
          ]

        content = VDS.render absurd $ unwrap
          $ HH.table [ HP.class_ $ HH.ClassName "sd-chart-tooltip-table" ]
          $ mkTableRow (P.lookup P.lat dimMap) (show $ LC.degreesToNumber lat )
          ⊕ mkTableRow (P.lookup P.lng dimMap) (show $ LC.degreesToNumber lng )
          ⊕ mkTableRow (P.lookup P.size dimMap) (show item.size)
          ⊕ mkTableRow (P.lookup P.series dimMap) item.series
          ⊕ (fold $ enumerate item.dims <#> \(ix × dim) →
              mkTableRow (map snd $ P.dims dimMap !! ix) (show dim))

        alterFunction Nothing = Just [ layer ]
        alterFunction (Just a) = Just $ A.cons layer a

        mkKey s =
          VDS.render absurd ∘ unwrap
            $ HH.table [ HP.class_ $ HH.ClassName "sd-chart-geo-layers-control" ]
              [ HH.tr_ [ HH.td_ [ HH.span [ HP.class_ $ HH.ClassName "sd-chart-tooltip-color"
                                          , HC.style $ CSS.backgroundColor $ color s
                                          ] [ ]
                                ]
                       , HH.td_ [ HH.text item.series ]
                       ]
              ]


      _ ← LC.bindPopup content layer

      pure { layers: A.cons layer layers
           , overlays: Sm.alter alterFunction (mkKey item.series) overlays
           }

  zoom ← LC.mkZoom zoomInt
  view ← LC.mkLatLng avgLat avgLng

  icon ← LC.icon iconConf

  {layers, overlays} ←
    A.foldRecM (foldFn icon) {layers: [ ], overlays: Sm.empty } items

  layGroups ← for overlays LC.layerGroup

  control ← case P.lookup P.series dimMap of
    Nothing → pure [ ]
    Just _ → do
      c ← LC.layers Sm.empty layGroups { collapsed: false }
      _ ← LC.addTo leaf c
      pure [ c ]


  _ ← LC.setZoom zoom leaf
  _ ← LC.setView view leaf
  let
    toSend
      | Sm.isEmpty layGroups = layers
      | otherwise = Sm.values $ map LC.groupToLayer layGroups
  pure $ toSend × control
  where
  mkItems ∷ Array Json → Array Item
  mkItems = foldMap $ foldMap A.singleton ∘ decodeItem

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

  mkMinSize ∷ Array Item → Number
  mkMinSize = fromMaybe zero ∘ A.head ∘ A.sort ∘ map _.size

  mkMaxSize ∷ Array Item → Number
  mkMaxSize = fromMaybe one ∘ A.head ∘ A.reverse ∘ A.sort ∘ map _.size

  mkSeries ∷ Array Item → Sm.StrMap C.Color
  mkSeries items = Sm.fromFoldable $ A.zip (A.sort $ A.nub $ map _.series items) colors
