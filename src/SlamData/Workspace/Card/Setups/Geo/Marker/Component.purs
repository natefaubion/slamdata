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

module SlamData.Workspace.Card.Setups.Geo.Marker.Component
  ( component
  ) where

import SlamData.Prelude

import Data.Argonaut (JCursor)
import Data.Array as A
import Data.Lens (_Just, (^?), (?~), (^.), (.~))
import Data.List ((:))
import Data.List as L
import Data.StrMap as SM
import Data.String.Regex as RX
import Data.String.Regex.Flags as RXF
import Data.String.Regex.Unsafe as URX
import Data.Set as Set
import Data.URI (runParseURIRef, printURIRef)

import Global (readFloat, isNaN, decodeURIComponent)

import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.HTML.Properties.ARIA as ARIA
import Halogen.Themes.Bootstrap3 as B

import SlamData.Render.Common (row)
import SlamData.Workspace.Card.Geo.Model (onURIRef)
import SlamData.Render.ClassName as CN
import SlamData.Workspace.Card.Component as CC
import SlamData.Workspace.Card.CardType as CT
import SlamData.Workspace.Card.CardType.GeoChartType as GcT
import SlamData.Workspace.Card.Eval.State as ES
import SlamData.Workspace.Card.Setups.Geo.Marker.Component.State as ST
import SlamData.Workspace.Card.Setups.Geo.Marker.Component.Query as Q
import SlamData.Workspace.Card.Setups.Geo.Marker.Component.ChildSlot as CS
import SlamData.Workspace.LevelOfDetails (LevelOfDetails(..))
import SlamData.Workspace.Card.Setups.Dimension as D
import SlamData.Workspace.Card.Setups.CSS as CSS
import SlamData.Workspace.Card.Setups.DimensionMap.Component as DM
import SlamData.Workspace.Card.Setups.DimensionMap.Component.Query as DQ
import SlamData.Workspace.Card.Setups.DimensionMap.Component.State as DS
import SlamData.Workspace.Card.Setups.Package.Types as TP
import SlamData.Workspace.Card.Setups.Package.DSL as P
import SlamData.Workspace.Card.Setups.Package.Projection as PP
import SlamData.Workspace.Card.Model as M
import SlamData.Workspace.Card.Setups.Geo.Marker.Model as GM

import Utils.Array (enumerate)

type DSL = CC.InnerCardParentDSL ST.State Q.Query CS.ChildQuery CS.ChildSlot
type HTML = CC.InnerCardParentHTML Q.Query CS.ChildQuery CS.ChildSlot

package ∷ DS.Package
package = P.onPrism (M._SetupGeoMarker ∘ _Just)
  { allFields
  , cursorMap
  , save
  , load
  }
  where
  allFields ∷ TP.DimensionMap → TP.Axes → L.List TP.Projection
  allFields dm _ =
    let
      flds = map PP._dimIx $ L.range 0 $ 1 + (L.length $ dm ^. PP._dims)
    in
     PP._lat
     : PP._lng
     : PP._size
     : PP._series
     : flds

  cursorMap ∷ TP.DimensionMap → TP.Axes → SM.StrMap (Set.Set JCursor)
  cursorMap dm axes =
    let
      lattify ∷ SM.StrMap (Set.Set JCursor) → SM.StrMap (Set.Set JCursor)
      lattify = TP.unpackProjection PP._lat ?~ axes.value

      longify ∷ SM.StrMap (Set.Set JCursor) → SM.StrMap (Set.Set JCursor)
      longify =
        TP.unpackProjection PP._lng ?~ DS.axesComposer.filter PP._lat dm axes.value

      latLngFilter ∷ Set.Set JCursor → Set.Set JCursor
      latLngFilter =
        DS.axesComposer.guard PP._lat dm
        ∘ DS.axesComposer.guard PP._lng dm
        ∘ DS.axesComposer.filter PP._lat dm
        ∘ DS.axesComposer.filter PP._lng dm

      seriify ∷ SM.StrMap (Set.Set JCursor) → SM.StrMap (Set.Set JCursor)
      seriify =
        TP.unpackProjection PP._series
        ?~ (latLngFilter $ axes.category <> axes.date <> axes.datetime <> axes.time <> axes.value)

      sizify ∷ SM.StrMap (Set.Set JCursor) → SM.StrMap (Set.Set JCursor)
      sizify =
        TP.unpackProjection PP._size ?~ latLngFilter axes.value

      prepared ∷ SM.StrMap (Set.Set JCursor)
      prepared =
        SM.empty # lattify ∘ longify ∘ seriify ∘ sizify

      flds ∷ L.List TP.Projection
      flds = map PP._dimIx $ L.range 0 $ L.length $ dm ^. PP._dims

      foldFn ∷ SM.StrMap (Set.Set JCursor) → TP.Projection → SM.StrMap (Set.Set JCursor)
      foldFn acc fld =
        let
          indices ∷ L.List Int
          indices = maybe L.Nil (L.range zero ∘ flip sub one) $ PP.mbDimIx fld

          cursorSet ∷ Set.Set JCursor
          cursorSet =
            Set.fromFoldable
            $ L.catMaybes
            $ indices <#> \ind →
              dm ^? (TP.unpackProjection $ PP._dimIx ind) ∘ _Just ∘ D._value ∘ D._projection
        in
          acc # (TP.unpackProjection fld) ?~ (latLngFilter $ Set.difference axes.value cursorSet)
    in
      foldl foldFn prepared flds

  save ∷ TP.DimensionMap → GM.ModelR → GM.Model
  save dm mr =
    { lat: _
    , lng: _
    , size
    , series
    , dims
    , minSize: mr.minSize
    , maxSize: mr.maxSize
    , osmURI: mr.osmURI
    }
    <$> lat
    <*> lng
    where
    lat = dm ^. TP.unpackProjection PP._lat
    lng = dm ^. TP.unpackProjection PP._lng
    size = dm ^. TP.unpackProjection PP._size
    series = dm ^. TP.unpackProjection PP._series
    flds = map PP._dimIx $ L.range 0 $ L.length $ dm ^. PP._dims
    dims = A.catMaybes $ A.fromFoldable $ flds <#> \fld → dm ^. TP.unpackProjection fld

  load ∷ GM.Model → TP.DimensionMap → TP.DimensionMap
  load Nothing dm = dm
  load (Just m) dm =
    let
      prepared ∷ TP.DimensionMap
      prepared = dm
        # (TP.unpackProjection PP._lat ?~ m.lat)
        ∘ (TP.unpackProjection PP._lng ?~ m.lng)
        ∘ (TP.unpackProjection PP._size .~ m.size)
        ∘ (TP.unpackProjection PP._series .~ m.series)

      foldFn ∷ TP.DimensionMap → Int × D.LabeledJCursor → TP.DimensionMap
      foldFn acc (i × v) = acc
        # ((TP.unpackProjection $ PP._dimIx i) ?~ v)
    in
      foldl foldFn prepared $ enumerate m.dims


component ∷ CC.CardOptions → CC.CardComponent
component =
  CC.makeCardComponent (CT.SetupGeoChart GcT.Marker) $ H.parentComponent
    { render
    , eval: cardEval ⨁ setupEval
    , receiver: const Nothing
    , initialState: const ST.initialState
    }

render ∷ ST.State → HTML
render state =
  HH.div
    [ HP.classes [ CSS.chartEditor ] ]
    [ HH.slot' CS.cpDims unit (DM.component package) unit
        $ HE.input \l → right ∘ Q.HandleDims l
    , HH.hr_
    , row [ renderMinSize state, renderMaxSize state ]
    , HH.hr_
    , row [ renderOsmURI state ]
    ]

renderMinSize ∷ ST.State → HTML
renderMinSize state =
  HH.div
    [ HP.classes [ B.colXs6, CSS.axisLabelParam ]
    ]
    [ HH.label
        [ HP.classes [ B.controlLabel ] ]
        [ HH.text "Minimum size" ]
    , HH.input
        [ HP.classes [ B.formControl ]
        , HP.value $ show $ state.minSize
        , ARIA.label "Min size"
        , HE.onValueChange $ HE.input (\s → right ∘ Q.SetMinSymbolSize s)
        ]
    ]

renderMaxSize ∷ ST.State → HTML
renderMaxSize state =
  HH.div
    [ HP.classes [ B.colXs6, CSS.axisLabelParam ]
    ]
    [ HH.label [ HP.classes [ B.controlLabel ] ] [ HH.text "Maximum size" ]
    , HH.input
        [ HP.classes [ B.formControl ]
        , HP.value $ show $ state.maxSize
        , ARIA.label "Max size"
        , HE.onValueChange$ HE.input (\s → right ∘ Q.SetMaxSymbolSize s)
        ]
    ]

renderOsmURI ∷ ST.State → HTML
renderOsmURI state =
  HH.div [ HP.classes [ CSS.axisLabelParam ] ]
    [ HH.label [ HP.classes [ CN.controlLabel ] ] [ HH.text "Open Street Map URI" ]
    , HH.input
        [ HP.classes [ CN.formControl ]
        , HP.value state.osmURIString
        , ARIA.label "Open Street Map URI"
        , HE.onValueInput $ HE.input \s → right ∘ Q.SetOsmURI s
        ]
    ]

cardEval ∷ CC.CardEvalQuery ~> DSL
cardEval = case _ of
  CC.Activate next →
    pure next
  CC.Deactivate next →
    pure next
  CC.Save k → do
    st ← H.get
    let
      inp = M.SetupGeoMarker $ Just
        { lat: D.topDimension
        , lng: D.topDimension
        , series: Nothing
        , size: Nothing
        , dims: [ ]
        , minSize: st.minSize
        , maxSize: st.maxSize
        , osmURI: st.osmURI
        }
    out ← H.query' CS.cpDims unit $ H.request $ DQ.Save inp
    pure $ k case join out of
      Nothing → M.SetupGeoMarker Nothing
      Just a → a
  CC.Load m next → do
    _ ← H.query' CS.cpDims unit $ H.action $ DQ.Load $ Just m
    for_ (m ^? M._SetupGeoMarker ∘ _Just) \r →
      H.modify _{ osmURI = r.osmURI
                , osmURIString = printURIRef r.osmURI
                }
    pure next
  CC.ReceiveInput _ _ next →
    pure next
  CC.ReceiveOutput _ _ next →
    pure next
  CC.ReceiveState evalState next → do
    for_ (evalState ^? ES._Axes) \axes →
      H.query' CS.cpDims unit $ H.action $ DQ.SetAxes axes
    pure next
  CC.ReceiveDimensions dims reply → do
    pure $ reply
      if dims.width < 576.0 ∨ dims.height < 416.0
      then Low
      else High


setupEval ∷ Q.Query ~> DSL
setupEval = case _ of
  Q.HandleDims q next → do
    case q of
      DQ.Update _ → H.raise CC.modelUpdate
    pure next
  Q.SetMinSymbolSize str next → do
    let fl = readFloat str
    unless (isNaN fl) do
      H.modify \st →
        st{ minSize = fl
          , maxSize = if st.maxSize > fl then st.maxSize else fl
          }
      H.raise CC.modelUpdate
    pure next
  Q.SetMaxSymbolSize str next → do
    let fl = readFloat str
    unless (isNaN fl) do
      H.modify \st →
        st{ maxSize = fl
          , minSize = if st.minSize < fl then st.minSize else fl
          }
      H.raise CC.modelUpdate
    pure next
  Q.SetOsmURI s next → do
    let
      oRx = URX.unsafeRegex "{" RXF.global
      cRx = URX.unsafeRegex "}" RXF.global
      replaced = RX.replace oRx "%7B" $ RX.replace cRx "%7D" s
    H.modify $  case runParseURIRef replaced of
      Left e → \st →
        st{ osmURIString = printURIRef st.osmURI
          }
      Right uri →
        _{ osmURI = onURIRef decodeURIComponent uri
         , osmURIString = s
         }
    H.raise CC.modelUpdate
    pure next
