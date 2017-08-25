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

module SlamData.Workspace.Card.Viz.Renderer.Geo.Component where

import SlamData.Prelude

import Data.Lens (_Just, (?~))
import Data.Path.Pathy ((</>), (<.>), file, rootDir, dir)
import Data.URI (URIRef)
import Data.URI as URI

import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE

import Leaflet.Halogen as HL
import Leaflet.Core as LC

import SlamData.Monad (Slam)
import SlamData.Workspace.Card.Eval.State as ES
import SlamData.Workspace.Card.Viz.Renderer.Geo.Model as M
import SlamData.Workspace.Card.Port as Port

type ChildSlot = Unit
type ChildQuery = HL.Query

data Query a
  = HandleMessage HL.Message a
  | Save (M.Model → a)
  | Load M.Model a
  | Setup Port.GeoChartPort a
  | Update ES.GeoR a
  | SetDimensions { width ∷ Int, height ∷ Int } a

type Message = Maybe ES.EvalState → Maybe ES.EvalState

type State =
  { osmURI ∷ URIRef
  , zoom ∷ Int
  , view ∷ { lat ∷ Number, lng ∷ Number }
  , layers ∷ Array LC.Layer
  , controls ∷ Array LC.Control
  , tileLayer ∷  Maybe LC.Layer
  }

initialState ∷ State
initialState =
  { osmURI:
      Left $ URI.URI
      (Just $ URI.URIScheme "http")
      (URI.HierarchicalPart
       (Just $ URI.Authority Nothing [(URI.NameAddress "{s}.tile.osm.org") × Nothing])
       (Just $ Right $ rootDir </> dir "{z}" </> dir "{x}" </> file "{y}" <.> "png"))
      Nothing
      Nothing
  , zoom: zero
  , view: { lat: zero, lng: zero }
  , layers: [ ]
  , controls: [ ]
  , tileLayer: Nothing
  }


type DSL = H.ParentDSL State Query ChildQuery ChildSlot Message Slam
type HTML = H.ParentHTML Query ChildQuery ChildSlot Slam

component ∷ H.Component HH.HTML Query Unit Message Slam
component = H.parentComponent
    { render
    , eval
    , receiver: const Nothing
    , initialState: const initialState
    }

render ∷ State → HTML
render state =
  HH.div_
    [ HH.slot unit HL.leaflet unit $ HE.input $ HandleMessage ]

eval ∷ Query ~> DSL
eval = case _ of
  SetDimensions {width, height} next → do
    _ ←
      H.query unit $ H.action
        $ HL.SetDimension
        { height: Just height
        , width: Just width
        }
    pure next
  HandleMessage (HL.Initialized leaf) next → do
    sync
    H.raise $ ( _Just ∘ ES._Leaflet ?~ leaf )
    pure next
  Save cont → do
    st ← H.get
    pure $ cont $ { osmURI: st.osmURI, view: st.view, zoom: st.zoom }
  Load r next → do
    H.modify _{ zoom = r.zoom
              , view = r.view
              , osmURI = r.osmURI
              }
    sync
    pure next
  Setup p next → do
    H.modify _{ osmURI = p.osmURI }
    sync
    pure next
  Update {layers, controls} next → do
    st ← H.get
    _ ← H.query unit $ H.action $ HL.RemoveLayers st.layers
    _ ← H.query unit $ H.action $ HL.AddLayers layers
    H.liftEff $ for_ st.controls LC.remove
    H.modify _{ controls = controls
              , layers = layers
              }
    pure next

sync ∷ DSL Unit
sync = do
  st ← H.get
  for_ st.tileLayer \l → H.query unit $ H.action $ HL.RemoveLayers [ l ]
  zoom ← H.liftAff $ LC.mkZoom st.zoom
  _ ← H.query unit $ H.action $ HL.SetZoom zoom
  latLng ← H.liftAff $ LC.mkLatLng st.view.lat st.view.lng
  _ ← H.query unit $ H.action $ HL.SetView latLng
  tiles ← LC.tileLayer st.osmURI
  H.modify _{ tileLayer = Just $ LC.tileToLayer tiles }
  _ ← H.query unit $ H.action $ HL.AddLayers [ LC.tileToLayer tiles ]
  pure unit
