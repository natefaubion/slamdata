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

module SlamData.Workspace.Card.Setups.Auxiliary.Eval where

import SlamData.Prelude

import Data.Lens (Lens', (.~), (^.), (%~))
import Data.Lens.Record (prop)
import Data.String.Regex as RX
import Data.String.Regex.Flags as RXF
import Data.String.Regex.Unsafe as URX
import Data.URI (URIRef, runParseURIRef, printURIRef)

import Global (decodeURIComponent, readFloat, isNaN)

import Halogen as H

import SlamData.Form.Select (class OptionVal)
import SlamData.Workspace.Card.Viz.Renderer.Geo.Model (onURIRef)
import SlamData.Workspace.Card.Setups.Auxiliary.Algebra as Alg
import SlamData.Workspace.Card.Setups.Auxiliary.Proxy (_uri, _string, _min, _max)
import SlamData.Workspace.Card.Setups.Auxiliary.Piece as P

osmURI
  ∷ ∀ r1 r2 s q m
  . RowCons s P.OsmURI r1 r2
  ⇒ IsSymbol s
  ⇒ SProxy s
  → Alg.SetF
  ~> H.ComponentDSL (Record r2) q (Record r2) m
osmURI proxy = case _ of
  Alg.Set s next → do
    let
      oRx = URX.unsafeRegex "{" RXF.global
      cRx = URX.unsafeRegex "}" RXF.global
      replaced = RX.replace oRx "%7B" $ RX.replace cRx "%7D" s
    H.modify $  case runParseURIRef replaced of
      Left e → \st →
        st # _osmURIString .~ printURIRef (st ^. _osmURI)
      Right uri →
        ( _osmURI .~ onURIRef decodeURIComponent uri )
        ∘ ( _osmURIString .~ s )
    st ← H.get
    H.raise st
    pure next
  where
  _osmURIString ∷ Lens' (Record r2) String
  _osmURIString = prop proxy ∘ prop _string

  _osmURI ∷ Lens' (Record r2) URIRef
  _osmURI = prop proxy ∘ prop _uri

choose
  ∷ ∀ r1 r2 s q m a
  . OptionVal a
  ⇒ RowCons s a r1 r2
  ⇒ IsSymbol s
  ⇒ SProxy s
  → Alg.ChooseF a
  ~> H.ComponentDSL (Record r2) q (Record r2) m
choose proxy = case _ of
  Alg.Choose a next → do
    H.modify $ prop proxy .~ a
    st ← H.get
    H.raise st
    pure next

minMax
  ∷ ∀ r1 r2 s m q
  . RowCons s P.MinMax r1 r2
  ⇒ IsSymbol s
  ⇒ SProxy s
  → Alg.MinMaxF
  ~> H.ComponentDSL (Record r2) q (Record r2) m
minMax proxy = case _ of
  Alg.Min str next → do
    let fl = readFloat str
    unless (isNaN fl) do
      H.modify \st → st
        # ( _minSize .~ fl )
        ∘ ( _maxSize .~ if (st ^. _maxSize) > fl then st ^. _maxSize else fl )
      st ← H.get
      H.raise st
    pure next
  Alg.Max str next → do
    let fl = readFloat str
    unless (isNaN fl) do
      H.modify \st → st
        # ( _maxSize .~ fl )
        ∘ ( _minSize .~ if (st ^. _minSize) < fl then st ^. _minSize else fl )
      st ← H.get
      H.raise st
    pure next
  where
  _minSize ∷ Lens' (Record r2) Number
  _minSize = prop (SProxy ∷ SProxy s) ∘ prop _min

  _maxSize ∷ Lens' (Record r2) Number
  _maxSize = prop (SProxy ∷ SProxy s) ∘ prop _max

toggle
  ∷ ∀ r1 r2 s m q
  . RowCons s Boolean r1 r2
  ⇒ IsSymbol s
  ⇒ SProxy s
  → Alg.ToggleF
  ~> H.ComponentDSL (Record r2) q (Record r2) m
toggle _ = case _ of
  Alg.Toggle next → do
    H.modify $ _toggle %~ not
    st ← H.get
    H.raise st
    pure next
  where
  _toggle ∷ Lens' (Record r2) Boolean
  _toggle = prop (SProxy ∷ SProxy s)

number
  ∷ ∀ r1 r2 s q m
  . RowCons s Number r1 r2
  ⇒ IsSymbol s
  ⇒ SProxy s
  → Alg.SetF
  ~> H.ComponentDSL (Record r2) q (Record r2) m
number proxy = case _ of
  Alg.Set str next → do
    let fl = readFloat str
    unless (isNaN fl)
      $ H.modify $ prop proxy .~ fl
    st ← H.get
    H.raise st
    pure next

string
  ∷ ∀ r1 r2 s q m
  . RowCons s String r1 r2
  ⇒ IsSymbol s
  ⇒ SProxy s
  → Alg.SetF
  ~> H.ComponentDSL (Record r2) q (Record r2) m
string proxy = case _ of
  Alg.Set str next → do
    H.modify $ prop proxy .~ str
    st ← H.get
    H.raise st
    pure next

reset
  ∷ ∀ s m q i
  . Tuple s
  ~> H.ComponentDSL s q i m
reset = case _ of
  Tuple st next → do
    H.put st
    pure next
