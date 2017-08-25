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

module SlamData.Workspace.Card.Setups.Auxiliary.Render where

import SlamData.Prelude

import Data.Array as Arr
import Data.Functor.Variant (FProxy, VariantF, inj)
import Data.Int as Int
import Data.Lens (Lens', (^.))
import Data.Lens.Record (prop)

import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.HTML.Properties.ARIA as ARIA
import Halogen.Themes.Bootstrap3 as B

import SlamData.Form.Select (class OptionVal, Select(..), stringVal)
import SlamData.Render.ClassName as CN
import SlamData.Render.Common (row)
import SlamData.Workspace.Card.Setups.Auxiliary.Algebra as Alg
import SlamData.Workspace.Card.Setups.Auxiliary.Proxy (class HasLabel, _string, _min, _max, label)
import SlamData.Workspace.Card.Setups.Auxiliary.Piece as P
import SlamData.Workspace.Card.Setups.CSS as CSS

printNum ∷ Number → String
printNum n = case Int.fromNumber n of
  Nothing → show n
  Just i → show i

choose
  ∷ ∀ r1 r2 s q1 q2 a
  . OptionVal a
  ⇒ RowCons s a r1 r2
  ⇒ RowCons s (FProxy (Alg.ChooseF a)) q1 q2
  ⇒ HasLabel s
  ⇒ SProxy s
  → Select a
  → Record r1
  → H.ComponentHTML (VariantF q2)
choose proxy (Select { options, value }) state =
  let
    len = Arr.length options
    renderedOptions = map renderOption options
  in
    HH.select
      [ HP.classes [ CN.formControl ]
      , HP.disabled $ len < 1
      , HE.onSelectedIndexChange \ix →
          Arr.index options ix <#> \val → inj proxy $ H.action $ Alg.Choose val
      ]
      $ map renderOption options
  where
  renderOption option =
    HH.option
      [ HP.selected $ Just option ≡ value ]
      [ HH.text $ stringVal option ]

osmURI
  ∷ ∀ r1 r2 s q1 q2
  . RowCons s P.OsmURI r1 r2
  ⇒ RowCons s (FProxy Alg.SetF) q1 q2
  ⇒ IsSymbol s
  ⇒ SProxy s
  → Record r2
  → H.ComponentHTML (VariantF q2)
osmURI proxy state =
  HH.div [ HP.classes [ CSS.axisLabelParam ] ]
    [ HH.label [ HP.classes [ CN.controlLabel ] ] [ HH.text "Open Street Map URI" ]
    , HH.input
        [ HP.classes [ CN.formControl ]
        , HP.value $ state ^. lens
        , ARIA.label "Open Street Map URI"
        , HE.onValueInput \s → Just $ inj proxy $ H.action $ Alg.Set s
        ]
    ]
  where
  lens ∷ Lens' (Record r2) String
  lens = prop proxy ∘ prop _string


minMax
  ∷ ∀ r1 r2 s q1 q2
  . RowCons s P.MinMax r1 r2
  ⇒ RowCons s (FProxy Alg.MinMaxF) q1 q2
  ⇒ HasLabel s
  ⇒ SProxy s
  → Record r2
  → H.ComponentHTML (VariantF q2)
minMax proxy state =
  row
  [ HH.div
    [ HP.classes [ B.colXs6, CSS.axisLabelParam ]
    ]
    [ HH.label
        [ HP.classes [ B.controlLabel ] ]
        [ HH.text $ "Minimum " <> label proxy ]
    , HH.input
        [ HP.classes [ B.formControl ]
        , HP.value $ printNum $ state ^. _minSize
        , ARIA.label $  "Min " <> label proxy
        , HE.onValueInput \s → Just $ inj proxy $ H.action $ Alg.Min s
        ]
    ]
  , HH.div
    [ HP.classes [ B.colXs6, CSS.axisLabelParam ]
    ]
    [ HH.label [ HP.classes [ B.controlLabel ] ] [ HH.text $ "Maximum " <> label proxy ]
    , HH.input
        [ HP.classes [ B.formControl ]
        , HP.value $ printNum $ state ^. _maxSize
        , ARIA.label $ "Max " <> label proxy
        , HE.onValueInput \s → Just $ inj (SProxy ∷ SProxy s) $ H.action $ Alg.Max s
        ]
    ]
  ]
  where
  _minSize ∷ Lens' (Record r2) Number
  _minSize = prop proxy ∘ prop _min

  _maxSize ∷ Lens' (Record r2) Number
  _maxSize = prop proxy ∘ prop _max

toggle
  ∷ ∀ r1 r2 s q1 q2
  . RowCons s Boolean r1 r2
  ⇒ RowCons s (FProxy Alg.ToggleF) q1 q2
  ⇒ HasLabel s
  ⇒ SProxy s
  → Record r2
  → H.ComponentHTML (VariantF q2)
toggle proxy state =
  HH.div
    [ HP.classes [ CSS.axisLabelParam ]
    ]
    [ HH.label [ HP.classes [ CN.controlLabel ] ]
        [ HH.input
            [ HP.type_ HP.InputCheckbox
            , HP.checked $ state ^. lens
            , ARIA.label $ label proxy
            , HE.onChecked \_ → Just $ inj proxy $ H.action $ Alg.Toggle
            ]
        , HH.text $ label proxy
        ]
    ]
  where
  lens ∷ Lens' (Record r2) Boolean
  lens = prop proxy

number
  ∷ ∀ r1 r2 s q1 q2
  . RowCons s Number r1 r2
  ⇒ RowCons s (FProxy Alg.SetF) q1 q2
  ⇒ HasLabel s
  ⇒ SProxy s
  → Record r2
  → H.ComponentHTML (VariantF q2)
number proxy state =
  HH.div
    [ HP.classes [ CSS.axisLabelParam ]
    ]
    [ HH.label
        [ HP.classes [ CN.controlLabel ] ]
        [ HH.text $ label proxy ]
    , HH.input
        [ HP.classes [ CN.formControl ]
        , HP.value $ printNum $ state ^. lens
        , ARIA.label $ label proxy
        , HE.onValueInput \s → Just $ inj proxy $ H.action $ Alg.Set s
        ]
    ]
  where
  lens ∷ Lens' (Record r2) Number
  lens = prop proxy


string
  ∷ ∀ r1 r2 s q1 q2
  . RowCons s String r1 r2
  ⇒ RowCons s (FProxy Alg.SetF) q1 q2
  ⇒ HasLabel s
  ⇒ SProxy s
  → Record r2
  → H.ComponentHTML (VariantF q2)
string proxy state =
  HH.div
    [ HP.classes [ CSS.axisLabelParam ]
    ]
    [ HH.label
        [ HP.classes [ CN.controlLabel ] ]
        [ HH.text $ label proxy ]
    , HH.input
        [ HP.classes [ CN.formControl ]
        , HP.value $ state ^. lens
        , ARIA.label $ label proxy
        , HE.onValueInput \s → Just $ inj proxy $ H.action $ Alg.Set s
        ]
    ]
  where
  lens ∷ Lens' (Record r2) String
  lens = prop proxy
