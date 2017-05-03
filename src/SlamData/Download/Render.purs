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

module SlamData.Download.Render where

import SlamData.Prelude

import Data.Lens (Lens', (^.))

import Halogen as H
import Halogen.HTML.Events as HE
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Halogen.Themes.Bootstrap3 as B

import SlamData.Download.Model as D
import SlamData.Render.CSS as Rc

optionsCSV
  :: forall f
   . (Lens' D.CSVOptions String -> String -> (Unit -> f Unit))
   -> D.CSVOptions
   -> H.ComponentHTML f
optionsCSV func opts =
  HH.div_
    [ HH.ul
      [ HP.classes [ Rc.downloadCSVDelimiters, B.clearfix ]]
      [ field D._rowDelimiter "Row delimiter"
      , field D._colDelimiter "Column delimiter"
      , field D._quoteChar "Quote character"
      , field D._escapeChar "Quote escape"
      ]
    ]
  where
  field :: Lens' D.CSVOptions String -> String -> H.ComponentHTML f
  field lens label =
    HH.li_
      [ HH.label_
          [ HH.span_ [ HH.text label ]
          , HH.input
              [ HP.classes [ B.formControl ]
              , HP.value (opts ^. lens)
              , HE.onValueInput $ HE.input (func lens)
              ]
          ]
      ]

optionsJSON
  :: forall f
   . (forall a. (Eq a) => Lens' D.JSONOptions a -> a -> (Unit -> f Unit))
  -> D.JSONOptions
  -> H.ComponentHTML f
optionsJSON func opts =
  HH.div
    [ HP.classes [ Rc.downloadJSONOptions ] ]
    [ multivalues, precision ]
  where
  multivalues :: H.ComponentHTML f
  multivalues =
    HH.div
      [ HP.classes [ B.clearfix ] ]
      [ HH.label_ [ HH.text "Multiple values" ]
      , HH.ul_
          [ radio "multivalues" D._multivalues D.ArrayWrapped "Wrap values in arrays"
          , radio "multivalues" D._multivalues D.LineDelimited "Separate values by newlines"
          ]
      ]

  precision :: H.ComponentHTML f
  precision =
    HH.div
      [ HP.classes [ B.clearfix ] ]
      [ HH.label_ [ HH.text "Precision" ]
      , HH.ul_
          [ radio "precision" D._precision D.Readable "Readable"
          , radio "precision" D._precision D.Precise "Encode all types"
          ]
      ]

  radio
    :: forall a
     . (Eq a)
    => String -> Lens' D.JSONOptions a -> a -> String -> H.ComponentHTML f
  radio grp lens value label =
    HH.li_
      [ HH.label_
          [ HH.input
              [ HP.type_ HP.InputRadio
              , HP.name grp
              , HP.checked (opts ^. lens == value)
              , HE.onValueChange (HE.input_ (func lens value))
              ]
          , HH.text label
          ]
      ]

fldName
  ∷ ∀ q
  . Boolean
  → Either D.CSVOptions D.JSONOptions
  → String
  → (String → H.Action q)
  → H.ComponentHTML q
fldName compressed options tgtValue query =
  HH.div
    [ HP.classes [ B.formGroup, Rc.downloadTarget ] ]
    [ HH.label_
        [ HH.span_ [ HH.text "Target name" ]
        , HH.input
            [ HP.classes [ B.formControl ]
            , HP.value tgtValue
            , HE.onValueInput (HE.input query)
            ]
        , HH.div
            [ HP.classes [ Rc.downloadTargetBox ] ]
            [ HH.span_ [ HH.text tgtValue ]
            , HH.span_ [ HH.text (D.extension compressed options) ]
            ]
        ]
    ]
