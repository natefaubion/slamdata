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

module SlamData.Workspace.Card.CardType
  ( CardType
  , name
  , cardClasses
  , icon
  , encode
  , decode
  , consumerInteractable
  , all
  , contractToPivot
  , contractToMetric
  , contractToMetricRenderer
  , module SlamData.Workspace.Card.CardType.Ace
  , module SlamData.Workspace.Card.CardType.Chart
  , module SlamData.Workspace.Card.CardType.Geo
  , module SlamData.Workspace.Card.CardType.Input
  , module SlamData.Workspace.Card.CardType.Select
  , module SlamData.Workspace.Card.CardType.Simple
  , module SlamData.Workspace.Card.CardType.Static
  ) where

import SlamData.Prelude

import Data.Argonaut as J
import Data.String as Str
import Halogen.HTML as H
import SlamData.Render.Icon as I
import SlamData.Workspace.Card.CardType.Ace ( _aceMarkdown, aceMarkdown, _aceSql, aceSql, mode, AceR, Ace, contractToAce)
import SlamData.Workspace.Card.CardType.Ace as Ace
import SlamData.Workspace.Card.CardType.Chart ( _pie, pie, _line, line, _bar, bar, _area, area, _scatter, scatter, _radar, radar, _funnel, funnel, _graph, graph, _heatmap, heatmap, _sankey, sankey, _gauge, gauge, _boxplot, boxplot, _metric, metric, _pivot, pivot, _punchCard, punchCard, _candlestick, candlestick, _parallel, parallel, ChartR, Chart, contractToChart)
import SlamData.Workspace.Card.CardType.Chart as Cht
import SlamData.Workspace.Card.CardType.Geo ( _geoMarker, geoMarker, _geoHeatmap, geoHeatmap, GeoR, Geo, contractToGeo)
import SlamData.Workspace.Card.CardType.Geo as Geo
import SlamData.Workspace.Card.CardType.Input ( _text, text, _numeric, numeric, _date, date, _time, time, _datetime, datetime, InputR, Input, contractToInput)
import SlamData.Workspace.Card.CardType.Input as Inp
import SlamData.Workspace.Card.CardType.Select ( _dropdown, dropdown, _radio, radio, _checkbox, checkbox, SelectR, Select, contractToSelect)
import SlamData.Workspace.Card.CardType.Select as Sel
import SlamData.Workspace.Card.CardType.Simple ( _search, search, _markdown, markdown, _table, table, _download, download, _variables, variables, _troubleshoot, troubleshoot, _open, open, _downloadOptions, downloadOptions, _tabs, tabs, _structureEditor, structureEditor, _cache, cache, _draftboard, draftboard, _viz, viz, SimpleR, Simple, contractToSimple, setupViz, _setupViz)
import SlamData.Workspace.Card.CardType.Simple as Sim
import SlamData.Workspace.Card.CardType.Static ( _static, static, StaticR, Static, contractToStatic)
import SlamData.Workspace.Card.CardType.Static as Sta

type CardType =
  Variant ( SimpleR ( AceR () ) )

all ∷ Array CardType
all = Sim.all ⊕ Ace.all

print ∷ CardType → String
print = case_
  # Ace.print
  # Sim.print

encode ∷ CardType → J.Json
encode r =
  J.encodeJson
  $ ( case_
      # Sim.print
      # Ace.print
      $ r )

parse ∷ String → String ⊹ CardType
parse s =
  Sim.parse s
  <|> parseChart s
  <|> parseSelect s
  <|> parseInput s
  <|> parseStatic s
  <|> parseGeo s
  <|> Ace.parse s
  <|> (Left $  "unknow card type '" ⊕ s ⊕ "'")
  where
  parseChart n =
    bimap
      (const $ "unknown card type '" ⊕ n ⊕ "'")
      (const setupViz)
      $ Cht.parse
      $ fromMaybe ""
      $ Str.stripSuffix (Str.Pattern "-options") n

  parseSelect n =
    bimap
      (const $ "unknown card type '" ⊕ n ⊕ "'")
      (const setupViz)
      $ Sel.parse
      $ fromMaybe ""
      $ Str.stripSuffix (Str.Pattern "-setup") n

  parseInput n =
    bimap
      (const $ "unknown card type '" ⊕ n ⊕ "'")
      (const setupViz)
      $ Inp.parse
      $ fromMaybe ""
      $ Str.stripSuffix (Str.Pattern "-setup") n

  parseStatic n =
    bimap
      (const $ "unknown card type '" ⊕ n ⊕ "'")
      (const setupViz)
      $ Sta.parse
      $ fromMaybe ""
      $ Str.stripSuffix (Str.Pattern "-setup") n

  parseGeo n =
    bimap
      (const $ "unknown card type '" ⊕ n ⊕ "'")
      (const setupViz)
      $ Geo.parse
      $ fromMaybe ""
      $ Str.stripSuffix (Str.Pattern "-setup") n

decode ∷ J.Json → String ⊹ CardType
decode = J.decodeJson >=> parse

name ∷ CardType → String
name = case_ # Sim.name # Ace.name

icon ∷ CardType → I.IconHTML
icon = case_ # Sim.icon # Ace.icon

consumerInteractable ∷ CardType → Boolean
consumerInteractable = case_
  # Sim.consumerInteractable
  # Ace.consumerInteractable

cardClasses ∷ CardType → Array H.ClassName
cardClasses = case_
  # Sim.cardClasses
  # Ace.cardClasses

contractToPivot
  ∷ ∀ r a
  . Contractable r (pivot ∷ a)
  ⇒ Variant r
  → Maybe (Variant (pivot ∷ a))
contractToPivot = contract

contractToMetric
  ∷ ∀ r a
  . Contractable r (metric ∷ a)
  ⇒ Variant r
  → Maybe (Variant (metric ∷ a))
contractToMetric = contract

contractToMetricRenderer
  ∷ ∀ r a b
  . Contractable r (metric ∷ a, static ∷ b)
  ⇒ Variant r
  → Maybe (Variant (metric ∷ a, static ∷ b))
contractToMetricRenderer = contract
