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

module SlamData.Workspace.Card.Setups.Common.Tooltip where

import SlamData.Prelude

import Color as Color
import CSS as CSS
import Data.Foreign (F, Foreign, readNumber, typeOf, toForeign) as Frn
import Data.Foreign.Index (readIndex, readProp) as Frn
import ECharts.Commands as E
import ECharts.Monad (DSLMonad)
import ECharts.Types as ET
import ECharts.Types.Phantom as ETP
import Halogen.HTML as HH
import Halogen.HTML.CSS as HC
import Halogen.HTML.Properties as HP
import Halogen.VDom.DOM.StringRenderer as VDS
import SlamData.Workspace.Card.Setups.Common.Eval as BCE
import Unsafe.Coerce (unsafeCoerce)
import Utils (showFormattedNumber, hush')

type ColumnFormatter a =
  { label ∷ String
  , value ∷ a → String
  }

type Color = String

tooltip ∷ ∀ i. DSLMonad ETP.TooltipI Unit  → DSLMonad (tooltip ∷ ETP.I|i) Unit
tooltip a = E.tooltip do
  E.textStyle do
    E.fontFamily "Ubuntu, sans-serif, sans"
    E.fontSize 12
  a


tableFormatter ∷ ∀ a. (a → Maybe String) → Array (ColumnFormatter a) → Array a → String
tableFormatter getColor cols as = VDS.render absurd (unwrap table)
  where
  table =
    HH.table
      [ HP.class_ $ HH.ClassName "sd-chart-tooltip-table" ]
      ([ headers ] <> map row as)

  headers = HH.tr_ ([ header "" ] <> map (header ∘ _.label) cols)
  header h = HH.th_ [ HH.text h ]
  row r = HH.tr_ ([ color (getColor r) ] <> map (cell r) cols)
  cell r f = HH.td_ [ HH.text (f.value r) ]

  color = case _ of
    Nothing → HH.td_ [ HH.text "" ]
    Just c →
      HH.td_
        [ HH.span
            [ HP.class_ $ HH.ClassName "sd-chart-tooltip-color"
            , HC.style $ for_ (Color.fromHexString c) CSS.backgroundColor
            ]
            []
        ]

tableRows ∷ Array (String × String) → String
tableRows rows = VDS.render absurd (unwrap table)
  where
  table =
    HH.table
      [ HP.class_ $ HH.ClassName "sd-chart-tooltip-table" ]
      (renderRow <$> rows)

  renderRow (header × value) =
    HH.tr_
      [ HH.th_ [ HH.text header ]
      , HH.td_ [ HH.text value ]
      ]

type FormatterInput r = { value ∷ Frn.Foreign, data ∷ ET.Item, seriesIndex ∷ Int | r }

valueIx ∷ ∀ a r. (Frn.Foreign → Frn.F a) → Int → FormatterInput r → Maybe a
valueIx read ix inp = hush' (read =<< Frn.readIndex ix inp.value)

foreignName ∷ ∀ r a. { name ∷ a | r} → { name ∷ Frn.Foreign | r}
foreignName r = r{ name = Frn.toForeign r.name }

nameIx ∷ ∀ a r. (Frn.Foreign → Frn.F a) → Int → FormatterInput r → Maybe a
nameIx read ix inp = hush' do
  names ← Frn.readProp "name" $ unItem inp.data
  res ← Frn.readIndex ix names
  read res

unItem ∷ ET.Item → Frn.Foreign
unItem (ET.Item f) = f

symbolSize ∷ ∀ a r. (Frn.Foreign → Frn.F a) → FormatterInput r → Maybe a
symbolSize read inp = hush' do
  read =<< Frn.readProp "symbolSize" (unItem inp.data)


dataProp ∷ ∀ a r. (Frn.Foreign → Frn.F a) → String → FormatterInput r → Maybe a
dataProp read prop { data: ET.Item inp } = hush' (read =<< Frn.readProp prop inp)

assocProp ∷ ∀ a r. (Frn.Foreign → Frn.F a) → String → FormatterInput r → Maybe a
assocProp read prop { data: inp } = do
  assoc ← BCE.deref inp
  hush' $ read =<< Frn.readProp prop assoc

formatValueIx ∷ ∀ r. Int → FormatterInput r → String
formatValueIx ix = maybe "" formatForeign ∘ valueIx pure ix

formatNumberValueIx ∷ ∀ r. Int → FormatterInput r → String
formatNumberValueIx ix = showFormattedNumber ∘ fromMaybe zero ∘ valueIx Frn.readNumber ix

formatDataProp ∷ ∀ r. String → FormatterInput r → String
formatDataProp p = maybe "" formatForeign ∘ dataProp pure p

formatAssocProp ∷ ∀ r. String → FormatterInput r → String
formatAssocProp p = maybe "" formatForeign ∘ assocProp pure p

formatNumber ∷ ∀ r. FormatterInput r → String
formatNumber = showFormattedNumber ∘ fromMaybe zero ∘ hush' ∘ Frn.readNumber ∘ _.value

formatNameIx ∷ ∀ r. Int → FormatterInput r → String
formatNameIx ix = maybe "" formatForeign ∘ nameIx pure ix

formatSymbolSize ∷ ∀ r. FormatterInput r → String
formatSymbolSize = maybe "" formatForeign ∘ symbolSize pure

formatForeign ∷ Frn.Foreign → String
formatForeign val = case Frn.typeOf val of
  "string"  → (unsafeCoerce ∷ Frn.Foreign → String) val
  "boolean" → show $ (unsafeCoerce ∷ Frn.Foreign → Boolean) val
  "number"  → showFormattedNumber $ (unsafeCoerce ∷ Frn.Foreign → Number) val
  _         → ""
