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
module SlamData.Workspace.Card.Setups.DisplayOptions.Model.Format where

import SlamData.Prelude

import Data.Argonaut as J
import Data.Codec as C
import Data.Codec.Argonaut.Common as CA
import Data.Codec.Argonaut.Variant as CAV
import Data.Formatter.DateTime as FDT
import Data.Variant as V

data FormatOptions
  = Default
  | Currency NumericFormat
  | Numeric NumericFormat
  | Date FDT.Formatter
  | Time FDT.Formatter
  | DateTime FDT.Formatter
  | Text TextFormat

derive instance eqFormats ∷ Eq FormatOptions
derive instance ordFormats ∷ Ord FormatOptions
derive instance genericFormats ∷ Generic FormatOptions _

type NumericFormat =
  { prefix ∷ String
  , suffix ∷ String
  , thousands ∷ String
  , decimal ∷ String
  }

codecNumericFormat ∷ CA.JsonCodec NumericFormat
codecNumericFormat = CA.object "NumericFormat" (CA.record
  # CA.recordProp (SProxy ∷ SProxy "prefix") CA.string
  # CA.recordProp (SProxy ∷ SProxy "suffix") CA.string
  # CA.recordProp (SProxy ∷ SProxy "thousands") CA.string
  # CA.recordProp (SProxy ∷ SProxy "decimal") CA.string)

type TextFormat =
  { maxLength ∷ Maybe Int
  , truncated ∷ String
  }

codecTextFormat ∷ CA.JsonCodec TextFormat
codecTextFormat = CA.object "TextFormat" (CA.record
  # CA.recordProp (SProxy ∷ SProxy "maxLength") (CA.maybe CA.int)
  # CA.recordProp (SProxy ∷ SProxy "truncated") CA.string)

codecDateTimeFormat ∷ CA.JsonCodec FDT.Formatter
codecDateTimeFormat = C.mapCodec parse FDT.printFormatter CA.string
  where
    parse v =
      lmap
        (const (CA.UnexpectedValue (J.fromString v)))
        (FDT.parseFormatString v)

defaultFormat ∷ FormatOptions
defaultFormat = Default

codecFormatOptions ∷ CA.JsonCodec FormatOptions
codecFormatOptions =
  dimap toVariant fromVariant $ (CAV.variant
    # CAV.variantCase _Default (Left unit)
    # CAV.variantCase _Currency (Right codecNumericFormat)
    # CAV.variantCase _Numeric (Right codecNumericFormat)
    # CAV.variantCase _Date (Right codecDateTimeFormat)
    # CAV.variantCase _Time (Right codecDateTimeFormat)
    # CAV.variantCase _DateTime (Right codecDateTimeFormat)
    # CAV.variantCase _Text (Right codecTextFormat))
  where
    toVariant = case _ of
      Default → V.inj _Default unit
      Currency f → V.inj _Currency f
      Numeric f → V.inj _Numeric f
      Date f → V.inj _Date f
      Time f → V.inj _Time f
      DateTime f → V.inj _DateTime f
      Text f → V.inj _Text f
    fromVariant = V.case_
      # V.on _Default (const Default)
      # V.on _Currency Currency
      # V.on _Numeric Numeric
      # V.on _Date Date
      # V.on _Time Time
      # V.on _DateTime DateTime
      # V.on _Text Text
    _Default = SProxy ∷ SProxy "Default"
    _Currency = SProxy ∷ SProxy "Currency"
    _Numeric = SProxy ∷ SProxy "Numeric"
    _Date = SProxy ∷ SProxy "Date"
    _Time = SProxy ∷ SProxy "Time"
    _DateTime = SProxy ∷ SProxy "DateTime"
    _Text = SProxy ∷ SProxy "Text"
