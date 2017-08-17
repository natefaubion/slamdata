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
import Data.Lens as Lens
import Data.List.NonEmpty as NEL
import Data.List.Safe ((:))
import Data.List.Safe as SL
import Data.Variant as V
import SlamData.Workspace.Card.Setups.DisplayOptions.TextFormat.Model as TextFormat
import SlamData.Workspace.Card.Setups.DisplayOptions.BooleanFormat.Model as BooleanFormat
import SlamData.Workspace.Card.Setups.DisplayOptions.IntegerFormat.Model as IntegerFormat
import SlamData.Workspace.Card.Setups.DisplayOptions.DecimalFormat.Model as DecimalFormat

data FormatOptions
  = DefaultFormat
  | CurrencyFormat DecimalFormat.DecimalFormat
  | DecimalFormat DecimalFormat.DecimalFormat
  | IntegerFormat IntegerFormat.IntegerFormat
  | DateFormat FDT.Formatter
  | TimeFormat FDT.Formatter
  | DateTimeFormat FDT.Formatter
  | TextFormat TextFormat.TextFormat
  | BooleanFormat BooleanFormat.BooleanFormat

derive instance eqFormatOptions ∷ Eq FormatOptions
derive instance ordFormatOptions ∷ Ord FormatOptions
derive instance genericFormatOptions ∷ Generic FormatOptions _

_DefaultFormat ∷ Lens.Prism' FormatOptions Unit
_DefaultFormat = Lens.prism' (const DefaultFormat) case _ of
  DefaultFormat → Just unit
  _ → Nothing

_CurrencyFormat ∷ Lens.Prism' FormatOptions DecimalFormat.DecimalFormat
_CurrencyFormat = Lens.prism' CurrencyFormat case _ of
  CurrencyFormat a → Just a
  _ → Nothing

_DecimalFormat ∷ Lens.Prism' FormatOptions DecimalFormat.DecimalFormat
_DecimalFormat = Lens.prism' DecimalFormat case _ of
  DecimalFormat a → Just a
  _ → Nothing

_IntegerFormat ∷ Lens.Prism' FormatOptions IntegerFormat.IntegerFormat
_IntegerFormat = Lens.prism' IntegerFormat case _ of
  IntegerFormat a → Just a
  _ → Nothing

_DateFormat ∷ Lens.Prism' FormatOptions FDT.Formatter
_DateFormat = Lens.prism' DateFormat case _ of
  DateFormat a → Just a
  _ → Nothing

_TimeFormat ∷ Lens.Prism' FormatOptions FDT.Formatter
_TimeFormat = Lens.prism' TimeFormat case _ of
  TimeFormat a → Just a
  _ → Nothing

_DateTimeFormat ∷ Lens.Prism' FormatOptions FDT.Formatter
_DateTimeFormat = Lens.prism' DateTimeFormat case _ of
  DateTimeFormat a → Just a
  _ → Nothing

_TextFormat ∷ Lens.Prism' FormatOptions TextFormat.TextFormat
_TextFormat = Lens.prism' TextFormat case _ of
  TextFormat a → Just a
  _ → Nothing

_BooleanFormat ∷ Lens.Prism' FormatOptions BooleanFormat.BooleanFormat
_BooleanFormat = Lens.prism' BooleanFormat case _ of
  BooleanFormat a → Just a
  _ → Nothing

codecDateTimeFormat ∷ CA.JsonCodec FDT.Formatter
codecDateTimeFormat = C.mapCodec parse FDT.printFormatter CA.string
  where
    parse v =
      lmap
        (const (CA.UnexpectedValue (J.fromString v)))
        (FDT.parseFormatString v)

codecFormatOptions ∷ CA.JsonCodec FormatOptions
codecFormatOptions =
  dimap toVariant fromVariant $ (CAV.variant
    # CAV.variantCase _Default (Left unit)
    # CAV.variantCase _Currency (Right DecimalFormat.codec)
    # CAV.variantCase _Decimal (Right DecimalFormat.codec)
    # CAV.variantCase _Integer (Right IntegerFormat.codec)
    # CAV.variantCase _Date (Right codecDateTimeFormat)
    # CAV.variantCase _Time (Right codecDateTimeFormat)
    # CAV.variantCase _DateTime (Right codecDateTimeFormat)
    # CAV.variantCase _Text (Right TextFormat.codec)
    # CAV.variantCase _Boolean (Right BooleanFormat.codec))
  where
    toVariant = case _ of
      DefaultFormat → V.inj _Default unit
      CurrencyFormat f → V.inj _Currency f
      DecimalFormat f → V.inj _Decimal f
      IntegerFormat f → V.inj _Integer f
      DateFormat f → V.inj _Date f
      TimeFormat f → V.inj _Time f
      DateTimeFormat f → V.inj _DateTime f
      TextFormat f → V.inj _Text f
      BooleanFormat f → V.inj _Boolean f
    fromVariant = V.case_
      # V.on _Default (const DefaultFormat)
      # V.on _Currency CurrencyFormat
      # V.on _Decimal DecimalFormat
      # V.on _Integer IntegerFormat
      # V.on _Date DateFormat
      # V.on _Time TimeFormat
      # V.on _DateTime DateTimeFormat
      # V.on _Text TextFormat
      # V.on _Boolean BooleanFormat
    _Default = SProxy ∷ SProxy "Default"
    _Currency = SProxy ∷ SProxy "Currency"
    _Decimal = SProxy ∷ SProxy "Decimal"
    _Integer = SProxy ∷ SProxy "Integer"
    _Date = SProxy ∷ SProxy "Date"
    _Time = SProxy ∷ SProxy "Time"
    _DateTime = SProxy ∷ SProxy "DateTime"
    _Text = SProxy ∷ SProxy "Text"
    _Boolean = SProxy ∷ SProxy "Boolean"

data Format
  = Default
  | Currency
  | Decimal
  | Integer
  | Date
  | Time
  | DateTime
  | Text
  | Boolean

derive instance eqFormat ∷ Eq Format
derive instance ordFormat ∷ Ord Format
derive instance genericFormat ∷ Generic Format _

formatFromOptions ∷ FormatOptions → Format
formatFromOptions = case _ of
  DefaultFormat → Default
  CurrencyFormat _ → Currency
  DecimalFormat _ → Decimal
  IntegerFormat _ → Integer
  DateFormat _ → Date
  TimeFormat _ → Time
  DateTimeFormat _ → DateTime
  TextFormat _ → Text
  BooleanFormat _ → Boolean

formats ∷ NEL.NonEmptyList Format
formats = SL.toNEL
  $ Default
  : Currency
  : Decimal
  : Integer
  : Date
  : Time
  : DateTime
  : Text
  : Boolean
  : SL.nil

format ∷ Lens.Prism' String Format
format = Lens.prism' to from
  where
    to = case _ of
      Default → "Default"
      Currency → "Currency"
      Decimal → "Decimal"
      Integer → "Integer"
      Date → "Date"
      Time → "Time"
      DateTime → "Date & time"
      Text → "Text"
      Boolean → "Boolean"
    from = case _ of
      "Default" → Just Default
      "Currency" → Just Currency
      "Decimal" → Just Decimal
      "Integer" → Just Integer
      "Date" → Just Date
      "Time" → Just Time
      "Date & time" → Just DateTime
      "Text" → Just Text
      "Boolean" → Just Boolean
      _ → Nothing
