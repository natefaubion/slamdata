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
module SlamData.Workspace.Card.Setups.DisplayOptions.Component.ChildSlot where

import SlamData.Prelude

import Halogen.Component.ChildPath as CP
import SlamData.Workspace.Card.Setups.DisplayOptions.BooleanFormat.Component as BooleanFormat
import SlamData.Workspace.Card.Setups.DisplayOptions.CurrencyFormat.Component as CurrencyFormat
import SlamData.Workspace.Card.Setups.DisplayOptions.DateFormat.Component as DateFormat
import SlamData.Workspace.Card.Setups.DisplayOptions.DateTimeFormat.Component as DateTimeFormat
import SlamData.Workspace.Card.Setups.DisplayOptions.DecimalFormat.Component as DecimalFormat
import SlamData.Workspace.Card.Setups.DisplayOptions.IntegerFormat.Component as IntegerFormat
import SlamData.Workspace.Card.Setups.DisplayOptions.TextFormat.Component as TextFormat
import SlamData.Workspace.Card.Setups.DisplayOptions.TimeFormat.Component as TimeFormat

type ChildQuery
  = CurrencyFormat.Query
  ⨁ DecimalFormat.Query
  ⨁ IntegerFormat.Query
  ⨁ DateFormat.Query
  ⨁ TimeFormat.Query
  ⨁ DateTimeFormat.Query
  ⨁ TextFormat.Query
  ⨁ BooleanFormat.Query
  ⨁ Const Void

type ChildSlot
  = Unit
  ⊹ Unit
  ⊹ Unit
  ⊹ Unit
  ⊹ Unit
  ⊹ Unit
  ⊹ Unit
  ⊹ Unit
  ⊹ Void

cpCurrencyFormat ∷ CP.ChildPath CurrencyFormat.Query ChildQuery Unit ChildSlot
cpCurrencyFormat = CP.cp1

cpDecimalFormat ∷ CP.ChildPath DecimalFormat.Query ChildQuery Unit ChildSlot
cpDecimalFormat = CP.cp2

cpIntegerFormat ∷ CP.ChildPath IntegerFormat.Query ChildQuery Unit ChildSlot
cpIntegerFormat = CP.cp3

cpDateFormat ∷ CP.ChildPath DateFormat.Query ChildQuery Unit ChildSlot
cpDateFormat = CP.cp4

cpTimeFormat ∷ CP.ChildPath TimeFormat.Query ChildQuery Unit ChildSlot
cpTimeFormat = CP.cp5

cpDateTimeFormat ∷ CP.ChildPath DateTimeFormat.Query ChildQuery Unit ChildSlot
cpDateTimeFormat = CP.cp6

cpTextFormat ∷ CP.ChildPath TextFormat.Query ChildQuery Unit ChildSlot
cpTextFormat = CP.cp7

cpBooleanFormat ∷ CP.ChildPath BooleanFormat.Query ChildQuery Unit ChildSlot
cpBooleanFormat = CP.cp8
