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

module SlamData.Theme.Option
  ( Option(..)
  , fromTheme
  , options
  , toLabel
  , valuePrism'
  ) where

import SlamData.Prelude

import Data.Lens.Prism (Prism', prism')
import SlamData.Theme.Theme as Theme

data Option
  = Default
  | Light
  | Dark
  | Custom

derive instance eqOption ∷ Eq Option

options ∷ Array Option
options =
  [ Default
  , Light
  , Dark
  , Custom
  ]

valuePrism' ∷ Prism' String Option
valuePrism' =
  prism' to fro
  where
    to ∷ Option → String
    to = case _ of
      Default → "default"
      Light → "light"
      Dark → "dark"
      Custom → "custom"

    fro ∷ String → Maybe Option
    fro = case _ of
      "default" → Just Default
      "light" → Just Light
      "dark" → Just Dark
      "custom" → Just Custom
      _ → Nothing

toLabel ∷ Option → String
toLabel = case _ of
  Default → "Default (" <> Theme.defaultLabel <> ")"
  Light → "Light"
  Dark → "Dark"
  Custom → "Custom"

fromTheme ∷ Theme.Theme → Option
fromTheme = case _ of
  Theme.Light → Light
  Theme.Dark → Dark
  Theme.Custom _ → Custom
