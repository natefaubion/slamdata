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

module SlamData.Theme.LocalStorage (default, defaultLabel) where

import SlamData.Prelude

import Data.Codec as C
import SlamData.LocalStorage.Class as LS
import SlamData.LocalStorage.Keys as LK
import SlamData.Theme.Theme (Theme(..), codec, toLabel)

defaultLabel
  ∷ ∀ m
  . Monad m
  ⇒ LS.LocalStorageDSL m
  ⇒ m String
defaultLabel = map toLabel default

default
  ∷ ∀ m
  . Monad m
  ⇒ LS.LocalStorageDSL m
  ⇒ m Theme
default = do
  defaultTheme ← hush <$> LS.retrieve (lmap show ∘ C.decode codec) LK.adminUIDefaultTheme
  pure (fromMaybe Light defaultTheme)
