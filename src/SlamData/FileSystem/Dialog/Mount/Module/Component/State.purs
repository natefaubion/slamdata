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

module SlamData.FileSystem.Dialog.Mount.Module.Component.State where

import SlamData.Prelude

import Data.String as Str
import Quasar.Mount.Module as QMM

type State = QMM.Config

initialState ∷ State
initialState = { "module": "" }

fromConfig ∷ QMM.Config → State
fromConfig = id

toConfig ∷ State → Either String QMM.Config
toConfig st
  | Str.trim st."module" == "" =
      Left "A module cannot be left empty"
  | otherwise =
      Right st
