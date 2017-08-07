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

module SlamData.FileSystem.Dialog.Mount.Common.SettingsQuery where

import SlamData.Prelude

import Halogen as H
import Halogen.Component.Utils as HCU

data SettingsQuery s a = ModifyState (s → s) a

onModify ∷ ∀ s a. (s → s) → SettingsQuery s a → SettingsQuery s a
onModify f (ModifyState g a) = ModifyState (f ∘ g) a

data SettingsMessage a = Modified (Either String a)

derive instance functorSettingsMessage ∷ Functor SettingsMessage

eval
  ∷ ∀ s a g i m
  . (s → Either String a)
  → SettingsQuery s
  ~> H.HalogenM s (SettingsQuery s) g i (SettingsMessage a) m
eval f (ModifyState g next) = do
  st ← HCU.modify g
  H.raise $ Modified (f st)
  pure next
