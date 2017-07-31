{-
Copyright 2015 SlamData, Inc.

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

module Test.SlamData.Property.Workspace.Card.Markdown.Model where

import SlamData.Prelude

import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Unsafe as Unsafe

import Data.JSDate (LOCALE)
import Data.Set as Set
import Data.StrMap as SM

import SlamData.Workspace.Card.Markdown.Model as M
import SlamData.Workspace.Card.Markdown.Interpret (formFieldDefaultValue)
import SlamData.Workspace.Card.Markdown.Component.State as MDS

import Text.Markdown.SlamDown.Halogen.Component.State as SDS

import Test.StrongCheck (SC, Result(..), quickCheck, (<?>))

checkSerialization ∷ ∀ eff. SC eff Unit
checkSerialization =
  quickCheck $ \(SDS.SlamDownState { document, formState }) →
    case M.decode $ M.encode formState of
      Left err → Failed $ "Decode failed: " <> err
      Right state →
        fold
         [ state ≡ state <?> "state mismatch: " <> show state <> " vs. " <> show state
         ]

unsafeRunLocale
  ∷ ∀ a
  . Eff (locale ∷ LOCALE) a
  → a
unsafeRunLocale =
  Unsafe.unsafePerformEff

checkVarMapConstruction ∷ ∀ eff. SC eff Unit
checkVarMapConstruction =
  quickCheck \(SDS.SlamDownState { document, formState }) →
    let
      inputState = SDS.formStateFromDocument document
      varMap = Right ∘ formFieldDefaultValue <$> MDS.updateFormState inputState formState
      descKeys = Set.fromFoldable $ SM.keys inputState
      stateKeys = Set.fromFoldable $ SM.keys varMap
    in
      descKeys ≡ stateKeys
        <?> ("Keys mismatch: " <> show descKeys <> " vs. " <> show stateKeys)

check ∷ ∀ eff. SC eff Unit
check = do
  checkVarMapConstruction
  checkSerialization
