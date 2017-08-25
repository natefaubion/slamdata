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

module SlamData.Workspace.Card.Variables.Error where

import SlamData.Prelude

import Data.List.NonEmpty (NonEmptyList)
import SlamData.SqlSquared.Tagged (ParseError)
import SlamData.Workspace.FormBuilder.Item.Model (FieldName)
import SlamData.Workspace.Card.Variables.Error.TypeMismatchError (TypeMismatchError)
import Utils (throwVariantError)

data VError
  = DefaultValueError FieldName (Either ParseError TypeMismatchError)
  | URLValueError FieldName (Either ParseError TypeMismatchError)
  | DuplicateVariableError FieldName
  | InvalidVariableNameError FieldName

instance showVError ∷ Show VError where
  show = case _ of
    DefaultValueError name err → "(DefaultValueError " <> show name <> " " <> show err <> ")"
    URLValueError name err → "(URLValueError " <> show name <> " " <> show err <> ")"
    DuplicateVariableError name → "(DuplicateVariableError " <> show name <> ")"
    InvalidVariableNameError name → "(InvalidVariableNameError " <> show name <> ")"

newtype VariablesError = VariablesError (NonEmptyList VError)

derive newtype instance semigroupVariablesError ∷ Semigroup VariablesError

instance showVariablesError ∷ Show VariablesError where
  show (VariablesError nel) = "(VariablesError " <> show nel <> ")"

throwVariablesError ∷ forall v m a. MonadThrow (Variant (variables ∷ VariablesError | v)) m ⇒ VariablesError → m a
throwVariablesError = throwVariantError (SProxy :: SProxy "variables")
