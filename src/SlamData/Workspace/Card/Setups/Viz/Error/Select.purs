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

module SlamData.Workspace.Card.Setups.Viz.Error.Select where

import SlamData.Prelude

import SlamData.Workspace.Card.CardType.Select (Select, print)
import Utils (throwVariantError)

data Error
  = NoAxis (Select ())
  | EmptyResource (Select ())
  | TooManyEntries
    { formInputType ∷ Select ()
    , maximum ∷ Int
    , entryCount ∷ Int
    }
  | TooManySelected
    { formInputType ∷ Select ()
    , maximum ∷ Int
    , selectedCount ∷ Int
    }
  | NonUniqueLabel (Select ()) (Maybe String)

instance showVizErrorSelect ∷ Show Error where
  show = case _ of
    NoAxis fit → "(NoAxis " <> print case_ fit <> ")"
    EmptyResource fit → "(EmptyResource " <> print case_ fit <> ")"
    TooManyEntries { formInputType, maximum, entryCount } →
      "(TooManyEntries "
      <> "{ formInputType: " <> print case_ formInputType
      <> ", maximum: " <> show maximum
      <> ", entryCount: " <> show entryCount
      <> " })"
    TooManySelected { formInputType, maximum, selectedCount } →
      "(TooManySelected "
      <> "{ formInputType: " <> print case_ formInputType
      <> ", maximum: " <> show maximum
      <> ", selectedCount: " <> show selectedCount
      <> " })"
    NonUniqueLabel fit label →
      "(NonUniqueLabel " <> print case_ fit <> " " <> show label <> ")"

throwFormInputLabeledError
  ∷ ∀ v m a
  . MonadThrow (Variant (formInputLabeled ∷ Error | v)) m
  ⇒ Error
  → m a
throwFormInputLabeledError = throwVariantError (SProxy :: SProxy "formInputLabeled")
