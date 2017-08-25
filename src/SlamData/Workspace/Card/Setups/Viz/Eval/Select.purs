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

module SlamData.Workspace.Card.Setups.Viz.Eval.Select where

import SlamData.Prelude

import Data.Argonaut as J
import Data.Array as A
import Data.Lens (preview)
import Data.Map as Map
import Data.Set as Set
import SlamData.Workspace.Card.CardType as CT
import SlamData.Workspace.Card.CardType.Select as Sel
import SlamData.Workspace.Card.Port as Port
import SlamData.Workspace.Card.Setups.Dimension as D
import SlamData.Workspace.Card.Setups.Viz.Error.Select as LE
import SlamData.Workspace.Card.Setups.Semantics as Sem
import SlamData.Workspace.Card.Setups.Viz.Eval.Common (VizEval)
import SlamData.Workspace.Card.Setups.DimensionMap.Projection as P

eval ∷ ∀ m. VizEval m (P.DimMap → Array J.Json → CT.Select () → m Port.Port)
eval dimMap records formInputType
  | A.null records =
      LE.throwFormInputLabeledError $ LE.EmptyResource formInputType
  | otherwise = do
    selectedValues × valueLabelMap × _ × _ ←
      A.foldM foldFn (Set.empty × Map.empty × 0 × 0) records
    let projection = unsafePartial fromJust $ P.lookup P.formValue dimMap
    pure
      $ Port.SetupSelect
          { projection
          , valueLabelMap
          , selectedValues
          , formInputType
          }
  where
  mbFormValue = P.lookup P.formValue dimMap >>= ( preview $ D._value ∘ D._projection )
  mbFormSelected = P.lookup P.formSelected dimMap >>= ( preview $ D._value ∘ D._projection )
  mbFormLabel = P.lookup P.formLabel dimMap >>= ( preview $ D._value ∘ D._projection )

  foldFn acc@(selected × vlmap × keyCount × selectedCount) record = do
    when (keyCount > Sel.maximumCountOfEntries formInputType)
      $ LE.throwFormInputLabeledError
      $ LE.TooManyEntries
          { formInputType
          , maximum: Sel.maximumCountOfEntries formInputType
          , entryCount: keyCount
          }
    when (selectedCount > Sel.maximumCountOfSelectedValues formInputType)
      $ LE.throwFormInputLabeledError
      $ LE.TooManySelected
          { formInputType
          , maximum: Sel.maximumCountOfEntries formInputType
          , selectedCount
          }

    newKeyCount × newVlmap ←
      case Sem.getSemantics record =<< mbFormValue of
        Nothing →
          pure $ keyCount × vlmap
        Just value → do
          let
            mbNewLabel = Sem.getMaybeString record =<< mbFormLabel
          case Map.lookup value vlmap of
            Nothing →
              pure
                $ (keyCount + one)
                × Map.insert value mbNewLabel vlmap
            Just mbExistingLabel → do
              when (mbExistingLabel ≠ mbNewLabel)
                $ LE.throwFormInputLabeledError
                $ LE.NonUniqueLabel formInputType mbExistingLabel
              pure $ keyCount × vlmap

    newSelCount × newSelected ←
      pure $ case Sem.getSemantics record =<< mbFormSelected of
        Nothing → selectedCount × selected
        Just value
          | Set.member value selected → selectedCount × selected
          | otherwise → (selectedCount + one) × Set.insert value selected
    pure $ newSelected × newVlmap × newKeyCount × newSelCount
