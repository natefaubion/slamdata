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

module SlamData.Workspace.Card.Variables.Eval where

import SlamData.Prelude

import Data.Map as Map
import Data.StrMap as SM
import Data.Validation.Semigroup (V)
import Data.Validation.Semigroup as V
import SlamData.SqlSquared.Tagged (ParseError)
import SlamData.Workspace.Card.CardId (CardId)
import SlamData.Workspace.Card.Eval.Monad as CEM
import SlamData.Workspace.Card.Port as Port
import SlamData.Workspace.Card.Port.VarMap as VM
import SlamData.Workspace.Card.Variables.Error (VError(..), VariablesError(..), throwVariablesError)
import SlamData.Workspace.Card.Variables.Error.TypeMismatchError (TypeMismatchError)
import SlamData.Workspace.Card.Variables.Model (Model)
import SlamData.Workspace.FormBuilder.Item.Component.State (sanitiseValueFromForm)
import SlamData.Workspace.FormBuilder.Item.Model as FB

eval
  ∷ ∀ m v
  . MonadAsk CEM.CardEnv m
  ⇒ MonadThrow (Variant (variables ∷ VariablesError | v)) m
  ⇒ Model
  → m Port.Out
eval model = do
  CEM.CardEnv { cardId, urlVarMaps } ← ask
  varMap ← V.unV throwVariablesError pure $ buildVarMap cardId urlVarMaps model
  pure (Port.Variables × varMap)

buildVarMap ∷ CardId → Map.Map CardId Port.URLVarMap → Model → V VariablesError Port.VarMap
buildVarMap cardId urlVarMaps model =
  foldl go (pure VM.empty) model.items
  where
    go ∷ V VariablesError Port.VarMap → FB.Model → V VariablesError Port.VarMap
    go acc { name, fieldType, defaultValue } = case unit of
      _ | defaultValue == Nothing || defaultValue == Just "" → acc
      _ | unwrap name == "" →
            -- TODO: are there other cases? -gb
            accumError (InvalidVariableNameError name)
      _ | V.unV (const false) (VM.member (VM.Var (unwrap name))) acc →
            accumError (DuplicateVariableError name)
      _ | otherwise →
            case SM.lookup (unwrap name) =<< Map.lookup cardId urlVarMaps, defaultValue of
              -- If we have a VarMap value from the URL, prefer it over the default value
              Just cardValue, _ →
                parseValue FB.urlVarMapValueToVarMapValue (URLValueError) cardValue
              _, Just defaultValue' →
                -- TODO: sanitiseValueFromForm can be removed at some point in the
                -- future, but for now ommitting it may cause some glitches with
                -- existing workspaces (will start complaining about incorrect
                -- format, but editing the value will fix it) -gb
                parseValue
                  (\ft v → FB.defaultValueToVarMapValue ft $ sanitiseValueFromForm ft v)
                  DefaultValueError
                  defaultValue'
              _, _ →
                acc
      where
        parseValue
          ∷ ∀ a
          . (FB.FieldType -> a -> Either (Either ParseError TypeMismatchError) Port.VarMapValue)
          → (FB.FieldName → Either ParseError TypeMismatchError → VError)
          → a
          → V VariablesError Port.VarMap
        parseValue f toError value =
          either
            (accumError ∘ toError name)
            (\v → map (VM.insert cardId (VM.Var (unwrap name)) v) acc)
            (f fieldType value)

        accumError ∷ VError -> V VariablesError Port.VarMap
        accumError err = acc <* V.invalid (VariablesError (pure err))
