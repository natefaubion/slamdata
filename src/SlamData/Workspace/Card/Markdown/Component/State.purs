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

module SlamData.Workspace.Card.Markdown.Component.State where

import SlamData.Prelude

import Data.BrowserFeatures (BrowserFeatures)
import Data.StrMap as SM
import SlamData.Workspace.Card.Markdown.Interpret (formFieldConstrainValue)
import SlamData.Workspace.Card.Markdown.Model as Model
import Text.Markdown.SlamDown.Halogen.Component as SDH
import Text.Markdown.SlamDown.Halogen.Component.State as SDS
import Text.Markdown.SlamDown.Syntax.FormField as SDF

type State =
  { browserFeatures ∷ Maybe BrowserFeatures
  , state ∷ SDS.SlamDownFormState Model.MarkdownExpr
  }

initialState ∷ State
initialState =
  { browserFeatures: Nothing
  , state: SM.empty
  }

updateFormState
  ∷ SDH.SlamDownFormState Model.MarkdownExpr
  → SDH.SlamDownFormState Model.MarkdownExpr
  → SDH.SlamDownFormState Model.MarkdownExpr
updateFormState newState oldState =
  SM.fold
    (\m k v → SM.insert k (formFieldConstrainValue $ valueForKey k v) m)
    SM.empty
    newState
  where
  valueForKey
    ∷ String
    → SDH.FormFieldValue Model.MarkdownExpr
    → SDH.FormFieldValue Model.MarkdownExpr
  valueForKey k newField = case SM.lookup k oldState of
    Nothing → newField
    Just oldField → case oldField, newField of
      SDF.DropDown oldSel _, SDF.DropDown _ newOptions → SDF.DropDown oldSel newOptions
      SDF.CheckBoxes oldSel _, SDF.CheckBoxes _ newOptions → SDF.CheckBoxes oldSel newOptions
      SDF.RadioButtons oldSel _, SDF.RadioButtons _ newOptions → SDF.RadioButtons oldSel newOptions
      SDF.TextBox oldSel, SDF.TextBox newSel → case oldSel, newSel of
        SDF.PlainText _, SDF.PlainText _ → oldField
        SDF.Numeric _, SDF.Numeric _ → oldField
        SDF.Date _, SDF.Date _ → oldField
        SDF.Time _ _, SDF.Time _ _ → oldField
        SDF.DateTime _ _, SDF.DateTime _ _ → oldField
        _, _ → newField
      _, _ → newField
