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

module SlamData.Workspace.Card.Markdown.Interpret
  ( formFieldValue
  , formFieldDefaultValue
  , formFieldConstrainValue
  ) where

import SlamData.Prelude

import Data.DateTime as DT
import Data.Foldable as F
import Data.Formatter.DateTime as FD
import Data.Identity (Identity(..))
import Data.List as L
import Data.Maybe as M
import Matryoshka (project)
import SlamData.Workspace.Card.Markdown.Model as Model
import SlamData.Workspace.Card.Port.VarMap as VM
import SqlSquared as Sql
import Text.Markdown.SlamDown as SD
import Text.Markdown.SlamDown.Halogen.Component.State as SDS

getLiteral
  ∷ ∀ m
  . Plus m
  ⇒ Applicative m
  ⇒ Model.MarkdownExpr
  → m Sql.Sql
getLiteral = case _ of
  Model.MarkdownExpr sql | Sql.Literal _ ← project sql → pure sql
  _ → empty

-- | A sanity check for form fields. When transitioning form state, you can end up with an
-- | invalid selection for a given set of options. This just checks that it makes sense,
-- | otherwise it will pick a sensible default.
formFieldConstrainValue ∷ SDS.FormFieldValue Model.MarkdownExpr → SDS.FormFieldValue Model.MarkdownExpr
formFieldConstrainValue formField = case formField of
  SD.CheckBoxes (Identity sels) (Identity options) →
    let sels' = L.mapMaybe (\sel → F.find (eq sel) options) sels
    in SD.CheckBoxes (Identity sels') (Identity options)
  SD.RadioButtons (Identity sel) (Identity options)
    | F.elem sel options → formField
    | Just sel' ← L.head options → SD.RadioButtons (Identity sel') (Identity options)
    | otherwise → formField
  SD.DropDown mbSel (Identity options)
    | Just (Identity sel) ← mbSel, F.elem sel options → formField
    | Just sel ← L.head options → SD.DropDown (Just (Identity sel)) (Identity options)
    | otherwise → SD.DropDown Nothing (Identity options)
  _ → formField

formFieldDefaultValue ∷ SDS.FormFieldValue Model.MarkdownExpr → VM.VarMapValue
formFieldDefaultValue formField = case formField of
  SD.TextBox tb → case tb of
    SD.PlainText _ → defaultValue (VM.Expr $ Sql.string "") formField
    SD.Numeric _ → defaultValue (VM.Expr $ Sql.int 0) formField
    _ → defaultValue (VM.Expr Sql.null) formField
  SD.CheckBoxes (Identity sels) (Identity options) →
    VM.Expr $ Sql.set $ unwrap <$> sels
  SD.RadioButtons (Identity sel) (Identity options)
    | F.elem sel options → VM.Expr $ unwrap sel
    | otherwise → VM.Expr $ Sql.null
  SD.DropDown mbSel (Identity options)
    | Just (Identity sel) ← mbSel, F.elem sel options → VM.Expr $ unwrap sel
    | Just sel ← L.head options → VM.Expr $ unwrap sel
    | otherwise → VM.Expr $ Sql.null
  where
  defaultValue a = fromMaybe a ∘ formFieldValue

formFieldValue ∷ SDS.FormFieldValue Model.MarkdownExpr → M.Maybe VM.VarMapValue
formFieldValue = case _ of
  SD.TextBox tb → VM.Expr <$> do
    tb' ← SD.traverseTextBox unwrap tb
    case tb' of
      SD.PlainText (Identity x) →
        pure $ Sql.string x
      SD.Numeric (Identity x) →
        pure $ Sql.hugeNum x
      SD.Date (Identity x) →
        hush $ FD.formatDateTime "YYYY-MM-DD" (DT.DateTime x bottom) <#> \s →
          Sql.invokeFunction "DATE" $ pure $ Sql.string s
      SD.Time _ (Identity x) →
        hush $ FD.formatDateTime "HH:mm:ss" (DT.DateTime bottom x) <#> \s →
          Sql.invokeFunction "TIME" $ pure $ Sql.string s
      SD.DateTime _ (Identity x) →
        hush $ FD.formatDateTime "YYYY-MM-DDTHH:mm:ssZ" x <#> \s →
          Sql.invokeFunction "TIMESTAMP" $ pure $ Sql.string s
  SD.CheckBoxes (Identity sel) _ →
      pure $ VM.Expr $ Sql.set $ L.mapMaybe getLiteral sel
  SD.RadioButtons (Identity x) _ →
    VM.Expr <$> getLiteral x
  SD.DropDown mx _ → VM.Expr <$> do
    Identity x ← mx
    getLiteral x
