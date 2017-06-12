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
  ( formFieldValueToVarMapValue
  , formFieldEmptyValue
  ) where

import SlamData.Prelude

import Data.DateTime as DT
import Data.Identity (Identity(..))
import Data.List as L
import Data.Formatter.DateTime as FD
import Data.Maybe as M

import Matryoshka (embed, project)

import SqlSquared as Sql

import SlamData.Workspace.Card.Markdown.Model as Model
import SlamData.Workspace.Card.Port.VarMap as VM

import Text.Markdown.SlamDown as SD
import Text.Markdown.SlamDown.Halogen.Component.State as SDS

import Utils (hush)

-- The use of this function in formFieldValueToVarMapValue is suspicious, and
-- lead me to think that we have not arranged our data structures properly. In
-- particular, we need to decide what it means to have a collection of SQL^2
-- query expressions. Currently, we end up just filtering them out.

-- One option that we might want to consider, in case we do intend to support things
-- like arrays of query expressions, etc. would be to have something like
-- `type VarMapValue = Mu (Coproduct EJsonF ExprF)`.

getLiteral
  ∷ ∀ m
  . Plus m
  ⇒ Applicative m
  ⇒ Model.MarkdownExpr
  → m Sql.Sql
getLiteral = case _ of
  Model.MarkdownExpr sql | Sql.Literal _ ← project sql → pure sql
  _ → empty

formFieldEmptyValue
  ∷ ∀ f a
  . SD.FormFieldP f a
  → VM.VarMapValue
formFieldEmptyValue field =
  VM.Expr case field of
    SD.TextBox tb →
      case tb of
        SD.PlainText _ → Sql.string ""
        SD.Numeric _ → Sql.int 0
        SD.Date _ → Sql.null
        SD.Time _ _ → Sql.null
        SD.DateTime _ _ → Sql.null
    SD.CheckBoxes _ _ → Sql.set []
    SD.RadioButtons _ _ → Sql.null
    SD.DropDown _ _ → Sql.null

formFieldValueToVarMapValue
  ∷ ∀ m
  . Monad m
  ⇒ SDS.FormFieldValue Model.MarkdownExpr
  → m (M.Maybe VM.VarMapValue)
formFieldValueToVarMapValue v = runMaybeT case v of
  SD.TextBox tb → map VM.Expr do
    tb' ←
      liftMaybe $ SD.traverseTextBox unwrap tb
    case tb' of
      SD.PlainText (Identity x) →
        pure $ Sql.string x
      SD.Numeric (Identity x) →
        pure $ Sql.hugeNum x
      SD.Date (Identity x) →
        liftMaybe
        $ hush
        $ FD.formatDateTime "YYYY-MM-DD" (DT.DateTime x bottom) <#> \s →
          Sql.invokeFunction "DATE" $ pure $ Sql.string s
      SD.Time _ (Identity x) →
        liftMaybe
        $ hush
        $ FD.formatDateTime "HH:mm:ss" (DT.DateTime bottom x) <#> \s →
          Sql.invokeFunction "TIME" $ pure $ Sql.string s
      SD.DateTime _ (Identity x) →
        liftMaybe
        $ hush
        $ FD.formatDateTime "YYYY-MM-DDTHH:mm:ssZ" x <#> \s →
          Sql.invokeFunction "TIMESTAMP" $ pure $ Sql.string s
  SD.CheckBoxes (Identity sel) _ →
      pure
      $ VM.Expr
      $ embed
      $ Sql.SetLiteral
      $ L.mapMaybe (getLiteral) sel
  SD.RadioButtons (Identity x) _ →
    map VM.Expr $ getLiteral x
  SD.DropDown mx _ → map VM.Expr do
    Identity x ← liftMaybe mx
    getLiteral x

  where
  liftMaybe
    ∷ ∀ n a
    . (Applicative n)
    ⇒ Maybe a
    → MaybeT n a
  liftMaybe =
    MaybeT ∘ pure
