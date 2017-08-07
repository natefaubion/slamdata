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

module SlamData.Workspace.Card.FormInput.Eval
  ( evalTextLike
  , evalLabeled
  , module SlamData.Workspace.Card.FormInput.Model
  ) where

import SlamData.Prelude

import Control.Monad.Aff.Class (class MonadAff)
import Control.Monad.State (class MonadState, get, put)
import Control.Monad.Writer.Class (class MonadTell)
import Data.Lens (preview, (?~) )
import Data.List as L
import Data.Map as Map
import Data.Set as Set
import SlamData.Effects (SlamDataEffects)
import SlamData.Quasar.Class (class QuasarDSL, class ParQuasarDSL)
import SlamData.Quasar.Query as QQ
import SlamData.Workspace.Card.CardType.FormInputType as FIT
import SlamData.Workspace.Card.Error as CE
import SlamData.Workspace.Card.Eval.Common as CEC
import SlamData.Workspace.Card.Eval.Monad as CEM
import SlamData.Workspace.Card.Eval.State as CES
import SlamData.Workspace.Card.FormInput.LabeledRenderer.Model as LR
import SlamData.Workspace.Card.FormInput.Model (Model(..))
import SlamData.Workspace.Card.FormInput.TextLikeRenderer.Model as TLR
import SlamData.Workspace.Card.Port as Port
import SlamData.Workspace.Card.Port.VarMap as VM
import SlamData.Workspace.Card.Setups.Semantics as Sem
import SqlSquared (SqlQuery)
import SqlSquared as Sql
import Utils (stringToNumber)
import Utils.SqlSquared (all, asRel, variRelation)

defaultSelectionVar ∷ String
defaultSelectionVar = "selection"

eval
  ∷ ∀ m v
  . MonadAff SlamDataEffects m
  ⇒ MonadAsk CEM.CardEnv m
  ⇒ MonadThrow (Variant (qerror ∷ CE.QError | v)) m
  ⇒ MonadTell CEM.CardLog m
  ⇒ MonadState CEM.CardState m
  ⇒ QuasarDSL m
  ⇒ ParQuasarDSL m
  ⇒ SqlQuery
  → Sql.Sql
  → Port.Resource
  → m Port.Out
eval sql selection r = do
  CEM.CardEnv { varMap, cardId } ← ask
  let varMap' = varMap # VM.insert cardId (VM.Var defaultSelectionVar) (VM.Expr selection)
  resource ← CE.liftQ $ CEC.localEvalResource sql varMap'
  pure $ Port.resourceOut cardId resource varMap'

evalLabeled
  ∷ ∀ m
  . MonadAff SlamDataEffects m
  ⇒ MonadAsk CEM.CardEnv m
  ⇒ MonadThrow CE.CardError m
  ⇒ MonadTell CEM.CardLog m
  ⇒ MonadState CEM.CardState m
  ⇒ QuasarDSL m
  ⇒ ParQuasarDSL m
  ⇒ LR.Model
  → Port.SetupLabeledFormInputPort
  → m Port.Out
evalLabeled m p = do
  resourceVar × r ← CEM.extractResourcePair Port.Initial
  cardState ← get
  let
    lastUsedResource = cardState >>= preview CES._LastUsedResource

    availableValues =
      Set.fromFoldable $ Map.keys p.valueLabelMap

    selected
      -- Reloading
      | lastUsedResource ≡ Nothing
        ∧ (not $ Set.isEmpty m.selected)
        ∧ (not $ Set.isEmpty $ Set.intersection availableValues m.selected)  =
          m.selected
      -- same resource: take selected from model
      | lastUsedResource ≡ Just r =
          m.selected
      -- new resource and checkbox: empty selection
      | p.formInputType ≡ FIT.Checkbox =
          Set.empty
      -- default selection is empty, new resource this is not checkbox: take first value
      | Set.isEmpty p.selectedValues =
          Set.fromFoldable $ L.head $ Map.keys p.valueLabelMap
      -- new resource, not checkbox: take default selection
      | otherwise =
          p.selectedValues

    semantics =
      foldMap Sem.semanticsToSql selected

    selection =
      Sql.set semantics

    sql =
      Sql.buildSelect
        $ all
        ∘ (Sql._relations
            ?~ (variRelation (unwrap resourceVar) # asRel "res"))
        ∘ (Sql._filter
             ?~ ( Sql.binop Sql.In
                    ( Sql.binop Sql.FieldDeref
                        ( Sql.ident "res" )
                        ( QQ.jcursorToSql Nothing p.cursor ))
                    ( Sql.vari defaultSelectionVar)))


  put $ Just $ CEM.AutoSelect {lastUsedResource: r, autoSelect: selected}

  eval (Sql.Query mempty sql) selection r

evalTextLike
  ∷ ∀ m
  . MonadAff SlamDataEffects m
  ⇒ MonadAsk CEM.CardEnv m
  ⇒ MonadThrow CE.CardError m
  ⇒ MonadTell CEM.CardLog m
  ⇒ MonadState CEM.CardState m
  ⇒ QuasarDSL m
  ⇒ ParQuasarDSL m
  ⇒ TLR.Model
  → Port.SetupTextLikeFormInputPort
  → m Port.Out
evalTextLike m p = do
  resourceVar × r ← CEM.extractResourcePair Port.Initial
  cardState ← get
  let
    selection = case p.formInputType of
      FIT.Numeric → maybe Sql.null Sql.num $ stringToNumber m.value
      FIT.Time → Sql.invokeFunction "TIME" $ pure $ Sql.string m.value
      FIT.Date → Sql.invokeFunction "DATE" $ pure $ Sql.string m.value
      FIT.Datetime → Sql.invokeFunction "TIMESTAMP" $ pure $ Sql.string m.value
      _ → Sql.string m.value
    sql =
      Sql.buildSelect
        $ all
        ∘ (Sql._relations
            ?~ (variRelation (unwrap resourceVar) # asRel "res"))
        ∘ (Sql._filter
             ?~ ( Sql.binop Sql.Eq
                    ( Sql.binop Sql.FieldDeref
                        ( Sql.ident "res" )
                        ( QQ.jcursorToSql Nothing p.cursor ))
                    (Sql.vari defaultSelectionVar) ))

  eval (Sql.Query mempty sql) selection r
