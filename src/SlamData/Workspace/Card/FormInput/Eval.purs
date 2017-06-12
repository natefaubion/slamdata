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
import Data.Lens (preview, (?~), (.~))
import Data.List as L
import Data.Map as Map
import Data.Path.Pathy as Path
import Data.Set as Set
import Data.StrMap as SM
import SlamData.Effects (SlamDataEffects)
import SlamData.Quasar.Class (class QuasarDSL, class ParQuasarDSL)
import SlamData.Quasar.Error as QE
import SlamData.Quasar.FS as QFS
import SlamData.Quasar.Query as QQ
import SlamData.Workspace.Card.CardType.FormInputType as FIT
import SlamData.Workspace.Card.Error as CE
import SlamData.Workspace.Card.Eval.Common (validateResources)
import SlamData.Workspace.Card.Eval.Monad as CEM
import SlamData.Workspace.Card.Eval.State as CES
import SlamData.Workspace.Card.FormInput.LabeledRenderer.Model as LR
import SlamData.Workspace.Card.FormInput.Model (Model(..))
import SlamData.Workspace.Card.FormInput.TextLikeRenderer.Model as TLR
import SlamData.Workspace.Card.Port as Port
import SlamData.Workspace.Card.Setups.Semantics as Sem
import SqlSquared (SqlQuery)
import SqlSquared as Sql
import Utils (stringToNumber)
import Utils.SqlSquared (all, asRel, tableRelation)

-- | I removed additional variable from this (It used to be `QueryExpr`)
-- | not sure that this is right decision, but it's just part of sql expression
-- | encoded as string. And it's actually can't be used to query anything
-- | because it's kinda `"foo"` or `("foo", "bar", true)`
-- | @cryogenian
eval
  ∷ ∀ m
  . MonadAff SlamDataEffects m
  ⇒ MonadAsk CEM.CardEnv m
  ⇒ MonadThrow CE.CardError m
  ⇒ MonadTell CEM.CardLog m
  ⇒ MonadState CEM.CardState m
  ⇒ QuasarDSL m
  ⇒ ParQuasarDSL m
  ⇒ SqlQuery
  → Port.Resource
  → m Port.Out
eval sql r = do
  tmpPath × resource ← CEM.temporaryOutputResource
  backendPath ←
    fromMaybe Path.rootDir ∘ Path.parentDir <$> CEM.anyTemporaryPath (Port.filePath r)
  { inputs } ←
    CE.liftQ $ lmap (QE.prefixMessage "Error compiling query") <$>
      QQ.compile backendPath sql SM.empty

  validateResources inputs
  CEM.addSources inputs
  _ ← CE.liftQ do
    _ ← QQ.viewQuery tmpPath sql SM.empty
    QFS.messageIfFileNotFound tmpPath "Requested collection doesn't exist"
  CEM.resourceOut (Port.View resource sql SM.empty)

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
  → Port.Resource
  → m Port.Out
evalLabeled m p r = do
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

    sql =
      Sql.buildSelect
        $ all
        ∘ (Sql._relations
            .~ (tableRelation (Port.filePath r) <#> asRel "res"))
        ∘ (Sql._filter
             ?~ ( Sql.binop Sql.In
                    ( Sql.binop Sql.FieldDeref
                        ( Sql.ident "res" )
                        ( QQ.jcursorToSql p.cursor ))
                    ( Sql.set semantics )))


  put $ Just $ CEM.AutoSelect {lastUsedResource: r, autoSelect: selected}

  eval (Sql.Query mempty sql) r

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
  → Port.Resource
  → m Port.Out
evalTextLike m p r = do
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
            .~ ( tableRelation (Port.filePath r) <#> asRel "res"))
        ∘ (Sql._filter
             ?~ ( Sql.binop Sql.Eq
                    ( Sql.binop Sql.FieldDeref
                        ( Sql.ident "res" )
                        ( QQ.jcursorToSql p.cursor ))
                    selection ))

  eval (Sql.Query mempty sql) r
