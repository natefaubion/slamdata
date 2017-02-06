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

module SlamData.Workspace.Card.Eval.Common where

import SlamData.Prelude

import Control.Monad.Aff.Class (class MonadAff)
import Control.Monad.Throw (class MonadThrow, throw)

import Data.Argonaut as J
import Data.Array as Array
import Data.Path.Pathy as Path
import Data.StrMap as SM

import Quasar.Types (FilePath)

import SlamData.Effects (SlamDataEffects)
import Quasar.Advanced.QuasarAF as QF
import SlamData.Quasar.Error as QE
import SlamData.Quasar.Class (class QuasarDSL, class ParQuasarDSL, sequenceQuasar)
import SlamData.Workspace.Card.Eval.Monad as CEM
import SlamData.Workspace.Card.Port as Port

escapeCursor ∷ J.JCursor → String
escapeCursor = case _ of
  J.JField c cs → "." <> Port.escapeIdentifier c <> escapeCursor cs
  J.JIndex c cs → "[" <> show c <> "]" <> escapeCursor cs
  J.JCursorTop  → ""

validateResources
  ∷ ∀ m t
  . ( MonadAff SlamDataEffects m
    , MonadThrow CEM.CardError m
    , QuasarDSL m
    , ParQuasarDSL m
    , Traversable t
    )
  ⇒ t FilePath
  → m Unit
validateResources fs = do
  res ← sequenceQuasar (map (\path → Tuple path <$> QF.fileMetadata path) fs)
  for_ res case _ of
    path × Left reason →
      throw $ QE.prefixMessage ("Resource `" ⊕ Path.printPath path ⊕ "` is unavailable") reason
    _ →
      pure unit

evalComposite
  ∷ ∀ m
  . MonadAsk CEM.CardEnv m
  ⇒ m Port.DataMap
evalComposite = do
  CEM.CardEnv { children } ← ask
  pure (foldl mergeChildren SM.empty children)
  where
    mergeChildren ∷ Port.DataMap → CEM.ChildOut → Port.DataMap
    mergeChildren vm { namespace, varMap } =
      case namespace of
        "" → SM.fold (update id) vm varMap
        ns → SM.fold (update \k → ns <> "." <> k) vm varMap

    update mkKey acc key val =
      SM.alter
        case val, _ of
          Right (Port.SetLiteral s1), Just (Right (Port.SetLiteral s2)) →
            Just (Right (Port.SetLiteral (s1 <> s2)))
          _, rhs@(Just (Right (Port.SetLiteral s))) →
            let
              val' = toValue val
            in if Array.elem val' s
              then rhs
              else Just (Right (Port.SetLiteral (Array.snoc s val')))
          _, Just v →
            let
              v1 = toValue val
              v2 = toValue v
            in if v1 ≡ v2
              then Just val
              else Just (Right (Port.SetLiteral [ v1, v2 ]))
          _, Nothing →
            Just val
        (mkKey key)
        acc

    toValue =
      either Port.resourceToVarMapValue id
