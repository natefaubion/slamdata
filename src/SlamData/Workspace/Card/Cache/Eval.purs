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

module SlamData.Workspace.Card.Cache.Eval where

import SlamData.Prelude

import Data.Lens ((^?))
import Data.Path.Pathy as Path
import Data.StrMap as SM

import Quasar.Types (FilePath)

import SlamData.Quasar.Class (class QuasarDSL)
import SlamData.Quasar.Error as QE
import SlamData.Quasar.FS as QFS
import SlamData.Quasar.Query as QQ
import SlamData.Workspace.Card.Eval.CardEvalT as CET
import SlamData.Workspace.Card.Port as Port

import Utils.Path as PU

eval
  ∷ ∀ m
  . (Monad m, QuasarDSL m)
  ⇒ CET.CardEvalInput
  → Maybe String
  → FilePath
  → Maybe Port.VarMap
  → CET.CardEvalT m Port.TaggedResourcePort
eval info mfp resource varMap =
  case mfp of
    Nothing → eval' (CET.temporaryOutputResource info) resource varMap
    Just pt →
      case PU.parseAnyPath pt of
        Just (Right fp) → eval' fp resource varMap
        _ → QE.throw $ pt ⊕ " is not a valid file path"

eval'
  ∷ ∀ m
  . (Monad m, QuasarDSL m)
  ⇒ FilePath
  → FilePath
  → Maybe Port.VarMap
  → CET.CardEvalT m Port.TaggedResourcePort
eval' fp resource varMap = do

  outputResource ← CET.liftQ $
    QQ.fileQuery resource fp "select * from {{path}}" SM.empty

  CET.liftQ $ QFS.messageIfFileNotFound
    outputResource
    "Error saving file, please try another location"

  -- TODO: this error message is pretty obscure. I think it occurs when a query
  -- is like "SELECT * FROM t" and quasar does no work. I'm not sure what the
  -- behaviour of Save should be in that case - perhaps instead of failing it
  -- could create a view so that a resource will actually be created. Debateable
  -- as to whether that is "right", but at least it means a resource will exist
  -- in the expected location, and the rest of the deck can run as the Save
  -- failing has not effect on the output. -gb
  when (fp /= outputResource)
    $ QE.throw
    $ "Resource: " ⊕ Path.printPath outputResource ⊕ " hasn't been modified"
  CET.addSource resource
  CET.addCache outputResource
  pure { resource: outputResource, tag: Nothing, varMap }
