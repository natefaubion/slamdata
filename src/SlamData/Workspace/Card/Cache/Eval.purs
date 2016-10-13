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

import SlamData.Quasar.FS as QFS
import SlamData.Quasar.Query as QQ
import SlamData.Workspace.Card.Eval.Monad as CEM
import SlamData.Workspace.Card.Eval.Transition (CardEvalInput)
import SlamData.Workspace.Card.Port as Port

import Utils.Path as PU

eval
  ∷ CardEvalInput
  → Maybe String
  → FilePath
  → Maybe Port.VarMap
  → CEM.CardEval Port.TaggedResourcePort
eval info mfp resource varMap =
  case mfp of
    Nothing → eval' (CEM.temporaryOutputResource info) resource varMap
    Just pt →
      case PU.parseAnyPath pt of
        Just (Right fp) → eval' fp resource varMap
        _ → CEM.throw $ pt ⊕ " is not a valid file path"

eval'
  ∷ FilePath
  → FilePath
  → Maybe Port.VarMap
  → CEM.CardEval Port.TaggedResourcePort
eval' fp resource varMap = do

  outputResource ← CEM.liftQ $
    QQ.fileQuery resource fp "select * from {{path}}" SM.empty

  CEM.liftQ $ QFS.messageIfFileNotFound
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
    $ CEM.throw
    $ "Resource: " ⊕ Path.printPath outputResource ⊕ " hasn't been modified"
  CEM.addSource resource
  CEM.addCache outputResource
  pure
    { resource: outputResource
    , tag: Nothing
    , varMap
    }
