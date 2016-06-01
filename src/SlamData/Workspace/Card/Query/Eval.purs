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

module SlamData.Workspace.Card.Query.Eval
  ( queryEval
  , querySetup
  ) where

import SlamData.Prelude

import Control.Monad.Maybe.Trans (MaybeT(..), runMaybeT)

import Data.Path.Pathy as Path
import Data.String as Str
import Data.StrMap as SM
import Data.Lens (_Just, (^?))

import Ace.Halogen.Component as Ace
import Ace.Types (Completion)

import Halogen (query, action, request, fromEff)

import SlamData.Workspace.Card.Ace.Component (AceDSL)
import SlamData.Workspace.Card.Common.EvalQuery as CEQ
import SlamData.Workspace.Card.Eval as Eval
import SlamData.Workspace.Card.Port as Port

import Utils.Ace (readOnly)
import Utils.Completions (mkCompletion, pathCompletions)


-- TODO: remove this -js
queryEval ∷ CEQ.CardEvalInput → String → AceDSL CEQ.CardEvalResult
queryEval info sql = do
  addCompletions $ fromMaybe SM.empty $ info.input ^? _Just ∘ Port._VarMap
  Eval.runEvalCard info (Eval.Query sql)

querySetup ∷ CEQ.CardSetupInfo → AceDSL Unit
querySetup { input, path } =
  case input of
    Port.VarMap varMap →
      addCompletions varMap

    Port.TaggedResource {resource} → void $ runMaybeT do
      resParent ← MaybeT $ pure $ Path.parentDir resource

      let
        path' = if path ≡ pure resParent
                  then Path.runFileName (Path.fileName resource)
                  else Path.printPath resource
      editor ←
        (MaybeT $ query unit $ request Ace.GetEditor)
        >>= (MaybeT ∘ pure)

      MaybeT
        $ query unit
        $ action
        $ Ace.SetText ("SELECT  *  FROM `" ⊕ path' ⊕ "` ")

      lift $ fromEff do
        readOnly editor
          { startRow: 0
          , startColumn: 0
          , endRow: 0
          , endColumn: 7
          }
        readOnly editor
          { startRow: 0
          , startColumn: 10
          , endRow: 0
          , endColumn: 19 + Str.length path'
          }
    _ → pure unit

addCompletions ∷ ∀ a. SM.StrMap a → AceDSL Unit
addCompletions vm =
  void $ query unit $ action $ Ace.SetCompleteFn \_ _ _ inp → do
    let compl = varMapCompletions vm
    paths ← pathCompletions
    pure $ compl ⊕ paths

  where
  varMapCompletions ∷ SM.StrMap a → Array Completion
  varMapCompletions strMap =
    SM.keys strMap <#> mkCompletion "variable" (Just ∘ append ":")
