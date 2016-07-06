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
  ) where

import SlamData.Prelude

import Data.StrMap as SM
import Data.Lens (_Just, _Nothing, (^?), (.~))
import Data.Path.Pathy as Pt
import Data.String as Str

import Ace.Editor as Editor
import Ace.Halogen.Component as Ace
import Ace.Types (Completion)

import Halogen (query, action, gets, request, fromEff, modify)

import SlamData.Workspace.Card.Component as CC
import SlamData.Workspace.Card.Ace.Component as AceCard
import SlamData.Workspace.Card.Ace.Component.State (Status(..), _status, isNew)
import SlamData.Workspace.Card.Eval.CardEvalT as CET
import SlamData.Workspace.Card.Port as Port

import Utils.Ace (readOnly)
import Utils.Completions (mkCompletion, pathCompletions)

queryEval ∷ CET.CardEvalInput → AceCard.DSL Unit
queryEval info = do
  status ← gets _.status
  when (isNew status) do
    mbEditor ←
      map join $ query unit $ request Ace.GetEditor

    for_ (info.input ^? _Just ∘ Port._VarMap) \vm → do
      addCompletions vm
      for_ mbEditor setSelectEmpty

    for_ (info.input ^? _Just ∘ Port._Resource) \r → do
      let resParent = Pt.parentDir r
          strPath =
            if pure info.path ≡ resParent
              then Pt.runFileName (Pt.fileName r)
              else Pt.printPath r

      query unit $ action $ Ace.SetText (" SELECT  *  FROM `" ⊕ strPath ⊕ "` ")

      for_ mbEditor \editor → fromEff do
        readOnly editor
          { startRow: 0
          , startColumn: 0
          , endRow: 0
          , endColumn: 8
          }
        readOnly editor
          { startRow: 0
          , startColumn: 11
          , endRow: 0
          , endColumn: 20 + Str.length strPath
          }
        Editor.navigateFileEnd editor

    for_ (info.input ^? _Nothing) \_ →
      for_ mbEditor setSelectEmpty

    modify $ _status .~ Ready
    CC.raiseUpdatedP CC.EvalModelUpdate
  where
  setSelectEmpty editor = do
    void $ query unit $ action $ Ace.SetText ("SELECT \"Hello World!\"")
    fromEff $ Editor.navigateFileEnd editor

addCompletions ∷ ∀ a. SM.StrMap a → AceCard.DSL Unit
addCompletions vm =
  void $ query unit $ action $ Ace.SetCompleteFn \_ _ _ inp → do
    let compl = varMapCompletions vm
    paths ← pathCompletions
    pure $ compl ⊕ paths

  where
  varMapCompletions ∷ SM.StrMap a → Array Completion
  varMapCompletions strMap =
    SM.keys strMap <#> mkCompletion "variable" (Just ∘ append ":")
