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

module SlamData.Workspace.Card.Setups.DimensionPicker.JCursor where

import SlamData.Prelude

import Control.Comonad.Cofree as CF

import Data.Argonaut as J
import Data.List as L

import SlamData.Workspace.MillerColumns.TreeData (constructTree)
import SlamData.Workspace.Card.Setups.DimensionPicker.Node (discriminateNodes)

type JCursorNode = Either J.JCursor J.JCursor

groupJCursors ∷ L.List J.JCursor → CF.Cofree L.List JCursorNode
groupJCursors cs =
  let
    tree = constructTree unfoldJCursor J.JCursorTop cs
  in
    discriminateNodes
      if L.null (CF.tail tree)
      then CF.mkCofree J.JCursorTop (pure (CF.mkCofree J.JCursorTop L.Nil))
      else tree

unfoldJCursor ∷ J.JCursor → Maybe (Tuple J.JCursor J.JCursor)
unfoldJCursor cur = case J.insideOut cur of
  J.JCursorTop → Nothing
  J.JField label rest → Just (Tuple cur (J.insideOut rest))
  J.JIndex index rest → Just (Tuple cur (J.insideOut rest))

flattenJCursors ∷ JCursorNode → J.JCursor
flattenJCursors = either id id
