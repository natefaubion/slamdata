module SlamData.Workspace.MillerColumns.Component.Query where

import SlamData.Prelude

import Data.List (List, (:))

import DOM.HTML.Types (HTMLElement)

data Query i b
  = Ref (Maybe HTMLElement) b
  | Extended b
  | Populate (List i) b
