module SlamData.Workspace.MillerColumns.Component.State where

import SlamData.Prelude

import Data.List (List(..), (:))

import DOM.HTML.Types (HTMLElement)

type State a i =
  { element ∷ Maybe HTMLElement
  , columns ∷ Array (Tuple i (List a))
  , selected ∷ List i
  , cycle ∷ Int
  }

initialState ∷ ∀ a i. State a i
initialState =
  { element: Nothing
  , columns: []
  , selected: Nil
  , cycle: 0
  }
