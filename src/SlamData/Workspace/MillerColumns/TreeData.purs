module SlamData.Workspace.MillerColumns.TreeData where

import SlamData.Prelude

import Control.Comonad (extract)
import Control.Comonad.Cofree ((:<))
import Control.Comonad.Cofree as CF

import Data.Foldable (find)

import Data.List ((:))
import Data.List as L

import SlamData.Workspace.MillerColumns.Component.State (State)

type Tree a = CF.Cofree L.List a

loadFromTree
  ∷ ∀ m a i
  . (Eq i, Applicative m)
  ⇒ (a → i)
  → Tree a
  → L.List i
  → m (Maybe (L.List a))
loadFromTree f tree = pure ∘ go tree ∘ L.drop 1 ∘ L.reverse
  where
  go ∷ Tree a → L.List i → Maybe (L.List a)
  go subtree = case _ of
    x : xs →
      case find (\node → f (extract node) == x) (CF.tail subtree) of
        Nothing → Nothing
        Just subtree' → go subtree' xs
    _ →
      let items = extract <$> CF.tail subtree
      in if L.null items then Nothing else Just items

initialStateFromTree ∷ ∀ a i. (a → i) → Tree a → State a i
initialStateFromTree f tree =
  { element: Nothing
  , columns: [Tuple (f (extract tree)) (extract <$> CF.tail tree)]
  , cycle: 0
  , selected: L.Nil
  }
