module SlamData.Workspace.Card.PivotTable.Pivot
  ( Pivot
  , Dimension
  , classify
  , classify'
  , fold
  , Spine(..)
  , unSpine
  , spine
  ) where

import SlamData.Prelude hiding (fold)
import Data.Unfoldable (class Unfoldable)
import Data.Map (Map)
import Data.Map as Map
import Data.List (List(..), (:))
import Data.List as List

data Pivot k (f ∷ * → *) a
  = Group (Map k (Pivot k f a))
  | Bucket (f a)

type Dimension a k = a → Maybe k

classify
  ∷ ∀ k f a
  . (Foldable f, Unfoldable f, Ord k)
  ⇒ List (Dimension a k)
  → f a
  → Pivot k f a
classify Nil rs = Bucket rs
classify (key : ds) rs =
  Group
    (map (classify ds <<< List.toUnfoldable <<< List.reverse)
      (foldl (updateIndex key) Map.empty
        rs))

classify'
  ∷ ∀ k f a
  . (Foldable f, Unfoldable f, Monoid (f a), Ord k)
  ⇒ List (Dimension a k)
  → f a
  → Pivot k f a
  → Maybe (Pivot k f a)
classify' ds rs p =
  case ds, p of
    Nil, Group _        → Nothing
    Nil, Bucket rs'     → Just (Bucket (rs' <> rs))
    key : ds', Bucket _ → Nothing
    key : ds', Group m  →
      map Group
        (foldl (go ds') (pure m)
          (Map.toList
            (foldl (updateIndex key) Map.empty
              rs)))
  where
  go _ Nothing _ = Nothing
  go ds' (Just m) (Tuple k rs') =
    let
      rs'' = List.toUnfoldable (List.reverse rs')
    in case Map.lookup k m of
      Just p →
        case classify' ds' rs'' p of
          Just p' → pure (Map.insert k p' m)
          Nothing → Nothing
      Nothing →
        pure
          (Map.insert k
            (classify ds' rs'') m)

updateIndex
  ∷ ∀ a k
  . Ord k
  ⇒ (a → Maybe k)
  → Map k (List a)
  → a
  → Map k (List a)
updateIndex f m r =
  case f r of
    Just k  → Map.alter (insert r) k m
    Nothing → m
  where
  insert a r =
    case r of
      Just as → Just (a : as)
      Nothing → Just (List.singleton a)

fold
  ∷ ∀ k f a r
  . (f a → r)
  → (Map k r → r)
  → Pivot k f a
  → r
fold f g (Bucket as) = f as
fold f g (Group m) = g (fold f g <$> m)

newtype Spine k a =
  Spine (a × (Maybe (Map k (Spine k a))))

unSpine ∷ ∀ k a. Spine k a → a × Maybe (Map k (Spine k a))
unSpine (Spine t) = t

spine
  ∷ ∀ k f a b c
  . Monoid b
  ⇒ (f a → b × c)
  → (Map k c → c)
  → Pivot k f a
  → b × (Spine k c)
spine f g = fold f' g'
  where
  f' as = map (\a → Spine (Tuple a Nothing)) (f as)
  g' m  =
    let
      bs = foldMap fst m
      cs = map snd m
      c' = g (map (fst <<< unSpine) cs)
    in
      bs × (Spine (c' × (Just cs)))
