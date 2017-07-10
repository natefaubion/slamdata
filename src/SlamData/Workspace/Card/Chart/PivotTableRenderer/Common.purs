{-
Copyright 2017 SlamData, Inc.

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

module SlamData.Workspace.Card.Chart.PivotTableRenderer.Common where

import SlamData.Prelude

import Data.Argonaut as J
import Data.Array as Array
import Data.Foldable as F
import Data.Int as Int
import Data.List (List, (:))
import Data.List as List
import SlamData.Workspace.Card.Setups.Chart.PivotTable.Model (ColumnDimension)
import SlamData.Workspace.Card.Setups.Dimension as D

topField ∷ String → J.JCursor
topField c = J.JField c J.JCursorTop

calcPageCount ∷ Int → Int → Int
calcPageCount count size =
  Int.ceil (Int.toNumber count / Int.toNumber size)

sizeOfRow ∷ Array (String × ColumnDimension) → J.Json → Int
sizeOfRow columns row =
  fromMaybe 1
    (F.maximum
      (Array.mapMaybe
        case _ of
          c × (_ × D.Dimension _ (D.Projection (Just _) _)) → Just 1
          c × _ → J.foldJsonArray 1 Array.length <$> J.cursorGet (topField c) row
        columns))

data PTree k a
  = Bucket (Array a)
  | Grouped (Array (k × PTree k a))

foldTree
  ∷ ∀ k a r
  . (Array a → r)
  → (Array (k × r) → r)
  → PTree k a
  → r
foldTree f g (Bucket a) = f a
foldTree f g (Grouped as) = g (map (foldTree f g) <$> as)

buildTree
  ∷ ∀ k a r
  . Eq k
  ⇒ List (a → Maybe k)
  → (Array a → r)
  → (Array (k × r) → r)
  → Array a
  → r
buildTree List.Nil f g as = f as
buildTree (k : ks) f g as =
  g (fin (foldl go { key: Nothing, group: [], acc: [] } as))
  where
  go res@{ key: mbKey, group, acc } a =
    case mbKey, k a of
      Just key, Just key' | key == key' →
        { key: mbKey, group: Array.snoc group a, acc }
      _, Just key' →
        { key: Just key', group: [a], acc: fin res }
      _, Nothing →
        res
  fin { key, group, acc } =
    case key of
      Just key' →
        Array.snoc acc (key' × (buildTree ks f g group))
      Nothing →
        acc

pagedTree
  ∷ ∀ k a
  . Int
  → (a → Int)
  → PTree k a
  → Int × Array (PTree k a)
pagedTree page sizeOf tree =
  case tree of
    Bucket as → map Bucket <$> chunked page sizeOf as
    Grouped gs → map Grouped <$> chunked page (sizeOf' ∘ snd) gs
  where
  sizeOf' (Bucket as) = F.sum (sizeOf <$> as)
  sizeOf' (Grouped gs) = F.sum (sizeOf' ∘ snd <$> gs)

chunked
  ∷ ∀ b
  . Int
  → (b → Int)
  → Array b
  → Int × Array (Array b)
chunked page sizeOf arr = res.total × Array.snoc res.chunks res.chunk
  where
  res =
    foldl go { total: 0, size: 0, chunk: [], chunks: []} arr

  go { total, size, chunk, chunks } a =
    let
      s2 = sizeOf a
      t2 = total + s2
    in
      case size + s2, chunk of
        size', [] | size' > page →
          { total: t2, size: 0, chunk, chunks: Array.snoc chunks [a] }
        size', _  | size' > page →
          { total: t2, size: s2, chunk: [a], chunks: Array.snoc chunks chunk }
        size', _ →
          { total: t2, size: size', chunk: Array.snoc chunk a, chunks }
