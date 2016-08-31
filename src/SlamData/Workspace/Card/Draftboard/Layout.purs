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

module SlamData.Workspace.Card.Draftboard.Layout
  ( SplitBias(..)
  , Rect
  , Vect
  , Cell
  , Edge
  , locatorToRect
  , eqEdge
  , rectAt
  , cells
  , edges
  , findSplit
  , insertSplit
  , insertRootSplit
  , resizeEdge
  , resizeEdge'
  , deleteCell
  , absoluteRect
  , absoluteVect
  , absoluteCells
  , absoluteEdges
  , closestSnapRatio
  , defaultMerge
  ) where

import SlamData.Prelude
import Data.List (List(..), (:))
import Data.List as List
import Data.Rational (Rational, (%))
import Data.Rational as Rational
import Math (floor, abs)

import SlamData.Workspace.Card.Draftboard.Orientation (Orientation(..), reverse)
import SlamData.Workspace.Card.Draftboard.Pane (Pane(..), Locator(..), Cursor, runCursor, walkWithLocator, getAtWithLocator, wrap)

data SplitBias
  = SideA
  | SideB

type Rect a =
  { top ∷ a
  , left ∷ a
  , width ∷ a
  , height ∷ a
  }

type Vect a =
  { x ∷ a
  , y ∷ a
  , z ∷ a
  }

type Cell a b =
  { cursor ∷ Cursor
  , value ∷ a
  , rect ∷ Rect b
  }

type Edge a =
  { index ∷ Int
  , parent ∷ Cursor
  , orientation ∷ Orientation
  , ratio ∷ Rational
  , vect ∷ Vect a
  }

locatorToRect ∷ Locator → Rect Rational
locatorToRect (Locator _ left top width height _ _) =
  { top, left, width, height }

-- | Edges are semantically equal if their cursor and index are equal.
eqEdge ∷ ∀ a b. Edge a → Edge b → Boolean
eqEdge a b = a.parent == b.parent && a.index == b.index

-- | Calculates the rectangle for a given Cursor.
rectAt ∷ ∀ a . Cursor → Pane a → Maybe (Rect Rational)
rectAt cursor = map (locatorToRect <<< fst) <<< getAtWithLocator cursor

-- | Collects all cells with their computed locations
cells ∷ ∀ a . Pane a → List (Cell a Rational)
cells = walkWithLocator go Nil
  where
  go (Locator cursor left top width height _ _) cs (Cell value) =
    { cursor
    , value
    , rect: { top, left, width, height }
    } : cs
  go _ cs _ = cs

-- | Collects all the computed edges with their vectors. This does not include
-- | the edges for the root.
edges ∷ ∀ a . Pane a → List (Edge Rational)
edges = walkWithLocator go Nil
  where
  go _ es (Cell _) = es
  go (Locator c x y w h _ _) es (Split orn panes) =
    goEdges x y zero 0 Nil panes <> es

    where
    goEdges x y off ix acc ps =
      let
        edge = case ps of
          (r × p) : (r' × p') : _ →
            let
              vect = case orn of
                Horizontal → { x: x + r * w, y, z: h }
                Vertical   → { x, y: y + r * h, z: w }
              in
                List.singleton
                  { index: ix
                  , parent: c
                  , orientation: orn
                  , ratio: off + r
                  , vect
                  }
          _ → Nil
      in
        case ps of
          Nil → acc
          (r × p) : ps' →
            case orn of
              Horizontal →
                goEdges (r * w + x) y (off + r) (ix + 1) (edge <> acc) ps'
              Vertical →
                goEdges x (r * h + y) (off + r) (ix + 1) (edge <> acc) ps'

-- | Finds the deepest split for a given orientation and cursor.
findSplit
  ∷ ∀ a
  . Orientation
  → Cursor
  → Pane a
  → Maybe Cursor
findSplit orn cursor =
  map (List.reverse) <=< runCursor
    (case _ of
      Split orn' _ | orn' == orn → Just Nil
      _ → Nothing)
    (\orn' _ ix r →
      case r of
        Just c → Just (ix : c)
        _ | orn' == orn → Just Nil
        _ → Nothing)
    cursor

-- | Inserts a new split into a Pane at a given ratio. If the Cursor points to
-- | a Split, then the Pane at that ratio will be resized appropriately.
insertSplit
  ∷ ∀ a
  . Pane a
  → Orientation
  → Rational
  → SplitBias
  → Cursor
  → Pane a
  → Maybe (Pane a)
insertSplit pane orn ratio side = runCursor split defaultMerge
  where
  split p = Split orn case p of
    -- When splitting an existing Split of a given orientation, we need to
    -- split the Pane that is at the given ratio.
    Split orn' ps | orn' == orn →
      goSplit (\off r p' → scale r (splitAt ((ratio - off) / r) p' pane)) zero Nil ps
    _ →
      splitAt ratio p pane

  splitAt ∷ Rational → Pane a → Pane a → List (Rational × Pane a)
  splitAt r cell1 cell2 =
    case side of
      SideA → scaleOrWrap r cell2 <> scaleOrWrap (one - r) cell1
      SideB → scaleOrWrap r cell1 <> scaleOrWrap (one - r) cell2
            
  scaleOrWrap ∷ Rational → Pane a → List (Rational × Pane a)
  scaleOrWrap r p = case p of
    -- Cells need to be wrapped in a single Split of the opposite orientation.
    -- This ensures we can keep alternating splits.
    Cell a → List.singleton (r × wrap (reverse orn) p)
    -- If a Split has the same orientation, we want to assimilate into the
    -- existing split, so we should just scale as needed.
    Split orn' ps | orn' == orn → scale r ps
    -- If a Split has the opposite orientation, we just nest.
    _ → List.singleton (r × p)

  goSplit splice off pre Nil = List.reverse pre
  goSplit splice off pre (t@(r × p) : ps)
    | ratio >= off + r || ratio == off =
        goSplit splice (off + r) (t : pre) ps
    | otherwise =
        List.reverse pre
        <> splice off r p
        <> ps

insertRootSplit
  ∷ ∀ a
  . Pane a
  → Orientation
  → Rational
  → SplitBias
  → Pane a
  → Pane a
insertRootSplit pane orn ratio side p =
  let
    ratio' = one - ratio
  in Split orn case side of
    SideA → assimilateOne orn ratio pane <> assimilateOne orn ratio' p
    SideB → assimilateOne orn ratio p <> assimilateOne orn ratio' pane

-- | Changes the location of an edge within a Split. If the new location is
-- | passed the edges of adjacent Panes, then all Panes to that side will be
-- | scaled using the provided scaling function.
resizeEdge
  ∷ ∀ a
  . (Rational → List (Rational × Pane a) → List (Rational × Pane a))
  → Int
  → Rational
  → Cursor
  → Pane a
  → Maybe (Pane a)
resizeEdge scaleFn edge offset' cursor = map compressRoot ∘ join ∘ runCursor resize merge cursor
  where
  resize p = case p of
    Cell a → Nothing
    Split orn ps →
      Split orn ∘ resize' ps <$> goEdge 0 zero Nil ps

  resize' ps { offset, pre, post, a: r1 × p1, b: r2 × p2 } =
    case compare offset' offset of
      EQ → ps
      LT | offset - offset' >= r1 →
        let
          ratio = offset' / offset
          cell  = r2 + (offset - offset') × p2
        in
          if offset' == zero
            then cell : post
            else scaleFn ratio (List.snoc pre (r1 × p1)) <> (cell : post)
      LT →
        let
          cell1 = (r1 - (offset - offset')) × p1
          cell2 = (r2 + (offset - offset')) × p2
        in
          pre <> (cell1 : cell2 : post)
      GT | offset' - offset >= r2 →
        let
          ratio = (one - offset') / (one - offset)
          cell  = (r1 + offset' - offset) × p1
        in
          if offset' == one
            then List.snoc pre cell
            else pre <> (cell : scaleFn ratio ((r2 × p2) : post))
      GT →
        let
          cell1 = (r1 + (offset' - offset)) × p1
          cell2 = (r2 - (offset' - offset)) × p2
        in
          pre <> (cell1 : cell2 : post)

  merge orn ps ix p =
    map (defaultMerge orn ps ix) p

  goEdge i off pre (a@(r1 × p1) : b@(r2 × p2) : post)
    | edge == i =
        Just { offset: off + r1, pre: List.reverse pre, post, a, b }
    | otherwise =
        goEdge (i + 1) (off + r1) (a : pre) (b : post)
  goEdge _ _ _ _ = Nothing

-- | Scales with a default relative scaling function.
resizeEdge' ∷ ∀ a . Int → Rational → Cursor → Pane a → Maybe (Pane a)
resizeEdge' = resizeEdge scale

deleteCell ∷ ∀ a. Cursor → Pane a → Maybe (Pane a)
deleteCell cursor = map compressRoot ∘ join ∘ runCursor (const Nothing) merge cursor
  where
  merge orn ps ix = case _ of
    Nothing →
      collapse orn =<< goIx 0 ix Nil ps
    Just p →
      Just (defaultMerge orn ps ix p)

  collapse orn p =
    let
      ps = case p of
        r × Nil × Nil →
          Nothing
        r × Nil × ((r2 × p2) : post) →
          Just (((r2 + r) × p2) : post)
        r × ((r1 × p1) : pre) × Nil →
          Just (List.reverse (((r1 + r) × p1) : pre))
        r × ((r1 × p1) : pre) × ((r2 × p2) : post) →
          Just (List.reverse (((r1 + r * (1%2)) × p1) : pre)
            <> (((r2 + r * (1%2)) × p2) : post))
    in ps <#>
      case _ of
        (r × (Split orn' ps')) : Nil →
          Split orn' ps'
        ps' →
          Split orn ps'

  goIx i ix pre Nil = Nothing
  goIx i ix pre ((r × _) : post) | i == ix = Just (r × pre × post)
  goIx i ix pre (p : post) = goIx (i + 1) ix (p : pre) post

absoluteRect
  ∷ ∀ r
  . { width ∷ Number, height ∷ Number | r }
  → Rect Rational
  → Rect Number
absoluteRect s r =
  let
    top = floor (Rational.toNumber r.top * s.height)
    left = floor (Rational.toNumber r.left * s.width)
    width =
      if r.left + r.width == one
        then s.width - left
        else floor (Rational.toNumber r.width * s.width)
    height =
      if r.top + r.height == one
        then s.height - top
        else floor (Rational.toNumber r.height * s.height)
  in
    { top, left, width, height }

absoluteVect
  ∷ ∀ r
  . { width ∷ Number, height ∷ Number | r }
  → Orientation
  → Vect Rational
  → Vect Number
absoluteVect s orn v =
  let
    x = floor (Rational.toNumber v.x * s.width)
    y = floor (Rational.toNumber v.y * s.height)
    z =
      case orn of
        Horizontal →
          if v.y + v.z == one
            then s.height - y
            else floor (Rational.toNumber v.z * s.height)
        Vertical →
          if v.x + v.z == one
            then s.width - x
            else floor (Rational.toNumber v.z * s.width)
  in
    { x, y, z }

absoluteCells
  ∷ ∀ a r
  . { width ∷ Number, height ∷ Number | r }
  → List (Cell a Rational)
  → List (Cell a Number)
absoluteCells s =
  map (\c → c { rect = absoluteRect s c.rect })

absoluteEdges
  ∷ ∀ r
  . { width ∷ Number, height ∷ Number | r }
  → List (Edge Rational)
  → List (Edge Number)
absoluteEdges s =
  map (\e → e { vect = absoluteVect s e.orientation e.vect })

ratios ∷ List Rational
ratios =
  List.sort $ List.nub do
    d ← List.fromFoldable [ 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 12, 16 ]
    n ← List.range 0 d
    pure (n%d)

closestSnapRatio ∷ Number → Rational
closestSnapRatio r = findClosest (\b → Rational.toNumber b - r) one ratios

findClosest ∷ ∀ f a. Foldable f ⇒ (a → Number) → a → f a → a
findClosest f = foldl go
  where
  go a b
    | abs (f b) < abs (f a) = b
    | otherwise = a

scale ∷ ∀ f a. Functor f ⇒ Rational → f (Rational × a) → f (Rational × a)
scale n = map (lmap (n * _))

spliceAt ∷ ∀ a. (a → List a) → Int → List a → Maybe (List a)
spliceAt f = go Nil 0
  where
  go pre i ix (x : xs)
    | i == ix = Just (List.reverse pre <> f x <> xs)
    | otherwise = go (x : pre) (i + 1) ix xs
  go _ _ _ _ = Nothing

defaultMerge
  ∷ ∀ a
  . Orientation
  → List (Rational × Pane a)
  → Int
  → Pane a
  → Pane a
defaultMerge orn ps ix p =
  case p of
    Cell a →
      case ps of
        Nil →
          Split orn (List.singleton (one × p))
        _ : Nil →
          Split orn (List.singleton (one × p))
        _ →
          Split orn (updateSndAt (wrap (reverse orn) p) ix ps)
    Split orn' ps' | orn /= orn' →
      case ps' of
        (_ × Split _ ps'') : Nil →
          Split orn (assimilated ps'')
        _ →
          case ps of
            (_ × p') : Nil →
              p'
            _ →
              Split orn (updateSndAt p ix ps)
    Split orn' ps' →
      Split orn (assimilated ps')

  where
  assimilated ps' =
    fromMaybe ps (spliceAt (\(r × _) → assimilate orn r ps') ix ps)

assimilateOne
  ∷ ∀ a
  . Orientation
  → Rational
  → Pane a
  → List (Rational × Pane a)
assimilateOne orn r p =
  assimilate orn r (List.singleton (one × p))

assimilate
  ∷ ∀ a
  . Orientation
  → Rational
  → List (Rational × Pane a)
  → List (Rational × Pane a)
assimilate orn r ps = do
  r' × p ← ps
  case p of
    Cell _ →
      List.singleton (r * r' × wrap (reverse orn) p)
    Split orn' ps' | orn /= orn' →
      List.singleton (r * r' × compress p)
    Split orn' ps' →
      assimilate orn (r * r') ps'

compress ∷ ∀ a. Pane a → Pane a
compress p =
  case p of
    Cell a →
      p
    Split orn ps → Split orn
      case ps of
        (_ × Cell a') : Nil →
          ps
        _ →
          assimilate orn one ps

compressRoot ∷ ∀ a. Pane a → Pane a
compressRoot p =
  case p of
    Split _ ((_ × (Split _ ((_ × (Cell a)) : Nil))) : Nil) → Cell a
    Split _ ((_ × (Cell a)) : Nil) → Cell a
    _ → p

modifySndAt ∷ ∀ a b. (b → b) → Int → List (a × b) → List (a × b)
modifySndAt f ix l = fromMaybe l (List.modifyAt ix (map f) l)

updateSndAt ∷ ∀ a b. b → Int → List (a × b) → List (a × b)
updateSndAt = modifySndAt ∘ const
