module SlamData.Workspace.MillerColumns.Component
  ( ItemSpec
  , ItemHTML
  , component
  , module SlamData.Workspace.MillerColumns.Component.Query
  , module SlamData.Workspace.MillerColumns.Component.State
  ) where

import SlamData.Prelude

import Control.Monad.Aff.Free (class Affable)
import Control.Monad.Eff (Eff)

import Data.Array as A
import Data.Traversable (scanr)
import Data.List ((:))
import Data.List as L
import Data.Unfoldable (replicate)

import DOM (DOM)
import DOM.HTML.Types (HTMLElement, htmlElementToElement)
import DOM.Node.Element (scrollWidth, setScrollLeft) as DOM
import DOM.HTML.HTMLElement (offsetWidth) as DOM

import Halogen as H
import Halogen.HTML.Events.Indexed as HE
import Halogen.HTML.Indexed as HH
import Halogen.HTML.Properties.Indexed as HP
import Halogen.HTML.Properties.Indexed.ARIA as ARIA

import SlamData.Workspace.MillerColumns.Component.Query (Query(..))
import SlamData.Workspace.MillerColumns.Component.State (State, initialState)

type ItemSpec a i m =
  { label ∷ a → String
  , render ∷ a → H.ComponentHTML (Const Void)
  , load ∷ L.List i → m (Maybe (L.List a))
  , id ∷ a → i
  }

type HTML i = H.ComponentHTML (Query i)
type DSL a i m p = H.ComponentDSL (State a i) (Query i) m p

type ItemHTML = H.ComponentHTML (Const Void)

type RenderRec i = { path ∷ L.List i, html ∷ Array (HTML i) }

component
  ∷ ∀ eff a i m
  . (Affable (dom ∷ DOM | eff) m, MonadPar m, Eq i)
  ⇒ ItemSpec a i m
  → Maybe (L.List i)
  → H.Component (State a i) (Query i) m
component ispec initial =
  H.lifecycleComponent
    { render
    , eval
    , initializer: map (H.action ∘ Populate) initial
    , finalizer: Nothing
    }
  where

  render ∷ State a i → HTML i
  render { columns, selected } =
    let
      -- Drop the root item from `selected` (since there's no corresponding
      -- rendered item), and reverse it to produce a list rather than a stack,
      -- so the steps can be zipped with the columns.
      selectSteps = A.reverse (A.fromFoldable (fromMaybe L.Nil (L.init selected)))
    in
      HH.div
        [ HP.class_ (HH.className "sd-miller-columns")
        , HP.ref (H.action ∘ Ref)
        ]
        $ _.html
        $ foldl goColumn
            { path: L.Nil, html: [] }
            (A.zip columns (pad selectSteps (A.length columns)))

  goColumn
    ∷ RenderRec i
    → Tuple (Tuple i (L.List a)) (Maybe i)
    → RenderRec i
  goColumn acc (Tuple (Tuple item children) sel) =
    let path = item : acc.path
    in { path, html: acc.html <> [renderColumn path children sel] }

  renderColumn ∷ L.List i → L.List a → Maybe i → HTML i
  renderColumn colPath items selected =
    HH.ul
      [ HP.class_ (HH.className "sd-miller-column")
      , HP.ref (\_ → H.action Extended)
      ]
      $ A.fromFoldable (renderItem colPath selected <$> items)

  renderItem ∷ L.List i → Maybe i → a → HTML i
  renderItem colPath selected item =
    let label = ispec.label item
    in
      HH.li
        [ HE.onClick $ HE.input_ $ Populate (ispec.id item : colPath)
        , HP.title label
        , ARIA.label label
        , HP.classes
            if Just (ispec.id item) == selected
            then [ HH.className "selected" ]
            else []
        ]
        [ absurd ∘ getConst <$> ispec.render item ]

  eval ∷ Query i ~> DSL a i m
  eval (Ref mel next) = do
    H.modify (_ { element = mel })
    pure next
  eval (Extended next) = do
    traverse_ (H.fromEff ∘ scrollToRight) =<< H.gets _.element
    pure next
  eval (Populate path next) = do
    currentPath <- H.gets _.selected

    let
      prefix = findPathPrefix currentPath path
      remainder = L.take (L.length path - L.length prefix) path
      remPaths = scanr (:) prefix remainder

    H.modify \st → st
      { cycle = st.cycle + 1
      , columns = A.take (L.length prefix) st.columns
      , selected = L.take (L.length prefix + 1) path
      }

    currentCycle ← H.gets _.cycle
    more ← H.liftH (parTraverse ispec.load remPaths)

    -- `load` is async, so in case multiple `Populate`s have been raised we need
    -- to check that we still care about the results of the load we just
    -- triggered.
    --
    -- The "cycle" counter is incremented every time a `load` is triggered, so
    -- if it differs after a `load` completes we know a competing `load` has
    -- been triggered in the mean time, and we can just abandon this result.
    --
    -- TODO: use fork instead so we can just cancel?
    currentCycle' ← H.gets _.cycle
    when (currentCycle == currentCycle') do
      let
        newColumns = foldr go [] (L.zip remainder more)
        go (Tuple item colItems) cols =
          maybe cols (\items -> cols <> [Tuple item items]) colItems
      H.modify \st → st
        { columns = st.columns <> newColumns
        , selected = path
        }

    pure next

findPathPrefix ∷ ∀ a. Eq a ⇒ L.List a → L.List a → L.List a
findPathPrefix xs ys =
  let
    lxs = L.length xs
    lys = L.length ys
    xs' = if lxs > lys then L.drop (lxs - lys) xs else xs
    ys' = if lys > lxs then L.drop (lys - lxs) ys else ys
  in
    snd $ foldr go (Tuple true L.Nil) (L.zip xs' ys')
  where
  go (Tuple x y) (Tuple matching acc)
    | matching && x == y = Tuple true (x : acc)
    | otherwise = Tuple false acc

pad ∷ ∀ a. Array a → Int → Array (Maybe a)
pad xs length = map Just xs <> replicate (length - A.length xs) Nothing

scrollToRight ∷ ∀ eff. HTMLElement → Eff (dom ∷ DOM | eff) Unit
scrollToRight hel = do
  let el = htmlElementToElement hel
  maxScroll ← (-) <$> DOM.scrollWidth el <*> DOM.offsetWidth hel
  DOM.setScrollLeft maxScroll el
