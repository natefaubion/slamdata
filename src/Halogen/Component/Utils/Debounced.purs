module Halogen.Component.Utils.Debounced
  ( module Utils.Debounced
  , DebounceTrigger
  , fireDebouncedQuery'
  ) where

import Prelude
import Control.Monad.Aff (Aff)
import Data.Lens (LensP, view, (?~))
import Data.Maybe (Maybe(..))
import Data.Time (Milliseconds)
import Halogen as H
import Utils.Debounced (debouncedEventSource, DebounceEffects)

type DebounceTrigger f eff = f Unit → Aff eff Unit

-- | Fires the specified debouced H.query trigger with the passed H.query. This
-- | function also handles constructing the initial trigger if it has not yet
-- | been created.
fireDebouncedQuery'
  ∷ ∀ s s' f f' p eff
  . Milliseconds
  → LensP s (Maybe (DebounceTrigger f (DebounceEffects eff)))
  → H.Action f
  → H.ParentDSL s s' f f' (Aff (DebounceEffects eff)) p Unit
fireDebouncedQuery' ms lens act = do
  t ← H.gets (view lens) >>= \mbt → case mbt of
    Just t' → pure t'
    Nothing → do
      t' ← debouncedEventSource H.fromEff H.subscribe' ms
      H.modify (lens ?~ t')
      pure t'
  H.liftH $ H.liftH $ t $ H.action $ act
