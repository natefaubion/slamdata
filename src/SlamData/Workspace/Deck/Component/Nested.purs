module SlamData.Workspace.Deck.Component.Nested
  ( comp
  , module DNQ
  , module DNS
  ) where

import SlamData.Prelude

import Control.Coroutine.Stalling as SCR
import Control.Monad.Aff (runAff)
import Control.Monad.Aff.AVar (makeVar, putVar, takeVar)
import Control.Monad.Eff (Eff)

import Data.Maybe.Unsafe (fromJust)

import Halogen as H
import Halogen.Component.Opaque.Unsafe (opaque, opaqueQuery)
import Halogen.HTML.Indexed as HH
import Halogen.HTML.Properties.Indexed as HP
import Halogen.Query.EventSource as HE

import SlamData.Effects (SlamDataEffects, Slam)
import SlamData.Workspace.Deck.Component.Nested.Query as DNQ
import SlamData.Workspace.Deck.Component.Nested.State as DNS
import SlamData.Workspace.Deck.Component as DC
import SlamData.Workspace.Deck.Component.Query as DCQ
import SlamData.Workspace.Deck.Component.State as DCS
import SlamData.Workspace.Wiring (Wiring)

import Utils.AffableProducer (produce)

type DSL = H.ComponentDSL DNS.State DNQ.QueryP Slam
type HTML = H.ComponentHTML DNQ.QueryP

comp ∷ Wiring → DCS.StateP → H.Component DNS.State DNQ.QueryP Slam
comp wiring deckState =
  H.lifecycleComponent
    { render
    , eval: coproduct eval evalProxy
    , initializer: Just (left $ H.action DNQ.Init)
    , finalizer: Just (left $ H.action DNQ.Finish)
    }

  where
  deckComponent' ∷ (DCQ.Query Unit → Eff SlamDataEffects Unit) → H.Component DCS.StateP DCQ.QueryP Slam
  deckComponent' emitter =
    opaque $ H.lifecycleParentComponent
      { render: DC.render wiring (comp wiring)
      , eval: \query → do
          res ← DC.eval wiring query
          case query of
            DCQ.DoAction a _ → H.fromEff $ emitter $ DCQ.DoAction a unit
            DCQ.GrabDeck a _ → H.fromEff $ emitter $ DCQ.GrabDeck a unit
            DCQ.ResizeDeck a _ → H.fromEff $ emitter $ DCQ.ResizeDeck a unit
            _ → pure unit
          pure res
      , peek: Just (DC.peek wiring)
      , initializer: Just (H.action DCQ.Init)
      , finalizer: Nothing
      }

  render ∷ DNS.State → HTML
  render _ =
    HH.div
      [ HP.ref (left ∘ H.action ∘ DNQ.Ref) ]
      []

  eval ∷ DNQ.Query ~> DSL
  eval = case _ of
    DNQ.Init next → do
      el ← fromJust <$> H.gets _.el
      emitter ← H.fromAff makeVar
      H.subscribe $
        HE.EventSource $
          SCR.producerToStallingProducer $ produce \emit →
            runAff (const (pure unit)) (const (pure unit)) $
              putVar emitter (emit <<< Left)
      emitter' ← H.fromAff $ takeVar emitter
      driver ← H.fromAff $ H.runUI (deckComponent' (emitter' ∘ right)) deckState el
      H.modify _ { driver = Just (DNS.Driver driver) }
      pure next
    DNQ.Finish next → do
      DNS.Driver driver ← fromJust <$> H.gets _.driver
      H.fromAff $ driver $ opaqueQuery $ DCQ.Finish next
    DNQ.Ref el next → do
      H.modify _ { el = el }
      pure next

  evalProxy ∷ DCQ.Query ~> DSL
  evalProxy = case _ of
    -- These are the actions that are peeked, so they are terminal.
    DCQ.DoAction _ next → pure next
    DCQ.GrabDeck _ next → pure next
    DCQ.ResizeDeck _ next → pure next
    -- The rest we'll just pass through
    query → do
      DNS.Driver driver ← fromJust <$> H.gets _.driver
      H.fromAff $ driver $ opaqueQuery query
