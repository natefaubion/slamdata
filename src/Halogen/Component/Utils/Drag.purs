module Halogen.Component.Utils.Drag where

import Prelude

import Control.Apply ((*>))
import Control.Bind ((=<<))
import Control.Monad.Aff (Canceler(..), forkAff, launchAff, runAff)
import Control.Monad.Aff.AVar (makeVar, makeVar', takeVar, putVar, AVAR)
import Control.Monad.Aff.Class (class MonadAff)
import Control.Monad.Aff.Free (class Affable, fromAff)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Exception (EXCEPTION)

import DOM (DOM)
import DOM.Event.EventTarget (EventListener, eventListener, addEventListener, removeEventListener)
import DOM.Event.EventTypes (mousemove, mouseup)
import DOM.HTML (window)
import DOM.HTML.Types (windowToEventTarget)

import Halogen as H
import Halogen.HTML.Events.Types (Event, MouseEvent)

import Unsafe.Coerce (unsafeCoerce)

type DragData =
  { x ∷ Number
  , y ∷ Number
  , deltaX ∷ Number
  , deltaY ∷ Number
  , offsetX ∷ Number
  , offsetY ∷ Number
  }

data DragEvent
  = Move (Event MouseEvent) DragData
  | Done (Event MouseEvent)

type DragEffects eff =
  ( dom ∷ DOM
  , avar ∷ AVAR
  , err ∷ EXCEPTION
  | eff
  )

begin
  ∷ ∀ g eff
  . Affable (DragEffects eff) g
  ⇒ Event MouseEvent
  → g { subscription
          ∷ (DragEvent → Eff (DragEffects eff) Unit)
          → Eff (DragEffects eff) Unit
      , canceler
          ∷ Canceler (DragEffects eff)
      }
begin ev = fromAff do
  handler ← makeVar
  remove ← makeVar
  event ← makeVar' ev

  let initX = (unsafeCoerce ev).pageX
      initY = (unsafeCoerce ev).pageY

  forkAff do
    handler' ← takeVar handler

    let remove' ∷ Eff (DragEffects eff) Unit
        remove' = do
          win ← windowToEventTarget <$> window
          removeEventListener mousemove mouseMove false win
          removeEventListener mouseup mouseUp false win

        mouseMove ∷ EventListener (DragEffects eff)
        mouseMove = eventListener \e → runAff (const (pure unit)) (const (pure unit)) do
          event' ← takeVar event
          let x1 = (unsafeCoerce event').pageX
              y1 = (unsafeCoerce event').pageY
              x2 = (unsafeCoerce e).pageX
              y2 = (unsafeCoerce e).pageY
              dragData =
                { x: x2
                , y: y2
                , deltaX: x2 - x1
                , deltaY: y2 - y1
                , offsetX: x2 - initX
                , offsetY: y2 - initY
                }
          putVar event (unsafeCoerce e)
          liftEff $ handler' (Move (unsafeCoerce e) dragData)

        mouseUp ∷ EventListener (DragEffects eff)
        mouseUp = eventListener \e → remove' *> handler' (Done (unsafeCoerce e))

    liftEff do
      win ← windowToEventTarget <$> window
      addEventListener mousemove mouseMove false win
      addEventListener mouseup mouseUp false win

    putVar remove remove'

  let subscription = launchAff <<< putVar handler
      canceler = Canceler \_ → do
        liftEff =<< takeVar remove
        pure true

  pure { subscription, canceler }


subscribe'
  ∷ ∀ s s' f f' g p eff
  . ( Affable (DragEffects eff) g
    , MonadAff (DragEffects eff) g
    , Monad g
    )
  ⇒ Event MouseEvent
  → (DragEvent → f Unit)
  → H.ParentDSL s s' f f' g p (Canceler (DragEffects eff))
subscribe' ev tag = do
  drag ← begin ev
  H.subscribe'
    $ H.eventSource drag.subscription
    $ pure <<< tag
  pure drag.canceler
