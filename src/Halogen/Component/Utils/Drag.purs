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

module Halogen.Component.Utils.Drag where

import Prelude

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
import Halogen.CustomEvents (mouseEventToPageEvent, domEventToMouseEvent, PageEvent)

type DragData =
  { x ∷ Number
  , y ∷ Number
  , deltaX ∷ Number
  , deltaY ∷ Number
  , offsetX ∷ Number
  , offsetY ∷ Number
  }

data DragEvent
  = Move (Event PageEvent) DragData
  | Done (Event PageEvent)

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
  let ev' = mouseEventToPageEvent ev
      initX = ev'.pageX
      initY = ev'.pageY

  handler ← makeVar
  remove ← makeVar
  event ← makeVar' ev'

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
          let e' = mouseEventToPageEvent $ domEventToMouseEvent e
              x1 = event'.pageX
              y1 = event'.pageY
              x2 = e'.pageX
              y2 = e'.pageY
              dragData =
                { x: x2
                , y: y2
                , deltaX: x2 - x1
                , deltaY: y2 - y1
                , offsetX: x2 - initX
                , offsetY: y2 - initY
                }
          putVar event e'
          liftEff $ handler' (Move e' dragData)

        mouseUp ∷ EventListener (DragEffects eff)
        mouseUp = eventListener \e → do
          let e' = mouseEventToPageEvent $ domEventToMouseEvent e
          remove'
          handler' (Done e')

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
