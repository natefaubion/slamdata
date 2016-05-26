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

module Halogen.CustomEvents where

import DOM.Event.Types (Event) as DOM
import Halogen.HTML.Events.Types (Event, MouseEvent)

import Unsafe.Coerce (unsafeCoerce)

type PageEvent =
  ( pageX ∷ Number
  , pageY ∷ Number
  | MouseEvent
  )

mouseEventToPageEvent ∷ Event MouseEvent → Event PageEvent
mouseEventToPageEvent = unsafeCoerce

domEventToMouseEvent ∷ DOM.Event → Event MouseEvent
domEventToMouseEvent = unsafeCoerce
