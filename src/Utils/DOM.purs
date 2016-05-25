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

module Utils.DOM where

import Control.Bind ((=<<))
import Control.Monad.Aff (Aff)
import Control.Monad.Eff (Eff)
import DOM (DOM)
import DOM.Event.Types (EventTarget)
import DOM.HTML (window)
import DOM.HTML.Types (HTMLElement, htmlElementToElement, htmlDocumentToDocument)
import DOM.HTML.Window (document)
import DOM.Node.ParentNode as P
import DOM.Node.Types (elementToParentNode, Element, documentToEventTarget)
import Data.Maybe (Maybe)
import Data.Nullable (toMaybe)
import Prelude
import Unsafe.Coerce (unsafeCoerce)

foreign import waitLoaded :: forall e. Aff (dom :: DOM |e) Unit
foreign import onLoad :: forall e. Eff e Unit -> Eff e Unit
foreign import blur :: forall e. HTMLElement -> Eff (dom :: DOM|e) Unit
foreign import focus :: forall e. HTMLElement -> Eff (dom :: DOM|e) Unit
foreign import offsetLeft :: forall e. HTMLElement -> Eff (dom :: DOM|e) Number
foreign import getBoundingClientRect :: forall eff.  HTMLElement -> Eff (dom :: DOM | eff) DOMRect
foreign import getTextWidth :: forall eff. String -> String -> Eff (dom :: DOM | eff) Number
foreign import elementEq :: forall eff. HTMLElement -> HTMLElement -> Eff (dom :: DOM | eff) Boolean
foreign import scrollTop :: forall eff. HTMLElement -> Eff (dom :: DOM | eff) Number
foreign import scrollLeft :: forall eff. HTMLElement -> Eff (dom :: DOM | eff) Number
foreign import getOffsetClientRect :: forall eff.  HTMLElement -> Eff (dom :: DOM | eff) DOMRect

type DOMRect =
  { left :: Number
  , top :: Number
  , width :: Number
  , height :: Number
  }

elementToHTMLElement :: Element -> HTMLElement
elementToHTMLElement = unsafeCoerce

querySelector
  :: forall e
   . String
  -> HTMLElement
  -> Eff (dom :: DOM|e) (Maybe HTMLElement)
querySelector str htmlEl =
  map (toMaybe >>> map elementToHTMLElement)
  $ P.querySelector str $ elementToParentNode $ htmlElementToElement htmlEl

documentTarget :: forall e. Eff (dom :: DOM|e) EventTarget
documentTarget = htmlDocumentToEventTarget <$> (document =<< window)
  where
  htmlDocumentToEventTarget = documentToEventTarget <<< htmlDocumentToDocument
