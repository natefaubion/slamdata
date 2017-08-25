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

module Utils.DOM
  ( module Utils.DOM
  , module DOM.Classy.Event
  , module DOM.Classy.HTMLElement
  , module DOM.Classy.Node
  , module DOM.Event.Types
  , module DOM.Node.Types
  ) where

import SlamData.Prelude

import Control.Monad.Aff (Aff)
import Control.Monad.Aff as Aff
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Class (liftEff)
import DOM (DOM)
import DOM.Classy.Event (toEvent, fromEvent, target, currentTarget, stopPropagation, preventDefault)
import DOM.Classy.HTMLElement (toHTMLElement, fromHTMLElement)
import DOM.Classy.Node (toNode, fromNode)
import DOM.Event.EventTarget as EventTarget
import DOM.Event.Types (EventTarget, EventType, Event, MouseEvent, KeyboardEvent, FocusEvent)
import DOM.HTML (window)
import DOM.HTML.Event.EventTypes as EventTypes
import DOM.HTML.HTMLElement (classList, offsetHeight, offsetWidth)
import DOM.HTML.Types (HTMLElement, Window, htmlDocumentToDocument, htmlElementToElement, htmlDocumentToNonElementParentNode, windowToEventTarget)
import DOM.HTML.Window (document)
import DOM.Node.ClassList as ClassList
import DOM.Node.Document (createElement)
import DOM.Node.Element (setAttribute, scrollWidth, scrollHeight)
import DOM.Node.NonElementParentNode (getElementById)
import DOM.Node.ParentNode as P
import DOM.Node.Types (DOMTokenList, Element, ElementId(..), Node, documentToEventTarget, elementToEventTarget, elementToParentNode)
import Data.Array (uncons, sort, reverse)
import Data.Nullable as Nullable
import Data.Time.Duration (Milliseconds(..))
import Data.URI (URIRef, printURIRef)
import Unsafe.Coerce (unsafeCoerce)
import Utils.Aff as AffUtils

newtype Font = Font String

foreign import waitLoaded ∷ ∀ e. Aff (dom ∷ DOM |e) Unit
foreign import onLoad ∷ ∀ e. Eff e Unit → Eff e Unit
foreign import blur ∷ ∀ e. HTMLElement → Eff (dom ∷ DOM|e) Unit
foreign import focus ∷ ∀ e. HTMLElement → Eff (dom ∷ DOM|e) Unit
foreign import getTextWidth ∷ ∀ eff. String → Font → Eff (dom ∷ DOM | eff) Number
foreign import nodeEq ∷ ∀ eff. Node → Node → Eff (dom ∷ DOM | eff) Boolean
foreign import getOffsetClientRect ∷ ∀ eff. HTMLElement → Eff (dom ∷ DOM | eff) DOMRect
foreign import close ∷ ∀ eff. Window → Eff (dom ∷ DOM | eff) Unit
foreign import closed ∷ ∀ eff. Window → Eff (dom ∷ DOM | eff) Boolean
foreign import centerPopupWindowFeatures ∷ ∀ eff. Int → Int → Window → Eff (dom ∷ DOM | eff) String
foreign import setFontSize ∷ ∀ eff. HTMLElement → String → Eff (dom ∷ DOM | eff) Unit
foreign import open
  ∷ ∀ eff
  . String
  → String
  → String
  → Window
  → Eff (dom ∷ DOM | eff) (Nullable.Nullable Window)

-- | Same as `getTextWidth` but w/o Eff wrapper. This function definitely has effects
-- | of allocating canvas and should have `Eff (ref ∷ REF|e)` or `Eff (dom ∷ DOM|e)`
-- | but since we don't use intermediate `canvas` anywhere it's safe to think about
-- | this as pure function from font style and string to width.
foreign import getTextWidthPure ∷ String → Font → Number

-- | Gets the HTML color via `getComputedStyle` which isn't in the PS DOM lib
-- | It's passed to the ECharts which wants a string. I'd make it a `Color` but
-- | there's also no generic `toColor` that handles `hsl`, `rgb`, etc.
foreign import getHTMLTextColorString ∷ ∀ eff. Window → Eff (dom ∷ DOM | eff) (Nullable.Nullable String)

fits ∷ ∀ eff. HTMLElement → Eff (dom ∷ DOM | eff) Boolean
fits el = (&&) <$> fitsHorizontally <*> fitsVertically
  where
  fitsHorizontally = (<=) <$> scrollWidth (htmlElementToElement el) <*> offsetWidth el
  fitsVertically = (<=) <$> scrollHeight (htmlElementToElement el) <*> offsetHeight el

fitText ∷ ∀ eff. Array Int → HTMLElement → Eff (dom ∷ DOM | eff) (Maybe Int)
fitText fontSizes el = go $ reverse $ sort fontSizes
  where
  go remainingFontSizes = maybe (pure Nothing) f $ uncons remainingFontSizes
  f { head, tail } =
    setFontSize el (g head) *> fits el >>= (if _ then pure (Just head) else go tail)
  g = flip append "px" ∘ show

type DOMRect =
  { left ∷ Number
  , top ∷ Number
  , width ∷ Number
  , height ∷ Number
  }

elementToHTMLElement ∷ Element → HTMLElement
elementToHTMLElement = unsafeCoerce

querySelector
  ∷ ∀ e
  . String
  → HTMLElement
  → Eff (dom ∷ DOM|e) (Maybe HTMLElement)
querySelector str htmlEl =
  map (map elementToHTMLElement)
    $ P.querySelector (P.QuerySelector str)
    $ elementToParentNode
    $ htmlElementToElement htmlEl

documentTarget ∷ ∀ e. Eff (dom ∷ DOM|e) EventTarget
documentTarget = htmlDocumentToEventTarget <$> (document =<< window)
  where
  htmlDocumentToEventTarget = documentToEventTarget ∘ htmlDocumentToDocument

onResize ∷ ∀ eff. Eff (dom ∷ DOM | eff) Unit → Eff (dom ∷ DOM | eff) Unit
onResize cb = do
  let listener = EventTarget.eventListener \_ → cb
  window
    >>= windowToEventTarget
    >>> EventTarget.addEventListener EventTypes.resize listener false

openPopup ∷ ∀ eff. String → Eff (dom ∷ DOM | eff) (Maybe Window)
openPopup stringUrl = do
  window ← window
  windowFeaturesStr ← centerPopupWindowFeatures 800 600 window
  Nullable.toMaybe <$> open stringUrl "SignIn" windowFeaturesStr window

waitUntilWindowClosed ∷ ∀ eff. Window → Aff (dom ∷ DOM | eff) Unit
waitUntilWindowClosed win = AffUtils.untilA do
  Aff.delay (Milliseconds 250.0)
  liftEff $ closed win

-- Load via `<img>`, but capture the error. Hacky, but that's browsers.
-- https://stackoverflow.com/a/5371426/7110837
-- `onError` skipped because, welp, this hack use erroring.
loadStyleSheet ∷ ∀ eff. URIRef → Aff (dom ∷ DOM | eff) Unit
loadStyleSheet uri = Aff.makeAff \_ onSuccess → do
  liftEff do
    doc ← document =<< window
    img ← createElement "img" (htmlDocumentToDocument doc)
    let
      imgTarget ∷ EventTarget
      imgTarget = elementToEventTarget img

      listener = EventTarget.eventListener \_ → do
        EventTarget.removeEventListener EventTypes.error listener false imgTarget
        onSuccess unit

    EventTarget.addEventListener EventTypes.error listener false imgTarget
    setAttribute "src" (printURIRef uri) img

toggleLoadingOverlay ∷ ∀ eff. Boolean → Eff (dom ∷ DOM | eff) Unit
toggleLoadingOverlay shouldShow = liftEff do
  doc ← document =<< window
  let overlayId = ElementId "page-loading-overlay"
  mbOverlay ← getElementById overlayId (htmlDocumentToNonElementParentNode doc)
  for_ mbOverlay \overlay → do
    overlayClassList ← liftEff $ classList $ elementToHTMLElement overlay
    -- showing? well then let's remove these classes
    when shouldShow do
      ClassList.remove overlayClassList "fade-overlay"
      ClassList.remove overlayClassList "hide-overlay"
    -- else when hiding the overlay, fade it and hide after the transition
    when (not shouldShow) do
      let
        overlayTarget ∷ EventTarget
        overlayTarget = elementToEventTarget overlay

        listener = EventTarget.eventListener \_ → do
          EventTarget.removeEventListener EventTypes.transitionend listener false overlayTarget
          ClassList.add overlayClassList "hide-overlay"
      EventTarget.addEventListener EventTypes.transitionend listener false overlayTarget
      ClassList.add overlayClassList "fade-overlay"

hideLoadingOverlay ∷ ∀ eff. Eff (dom ∷ DOM | eff) Unit
hideLoadingOverlay = toggleLoadingOverlay false

showLoadingOverlay ∷ ∀ eff. Eff (dom ∷ DOM | eff) Unit
showLoadingOverlay = toggleLoadingOverlay true
