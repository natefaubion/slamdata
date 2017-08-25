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

module SlamData.Autocomplete.Component where

import Prelude

import Control.Monad.Eff.Class (class MonadEff)
import Control.MonadPlus (guard)
import DOM (DOM)
import DOM.Classy.HTMLElement as DCH
import DOM.Classy.Node as DCN
import DOM.Event.Event (preventDefault)
import DOM.Event.KeyboardEvent (KeyboardEvent)
import DOM.Event.KeyboardEvent as KeyEv
import DOM.Event.MouseEvent (MouseEvent)
import DOM.Event.MouseEvent as MouseEvent
import DOM.HTML.Types (HTMLElement)
import DOM.Node.NodeList as NodeList
import Data.Array (filter, length, mapWithIndex, null, (!!))
import Data.Bifunctor (bimap)
import Data.Const (Const(..))
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Newtype (un)
import Data.String as String
import Data.Traversable (for_, traverse_)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.HTML.Properties.ARIA as Aria

data Query item a
  = Init a
  | UpdateItems (Array item) a
  | Input String a
  | GetInput (String → a)
  | Select item a
  | ItemClick item MouseEvent a
  | Focus a
  | Blur a
  | Previous a
  | Next a
  | Open a
  | Close Reason a
  | KeyDown KeyboardEvent a

data Reason
  = CuzEscape
  | CuzBlur
  | CuzNoMatches
  | CuzSelect

showReason ∷ Reason → String
showReason = case _ of
  CuzEscape → "CuzEscape"
  CuzBlur → "CuzBlur"
  CuzNoMatches → "CuzNoMatches"
  CuzSelect → "CuzSelect"

type State item =
  { open ∷ Boolean
  , statusText ∷ String
  , index ∷ Maybe Int
  , items ∷ Array item
  , inputText ∷ String
  }

data Message item = Changed String | Selected item

type HTML item = H.ComponentHTML (Query item)
type DSL item m = H.ComponentDSL (State item) (Query item) (Message item) m

type Config item =
  { containerClass ∷ HH.ClassName
  , placeholder ∷ String
  , autofirst ∷ Boolean
  , itemFilter ∷ String → item → Boolean
  , itemText ∷ item → String
  , itemDisplay ∷ item → H.HTML Void (Const Void)
  }

defaultConfig ∷ Config String
defaultConfig =
  { containerClass: HH.ClassName "halogen-autocomplete"
  , placeholder: ""
  , autofirst: false
  , itemFilter: \input item → not String.null input && String.contains (String.Pattern input) item
  , itemText: id
  , itemDisplay: \item → HH.text item
  }

component
  ∷ ∀ item e m
  . MonadEff (dom ∷ DOM | e) m
  ⇒ Config item
  → H.Component HH.HTML (Query item) (Array item) (Message item) m
component { containerClass, placeholder, autofirst, itemFilter, itemText, itemDisplay } =
  H.lifecycleComponent
   { initialState
   , render
   , eval
   , initializer: Just (H.action Init)
   , finalizer: Nothing
   , receiver: HE.input UpdateItems
   }
  where
    initialState =
      { open: false
      , statusText: ""
      , index: Nothing
      , items: _
      , inputText: ""
      }

    render ∷ State item → HTML item
    render state =
      HH.div
        [ HP.class_ containerClass
        ]
        [ HH.input
            [ HP.value state.inputText
            , HE.onValueInput (HE.input Input)
            , HP.placeholder placeholder
            , HE.onBlur (HE.input_ Blur)
            , HE.onFocus (HE.input_ Focus)
            , HE.onKeyDown (HE.input KeyDown)
            , HP.class_ (H.ClassName "form-control")
            ]
        , HH.ul
            (join
              [ guard (not state.open) $> Aria.hidden ""
              , guard (not state.open) $> HP.class_ (H.ClassName "hidden")
              , pure (HP.ref ulRef)
              ])
            (mapWithIndex mkSelection (filter (itemFilter state.inputText) state.items))
        , HH.span
            [ className "visually-hidden"
            , Aria.role "status"
            , Aria.live "assertive"
            , Aria.relevant "additions"
            ]
            [ HH.text state.statusText ]
        ]
      where
        mkSelection ix item =
          HH.li
            [ HE.onMouseDown (HE.input (ItemClick item))
            , Aria.selected (if Just ix == state.index then "true" else "false")
            ]
            [ bimap absurd (absurd <<< un Const) (itemDisplay item) ]

    eval ∷ Query item ~> DSL item m
    eval = case _ of
     Init a → pure a
     GetInput f → do
       inputText ← H.gets _.inputText
       pure (f inputText)
     UpdateItems items a → do
       H.modify (_ { items = items })
       pure a
     Input input a → do
       H.modify (_ { inputText = input })
       { items } ← H.get
       H.raise (Changed input)
       if null (filter (itemFilter input) items)
         then do
           close CuzNoMatches
           pure a
         else eval (Open a)
     Focus a → do
       when autofirst (H.modify (_ { open = true, index = Just 0 }))
       pure a
     Blur a → do
       close CuzBlur
       pure a
     ItemClick item ev a → do
       when (MouseEvent.button ev == 0) do
         H.liftEff (preventDefault (MouseEvent.mouseEventToEvent ev))
         let newInput = itemText item
         H.modify (_ { inputText = newInput })
         H.raise (Changed newInput)
         H.raise (Selected item)
         close CuzSelect
       pure a
     Select item a → do
       let newInput = itemText item
       H.modify (_ { inputText = itemText item })
       H.raise (Changed newInput)
       H.raise (Selected item)
       pure a
     Previous a → do
       { index } ← H.get
       case index of
         Nothing → pure a
         Just ix → do
           count ← length <$> displayedItems
           goto itemText (if ix == 0 then count - 1 else ix - 1)
           pure a
     Next a → do
       { index } ← H.get
       case index of
         Nothing → pure a
         Just ix → do
           count ← length <$> displayedItems
           goto itemText (if ix == count - 1 then 0 else ix + 1)
           pure a
     Open a → do
       H.modify (_ { open = true, index = Just 0 })
       pure a
     Close reason a → do
       close reason
       pure a
     KeyDown ev a → do
       case KeyEv.code ev of
         "Enter" → do
           H.liftEff (preventDefault (KeyEv.keyboardEventToEvent ev))
           { index } ← H.get
           items ← displayedItems
           case (items !! _) =<< index of
             Just item → eval (Select item a)
             Nothing → pure a
         "Escape" → do
           close CuzEscape
           pure a
         "ArrowUp" → do
           H.liftEff (preventDefault (KeyEv.keyboardEventToEvent ev))
           eval (Previous a)
         "ArrowDown" → do
           H.liftEff (preventDefault (KeyEv.keyboardEventToEvent ev))
           eval (Next a)
         _ → pure a

    displayedItems = do
      { items, inputText } ← H.get
      pure (filter (itemFilter inputText) items)

goto
  ∷ ∀ m e item
  . MonadEff (dom ∷ DOM | e) m
  ⇒ (item → String)
  → Int
  → DSL item m Unit
goto itemText index = do
  H.modify \state →
    state
      { index = Just index
      , statusText = fromMaybe "" (itemText <$> (state.items !! index))
      }
  H.getHTMLElementRef ulRef >>= traverse_ (scrollListToIndex index)

close ∷ ∀ item m. Reason → DSL item m Unit
close reason = do
  whenM (H.gets _.open) do
    H.modify (_ { index = Nothing, open = false })

scrollListToIndex ∷ ∀ m e. MonadEff (dom ∷ DOM | e) m ⇒ Int → HTMLElement → m Unit
scrollListToIndex index el = H.liftEff do
  lis ← DCN.childNodes el
  NodeList.item index lis >>= traverse_ \item → do
    for_ (DCN.fromNode item) \(itemElement ∷ HTMLElement) → do
      itemTop ← DCH.offsetTop itemElement
      ulHeight ← DCH.clientHeight el
      itemHeight ← DCH.clientHeight itemElement
      DCH.setScrollTop (itemTop - ulHeight + itemHeight) el


ulRef ∷ H.RefLabel
ulRef = H.RefLabel "autocomplete-ul"

className ∷ ∀ r a. String → HP.IProp ("class" ∷ String | r) a
className = HP.class_ <<< HH.ClassName
