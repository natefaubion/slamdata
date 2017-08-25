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

module SlamData.Workspace.Deck.Component.Render
  ( renderDeck
  , renderError
  ) where

import SlamData.Prelude
import Data.Array as A
import Data.List as L
import Halogen.HTML.Events as HE
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Halogen.HTML.Properties.ARIA as ARIA
import SlamData.ActionList.Component as ActionList
import SlamData.ActionList.Filter.Component as ActionFilter
import SlamData.Render.ClassName as CN
import SlamData.Render.Common as RC
import SlamData.Render.Icon as I
import SlamData.Workspace.AccessType as AT
import SlamData.Workspace.Card.Component.CSS as CCSS
import SlamData.Workspace.Deck.Common (DeckOptions, DeckHTML)
import SlamData.Workspace.Deck.Common as Common
import SlamData.Workspace.Deck.Component.CSS as CSS
import SlamData.Workspace.Deck.Component.ChildSlot (cpBackSide, cpActionFilter)
import SlamData.Workspace.Deck.Component.Cycle (DeckComponent)
import SlamData.Workspace.Deck.Component.Query (Query(..))
import SlamData.Workspace.Deck.Component.Query as DCQ
import SlamData.Workspace.Deck.Component.State as DCS
import SlamData.Workspace.Deck.Slider as Slider
import SlamData.Hint as Hint
import Utils (endSentence)
import Utils.DOM as DOM

renderError ∷ ∀ f a. String → HH.HTML a (f Unit)
renderError err =
  HH.div
    [ HP.classes [ HH.ClassName "sd-workspace-error" ] ]
    [ HH.h1_
        [ HH.text "Couldn't load this SlamData deck." ]
    , HH.p_
        [ HH.text $ endSentence err ]
    ]

renderDeck ∷ DeckOptions → (DeckOptions → DeckComponent) → DCS.State → DeckHTML
renderDeck opts deckComponent st =
  HH.div
    [ HP.classes
        [ HH.ClassName "sd-deck-nested"
        , HH.ClassName ("sd-deck-level-" <> show (L.length opts.displayCursor + 1))
        ]
    ]
    $ [ HH.div
          (deckClasses st
          ⊕ deckProperties opts
          ⊕ Slider.containerProperties st)
          [ HH.div
              [ HP.class_ CSS.deckFrame
              , HE.onMouseDown $ HE.input Defocus
              ]
              $ frameElements opts st
          , HH.div
              [ HP.class_ CSS.deck
              ]
              [ HH.div
                  [ HP.class_ $ HH.ClassName "sd-card-sizer"
                  , HP.ref Common.sizerRef
                  ]
                  []
              , Slider.render opts deckComponent st $ DCS.isFrontSide st.displayMode
              , renderBackside $ DCS.isFlipSide st.displayMode
              ]
          ]
      ]
      <> (guard presentFocusDeckHint $> renderFocusDeckHint)
      <> (guard presentFocusDeckFrameHint $> renderFocusDeckFrameHint)


  where
  presentFocusDeckHint ∷ Boolean
  presentFocusDeckHint =
    Common.willBePresentedWithChildFrameWhenFocused opts st
      ∧ (not st.focused)
      ∧ (not st.focusDeckHintDismissed)

  presentFocusDeckFrameHint ∷ Boolean
  presentFocusDeckFrameHint =
    Common.willBePresentedWithChildFrameWhenFocused opts st
      ∧ st.focused
      ∧ (not st.focusDeckFrameHintDismissed)

  renderFocusDeckHint ∷ DeckHTML
  renderFocusDeckHint =
    Hint.render
      Hint.DownArrow
      (HH.ClassName "sd-focus-deck-hint")
      (Just DCQ.DismissFocusDeckHint)
      "This Deck is wrapped in a Dashboard Card and unfocused. To do more with this Deck focus it by clicking or tapping on it."

  renderFocusDeckFrameHint ∷ DeckHTML
  renderFocusDeckFrameHint =
    Hint.render
      Hint.UpArrow
      (HH.ClassName "sd-focus-deck-frame-hint")
      (Just DCQ.DismissFocusDeckFrameHint)
      "This Deck is focused. To do more with the containing Deck focus it by clicking or tapping on the empty space in this Deck Frame."

  renderBackside visible =
    HH.div
      [ HP.classes $ [CSS.cardSlider] ⊕ (guard (not visible) $> CSS.invisible)
      , ARIA.hidden $ show $ not visible
      ]
      [ backside ]

deckClasses ∷ ∀ r. DCS.State → Array (HP.IProp (class ∷ String | r) (Query Unit))
deckClasses st =
  pure $ HP.classes $
    [ CSS.deckContainer
    , HH.ClassName (responsiveSize st.responsiveSize)
    , if st.focused then CSS.focused else CSS.unfocused
    ]

deckProperties ∷ ∀ r. DeckOptions → Array (HP.IProp (onMouseDown ∷ DOM.MouseEvent | r) (Query Unit))
deckProperties opts =
  guard (L.length opts.displayCursor <= 1) $> HE.onMouseDown (HE.input Focus)

frameElements ∷ DeckOptions → DCS.State → Array DeckHTML
frameElements { accessType, cursor, displayCursor } st
  | accessType ≡ AT.ReadOnly = mempty
  | L.null displayCursor = rootFrameElements (L.null cursor) st
  | otherwise = childFrameElements st

rootFrameElements ∷ Boolean → DCS.State → Array DeckHTML
rootFrameElements n st =
  [ deckFrameControls (if n then Back else Out) st
  , deckIndicator st
  ]

childFrameElements ∷ DCS.State → Array DeckHTML
childFrameElements st =
  [ deckFrameControls In st
  , deckIndicator st
  ]

backside ∷ DeckHTML
backside =
  HH.div
    [ HP.classes [ CSS.card ] ]
    [ HH.div_
        [ HH.div
            [ HP.class_ CCSS.deckCard ]
            [ HH.div
                [ HP.class_ CN.deckBackSide ]
                [ HH.slot' cpActionFilter unit ActionFilter.component
                    "Filter deck and card actions"
                    (HE.input HandleBackFilter)
                , HH.slot' cpBackSide unit ActionList.component
                    unit
                    (HE.input HandleBackAction)
                ]
            ]
        ]
    ]

data Zoom = In | Out | Back

derive instance eqZoom ∷ Eq Zoom

deckFrameControls ∷ Zoom → DCS.State → DeckHTML
deckFrameControls zoom { name } =
  HH.div
    [ HP.class_ CSS.deckFrameControls ]
    [ nameMover
    , HH.div
        [ HP.class_ CSS.deckFrameActions ]
        [ case zoom of
            In → zoomInButton
            Out → zoomOutButton
            Back → backToFsButton
        , flipButton
        ]
    ]
  where
    nameMover =
      [ HH.text name ]
        -- zoom-in-able names are grabbable
        # if zoom == In then
          HH.button
            [ HP.classes [ CSS.deckName, CSS.grabDeck ]
            , HP.type_ HP.ButtonButton
            , HP.title "Move deck"
            , ARIA.label "Move deck"
            , HE.onMouseDown (HE.input HandleGrab)
            ]
        else
          HH.div
            [ HP.class_ CSS.deckName ]


deckIndicator ∷ DCS.State → DeckHTML
deckIndicator st =
  HH.div [ HP.classes [ HH.ClassName "indicator" ] ] $
    A.mapWithIndex renderCircle st.displayCards

  where
    activeCard =
      DCS.activeCardIndex st

    isRunning ix =
      st.pendingCardIndex == Just ix

    classes ix card =
      map HH.ClassName
      $ (guard (isRunning ix) $> "running")
      ⊕ (A.singleton case card of
            Right _ → "available"
            Left DCS.PendingCard → "pending"
            Left (DCS.ErrorCard _) → "errored"
            Left (DCS.NextActionCard _) → "placeholder")
      ⊕ (guard (activeCard ≡ ix) $> "focused")

    renderCircle ix card =
      HH.i
        [ HP.classes $ classes ix card ]
        [ if isRunning ix then RC.spinnerSmall else (HH.text "") ]

flipButton ∷ DeckHTML
flipButton =
  HH.button
  [ HP.classes [ CSS.flipDeckBtn ]
    , HE.onClick (HE.input_ FlipDeck)
    , ARIA.label "Flip deck"
    , HP.title "Flip deck"
    ]
    [ I.flipDeck ]

zoomInButton ∷ DeckHTML
zoomInButton =
  HH.button
    [ HP.classes [ CSS.zoomDeckBtn ]
    , ARIA.label "Zoom in"
    , HP.title "Zoom in"
    , HP.type_ HP.ButtonButton
    , HE.onClick (HE.input_ ZoomIn)
    ]
    [ I.zoomInSm ]

zoomOutButton ∷ DeckHTML
zoomOutButton =
  HH.button
    [ HP.classes [ CSS.zoomDeckBtn ]
    , ARIA.label "Zoom out"
    , HP.title "Zoom out"
    , HP.type_ HP.ButtonButton
    , HE.onClick (HE.input_ ZoomOut)
    ]
    [ I.zoomOutSm ]

backToFsButton ∷ DeckHTML
backToFsButton =
  HH.button
    [ HP.classes [ CSS.zoomDeckBtn ]
    , ARIA.label "Back to File System"
    , HP.title "Back to File System"
    , HP.type_ HP.ButtonButton
    , HE.onClick (HE.input_ BackToFileSystem)
    ]
    [ I.gripperArrowLeft ]

responsiveSize ∷ DCS.ResponsiveSize → String
responsiveSize = case _ of
  DCS.XSmall  → "sd-size-xsmall"
  DCS.Small   → "sd-size-small"
  DCS.Medium  → "sd-size-medium"
  DCS.Large   → "sd-size-large"
  DCS.XLarge  → "sd-size-xlarge"
  DCS.XXLarge → "sd-size-xxlarge"
