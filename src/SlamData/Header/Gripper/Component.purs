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

module SlamData.Header.Gripper.Component where

import SlamData.Prelude

import Data.Int as Int
import DOM.Event.Event as DEE
import DOM.Event.EventTarget as Etr
import DOM.Event.MouseEvent as DEM
import DOM.Event.Types as DET
import DOM.HTML (window)
import DOM.HTML.Event.EventTypes as Etp
import DOM.HTML.Types as Ht
import DOM.HTML.Window as Win
import DOM.Node.Element (clientHeight)
import DOM.Node.ParentNode as Pn
import DOM.Node.Types as Dt
import CSS.Animation (animation, iterationCount, normalAnimationDirection, forwards)
import CSS.Property (value)
import CSS.Size (Rel, Size, pct)
import CSS.String (fromString)
import CSS.Stylesheet (CSS, (?), keyframesFromTo)
import CSS.Time (sec)
import CSS.Transform (Transformation(Transformation), transform)
import CSS.Transition (easeOut)
import Halogen as H
import Halogen.Component.Utils (busEventSource)
import Halogen.HTML.CSS as CSS
import Halogen.HTML.Events as HE
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Halogen.HTML.Properties.ARIA as ARIA
import Halogen.Query.EventSource as ES
import SlamData.Monad (Slam)
import SlamData.Render.Icon as I
import SlamData.Wiring as Wiring
import Unsafe.Coerce (unsafeCoerce)

data Query a
  = Init a
  | StartDragging Number a
  | StopDragging a
  | ChangePosition Number Number a
  | Animated a
  | PreventDefault DET.Event (Query a)
  | Close a
  | GetState (State → a)

data Direction = Up | Down

data State
  = Closed
  | Opened
  | Dragging Direction Number Number Number
  | Opening Number Number
  | Closing Number Number

derive instance eqDirection :: Eq Direction
derive instance eqState :: Eq State

data Message = Notify State

initialState ∷ State
initialState = Closed

type HTML = H.ComponentHTML Query
type DSL = H.ComponentDSL State Query Message Slam

component ∷ String → H.Component HH.HTML Query Unit Message Slam
component querySelector = H.lifecycleComponent
  { initialState: const Closed
  , render: render querySelector
  , eval: eval querySelector
  , initializer: Just $ H.action Init
  , finalizer: Nothing
  , receiver: const Nothing
  }

render ∷ String → State → HTML
render sel state =
  HH.div
    [ HP.classes
        [ HH.ClassName "header-gripper"
        , HH.ClassName className
        ]
    , HP.title label
    , HE.onMouseDown \e →
        Just
          $ PreventDefault (DET.mouseEventToEvent e)
          $ H.action
          $ StartDragging (Int.toNumber $ DEM.clientY e)
    , ARIA.label label
    ]
    [ CSS.stylesheet $ renderStyles sel state
    , I.menuSm
    ]
  where
    label ∷ String
    label = case state of
      Closed → "Show header"
      Opening _ _ →  "Show header"
      Dragging Down _ _ _ → "Show header"
      _ → "Hide header"

    className ∷ String
    className = case state of
      Opened → "sd-header-gripper-opened"
      Closed → "sd-header-gripper-closed"
      Opening _ _ → "sd-header-gripper-opening"
      Closing _ _ → "sd-header-gripper-closing"
      Dragging Down _ _ _ → "sd-header-gripper-dragging-down"
      Dragging Up _ _ _ → "sd-header-gripper-dragging-up"

translateY ∷ Size Rel → CSS
translateY y =
  transform $ Transformation $ fromString "translateY(" <> value y <> fromString ")"

animationDuration ∷ Number
animationDuration = 0.5

renderStyles ∷ String → State → CSS
renderStyles sel = case _ of
  Closed → do
    (fromString sel) ? (translateY $ pct $ -100.0)
  Opened → do
    (fromString sel) ? (translateY $ pct zero)
  Opening navHeight startPos → do
    mkAnimation sel ((startPos - navHeight) / navHeight * 100.0) zero
  Closing navHeight startPos → do
    mkAnimation sel ((startPos - navHeight) / navHeight * 100.0) $ -100.0
  Dragging _ navHeight start current → do
    (fromString sel) ? do
      translateY $ pct ((-navHeight + current - start) / navHeight * 100.0)

mkAnimation ∷ String → Number → Number → CSS
mkAnimation sel translateFrom translateTo = do
  let
    translateFromStyle = translateY $ pct translateFrom
    translateToStyle = translateY $ pct translateTo
  (fromString sel) ? do
    keyframesFromTo "header-gripper-translate" translateFromStyle translateToStyle
    animation
      (fromString "header-gripper-translate")
      (sec animationDuration)
      easeOut
      (sec zero)
      (iterationCount one)
      normalAnimationDirection
      forwards

eval ∷ String → (Query ~> DSL)
eval sel = case _ of
  PreventDefault evt q → do
    H.liftEff $ DEE.preventDefault evt
    eval sel q

  Init next → do
    doc ← H.liftEff $ window >>= Win.document
    mbNavEl ← H.liftEff $ Pn.querySelector (Pn.QuerySelector sel) (Ht.htmlDocumentToParentNode doc)

    let
      evntify ∷ ∀ a. a → { clientY ∷ Number }
      evntify = unsafeCoerce

      docTarget = Ht.htmlDocumentToEventTarget doc

      attachMouseUp f =
        Etr.addEventListener Etp.mouseup (Etr.eventListener f) false docTarget
      attachMouseMove f =
        Etr.addEventListener Etp.mousemove (Etr.eventListener f) false docTarget

      handleMouseUp e = do
        pure $ StopDragging ES.Listening

    H.subscribe $ H.eventSource attachMouseUp handleMouseUp

    for_ mbNavEl \navEl → do
      navHeight ← H.liftEff $ clientHeight navEl

      let
        attachAnimationEnd f =
          Etr.addEventListener Etp.animationend (Etr.eventListener f) false
            $ Dt.elementToEventTarget navEl

        handleMouseMove e = do
          pure $ ChangePosition navHeight (evntify e).clientY ES.Listening

        handleAnimationEnd e =
          pure $ Animated ES.Listening

      { auth } ← Wiring.expose
      H.subscribe $ busEventSource (const (Close H.Listening)) auth.signIn
      H.subscribe $ H.eventSource attachMouseMove handleMouseMove
      H.subscribe $ H.eventSource attachAnimationEnd handleAnimationEnd

    pure next

  StartDragging pos next → do
    doc ← H.liftEff $ window >>= Win.document
    mbNavEl ← H.liftEff $ Pn.querySelector (Pn.QuerySelector sel) (Ht.htmlDocumentToParentNode doc)

    for_ mbNavEl \navEl → do
      navHeight ← H.liftEff $ clientHeight navEl
      H.get >>= case _ of
        Closed → H.put (Dragging Down navHeight pos pos)
        Opened → H.put (Dragging Up navHeight (pos - navHeight) pos)
        _ → pure unit
      H.get >>= H.raise ∘ Notify

    pure next

  StopDragging next → do
    astate ← H.get
    case astate of
      Dragging dir navHeight s current →
        let
          nextState ∷  Number → Number → State
          nextState = case dir of
            Down → Opening
            Up → Closing
        in
          H.put (nextState navHeight (current - s))
      _ → pure unit
    pure next

  ChangePosition navHeight num next → do
    astate ← H.get
    let
      toSet s = s + offset
        where
          diff = num - s
          offset = if diff < 0.0 then 0.0 else if diff > navHeight then navHeight else diff
      direction oldPos oldDir =
        if num == oldPos then oldDir else if num > oldPos then Down else Up
    case astate of
      Dragging oldDir navHeight' s old →
        H.put (Dragging (direction old oldDir) navHeight' s (toSet s))
      _ → pure unit
    pure next

  Animated next → do
    astate ← H.get
    case astate of
      Opening _ _ → H.put Opened
      Closing _ _ → H.put Closed
      _ → pure unit
    H.get >>= H.raise ∘ Notify
    pure next

  Close next → do
    doc ← H.liftEff $ window >>= Win.document
    mbNavEl ← H.liftEff $ Pn.querySelector (Pn.QuerySelector sel) (Ht.htmlDocumentToParentNode doc)

    for_ mbNavEl \navEl → do
      navHeight ← H.liftEff $ clientHeight navEl
      H.put (Closing navHeight navHeight)

    pure next

  GetState k →
    k <$> H.get
