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

module SlamData.Dialog.Component
  ( Query
  , Message(..)
  , component
  , withClass
  , withPending
  , Button
  , ButtonBuilder
  , ButtonRec
  , button
  , withLabel
  , withAction
  , DialogSpec
  , DialogSpecBuilder
  , DialogSpecDefaults
  , DialogSpecRec
  , Render(..)
  , Eval(..)
  , dialog
  , withTitle
  , withButton
  , withInitialState
  , withRender
  , withParentRender
  , withEval
  , withInitializer
  , withFinalizer
  ) where

import SlamData.Prelude

import Control.Monad.Eff.Class (class MonadEff)
import DOM (DOM)
import DOM.Event.Event (Event, preventDefault)
import DOM.Event.Types (MouseEvent)
import Data.Array as A
import Data.List.NonEmpty as NEL
import Data.List.Safe as SL
import Data.Record as DR
import Halogen as H
import Halogen.Component as HC
import Halogen.Component.Proxy as Proxy
import Halogen.Data.OrdBox (OrdBox, mkOrdBox)
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.HTML.Properties.ARIA as ARIA
import Halogen.Query.HalogenM as HM
import SlamData.Render.ClassName as CN
import SlamData.Render.Common as RC
import Type.Prelude (class RowLacks)
import Unsafe.Coerce (unsafeCoerce)
import Utils.DOM as DOM

derive instance functorMessage ∷ Functor Message

data Query i o a
  = ChangeDialog (Maybe i) a
  | HandleMessage (Message o) a
  | BackdropDismiss MouseEvent a

data Message o
  = Dismiss
  | Bubble o

type State o m = Maybe (Proxy.ProxyComponent (Const Void) Unit (Message o) m)
type InnerQuery o = Proxy.ProxyQ (Const Void) Unit (Message o)
type HTML i o m = H.ParentHTML (Query i o) (InnerQuery o) Unit m
type DSL i o m = H.ParentDSL (State o m) (Query i o) (InnerQuery o) Unit (Message o) m

component
  ∷ ∀ i o m eff
  . MonadEff (dom ∷ DOM | eff) m
  ⇒ (i → DialogSpec o m)
  → H.Component HH.HTML (Query i o) (Maybe i) (Message o) m
component mkInner =
  H.parentComponent
    { render
    , eval: eval mkInner
    , initialState: const Nothing
    , receiver: HE.input ChangeDialog
    }

render ∷ ∀ i o m. State o m → HTML i o m
render = case _ of
  Nothing → HH.text ""
  Just dialogComponent →
    HH.div
      [ HP.class_ (H.ClassName "sd-dialog-container")
      , HE.onClick $ HE.input BackdropDismiss
      ]
      [ HH.slot unit dialogComponent unit (HE.input HandleMessage) ]

eval ∷ ∀ i o m eff. MonadEff (dom ∷ DOM | eff) m ⇒ (i → DialogSpec o m) → Query i o ~> DSL i o m
eval mkInner = case _ of
  ChangeDialog i next → do
    H.put (buildDialog ∘ mkInner <$> i)
    pure next
  HandleMessage msg next → do
    H.raise msg
    pure next
  BackdropDismiss me next → do
    isDialog ← H.liftEff $ DOM.nodeEq (DOM.target me) (DOM.currentTarget me)
    when isDialog (H.raise Dismiss)
    pure next

-------------------------------------------------------------------------------

build ∷ ∀ a b c. (a → b) → c → (c → a) → b
build f c g = f (g c)

-------------------------------------------------------------------------------

withClass ∷ ∀ r. H.ClassName → { classes ∷ Array H.ClassName | r } → { classes ∷ Array H.ClassName | r }
withClass = DR.modify (SProxy ∷ SProxy "classes") ∘ flip A.snoc

withPending ∷ ∀ s r. (s → Boolean) → { pending ∷ s → Boolean | r } → { pending ∷ s → Boolean | r }
withPending = DR.set (SProxy ∷ SProxy "pending")

-------------------------------------------------------------------------------

type ButtonRec s f =
  { label ∷ String
  , classes ∷ Array H.ClassName
  , action ∷ s → Maybe (H.Action f)
  , pending ∷ s → Boolean
  }

newtype Button s f = Button (ButtonRec s f)

derive instance newtypeButton ∷ Newtype (Button s f) _

-------------------------------------------------------------------------------

type ButtonBuilder s f = { classes ∷ Array H.ClassName, pending ∷ s → Boolean } → ButtonRec s f

button ∷ ∀ s f. ButtonBuilder s f → Button s f
button = build Button { classes: [], pending: const false }

withLabel ∷ ∀ r. RowLacks "label" r ⇒ String → { | r } → { label ∷ String | r }
withLabel = DR.insert (SProxy ∷ SProxy "label")

withAction ∷ ∀ s r f. RowLacks "action" r ⇒ (s → Maybe (H.Action f)) → { | r } → { action ∷ s → Maybe (H.Action f) | r }
withAction = DR.insert (SProxy ∷ SProxy "action")

-------------------------------------------------------------------------------

type DialogSpecDefaults s f =
  { classes ∷ Array H.ClassName
  , initializer ∷ Maybe (f Unit)
  , finalizer ∷ Maybe (f Unit)
  , buttons ∷ SL.SafeList SL.Empty (Button s f)
  , pending ∷ s → Boolean
  }

dialogSpecDefaults ∷ ∀ s f. DialogSpecDefaults s f
dialogSpecDefaults =
  { classes: []
  , initializer: Nothing
  , finalizer: Nothing
  , buttons: SL.nil
  , pending: const false
  }

type DialogSpecRec s f g p o m =
  { title ∷ String
  , classes ∷ Array H.ClassName
  , buttons ∷ SL.SafeList SL.NonEmpty (Button s f)
  , initialState ∷ s
  , render ∷ Render s f g p m
  , eval ∷ Eval s f g p (Message o) m
  , initializer ∷ Maybe (f Unit)
  , finalizer ∷ Maybe (f Unit)
  , pending ∷ s → Boolean
  }

foreign import data DialogSpec ∷ Type → (Type → Type) → Type

mkDialogSpec ∷ ∀ s f g p o m. DialogSpecRec s f g p o m → DialogSpec o m
mkDialogSpec = unsafeCoerce

unDialogSpec ∷ ∀ o m r. (∀ s f g p. DialogSpecRec s f g p o m → r) → DialogSpec o m → r
unDialogSpec = unsafeCoerce

newtype Eval s f g p o m = Eval (f ~> H.HalogenM s f g p o m)
newtype Render s f g p m = Render (Tuple (p → OrdBox p) (s → H.ParentHTML f g p m))

type DialogSpecBuilder s f g p o m = DialogSpecDefaults s f → DialogSpecRec s f g p o m

dialog ∷ ∀ s f g p o m. Ord p ⇒ DialogSpecBuilder s f g p o m → DialogSpec o m
dialog = build mkDialogSpec dialogSpecDefaults

withTitle ∷ ∀ r. RowLacks "title" r ⇒ String → { | r } → { title ∷ String | r }
withTitle = DR.insert (SProxy ∷ SProxy "title")

withButton ∷ ∀ r e s f. Button s f → { buttons ∷ SL.SafeList e (Button s f) | r } → { buttons ∷ SL.SafeList SL.NonEmpty (Button s f) | r }
withButton = DR.modify (SProxy ∷ SProxy "buttons") ∘ SL.cons

withInitialState ∷ ∀ r s. RowLacks "initialState" r ⇒ s → { | r } → { initialState ∷ s | r }
withInitialState = DR.insert (SProxy ∷ SProxy "initialState")

withRender ∷ ∀ r s f g p m. RowLacks "render" r ⇒ Ord p ⇒ (s → H.ComponentHTML f) → { | r } → { render ∷ Render s f g p m | r }
withRender = withParentRender ∘ map (lmap absurd)

withParentRender ∷ ∀ r s f g p m. RowLacks "render" r ⇒ Ord p ⇒ (s → H.ParentHTML f g p m) → { | r } → { render ∷ Render s f g p m | r }
withParentRender = DR.insert (SProxy ∷ SProxy "render") ∘ Render ∘ Tuple mkOrdBox

withEval ∷ ∀ r s f g p o m. RowLacks "eval" r ⇒ (f ~> H.HalogenM s f g p (Message o) m) → { | r } → { eval ∷ Eval s f g p (Message o) m | r }
withEval f = DR.insert (SProxy ∷ SProxy "eval") (Eval f)

withInitializer ∷ ∀ f r. H.Action f → { initializer ∷ Maybe (f Unit) | r } → { initializer ∷ Maybe (f Unit) | r }
withInitializer = DR.set (SProxy ∷ SProxy "initializer") ∘ Just ∘ H.action

withFinalizer ∷ ∀ f r. H.Action f → { finalizer ∷ Maybe (f Unit) | r } → { finalizer ∷ Maybe (f Unit) | r }
withFinalizer = DR.set (SProxy ∷ SProxy "finalizer") ∘ Just ∘ H.action

-------------------------------------------------------------------------------

data DialogFormQuery f a = HandleSubmit Event (Either (f a) a)

buildDialog ∷ ∀ o m eff. MonadEff (dom ∷ DOM | eff) m ⇒ DialogSpec o m → Proxy.ProxyComponent (Const Void) Unit (Message o) m
buildDialog = unDialogSpec buildDialog'

buildDialog' ∷ ∀ s f g p o m eff. MonadEff (dom ∷ DOM | eff) m ⇒ DialogSpecRec s f g p o m → Proxy.ProxyComponent (Const Void) Unit (Message o) m
buildDialog' spec@{ render: Render (Tuple mkOrdBox renderInner), eval: Eval evalInner } =
  Proxy.proxy (HC.mkComponent
    { initialState: const spec.initialState
    , render: render'
    , eval: eval'
    , receiver: const Nothing
    , initializer: map right spec.initializer
    , finalizer: map right spec.finalizer
    , mkOrdBox
    })
  where
    buttonDefs ∷ Array (Button s f)
    buttonDefs = SL.toUnfoldable spec.buttons

    actionButtonDef ∷ Button s f
    actionButtonDef = NEL.last (SL.toNEL spec.buttons)

    eval' ∷ Coproduct (DialogFormQuery f) f ~> H.ParentDSL s (Coproduct (DialogFormQuery f) f) g p (Message o) m
    eval' = coproduct evalDialogInner (HM.mapQuery right ∘ evalInner)

    evalDialogInner ∷ (DialogFormQuery f) ~> H.ParentDSL s (Coproduct (DialogFormQuery f) f) g p (Message o) m
    evalDialogInner = case _ of
      HandleSubmit ev eqa → do
        H.liftEff $ preventDefault ev
        case eqa of
          Left q → HM.mapQuery right $ evalInner q
          Right a → pure a

    render' ∷ s → H.ParentHTML (Coproduct (DialogFormQuery f) f) g p m
    render' state =
      let
        isPending = spec.pending state
        submitAction =
          maybe (Right unit) Left
            if isPending
              then Nothing
              else H.action <$> (unwrap actionButtonDef).action state
      in
        HH.form
          [ HP.classes (H.ClassName "sd-dialog" `A.cons` spec.classes)
          , HE.onSubmit \ev → Just (left (HandleSubmit ev submitAction))
          ]
          $ bimap (rmap right) right <$>
              [ HH.div
                  [ HP.class_ (HH.ClassName "sd-dialog-header") ]
                  [ HH.div
                      [ HP.class_ (HH.ClassName "sd-dialog-title") ]
                      [ HH.h4_ [ HH.text spec.title ] ]
                  ]
              , HH.div
                  [ HP.class_ (H.ClassName "sd-dialog-body") ]
                  [ renderInner state ]
              , HH.div
                  [ HP.class_ (H.ClassName "sd-dialog-footer") ]
                  $ renderButtons state isPending buttonDefs
              ]

renderButtons ∷ ∀ s f g p m. s → Boolean → Array (Button s f) → Array (H.ParentHTML f g p m)
renderButtons state disabled xs =
  let
    lefts = A.drop 2 xs
    rights = A.take 2 xs
  in
    join
      [ guard (not A.null lefts) $>
          HH.div
            [ HP.class_ (H.ClassName "sd-dialog-footer-left") ]
            (A.mapWithIndex (renderButton state disabled ∘ const Nothing) lefts)
      , pure $
          HH.div
            [ HP.class_ (H.ClassName "sd-dialog-footer-right") ]
            (A.mapWithIndex (renderButton state disabled ∘ Just) rights)
      ]

renderButton ∷ ∀ s f g p m. s → Boolean → Maybe Int → Button s f → H.ParentHTML f g p m
renderButton state disabled ix (Button { label, classes, action, pending }) =
  let
    query = action state
    classes' = if A.null classes then [CN.btnDefault] else classes
  in
    HH.button
      (join
        [ pure $ HP.classes $ A.cons CN.btn classes'
        , pure $ HP.type_ (if ix == Just 0 then HP.ButtonSubmit else HP.ButtonButton)
        , pure $ HP.enabled (not disabled && isJust query)
        , foldMap (pure ∘ HE.onClick ∘ HE.input_) query
        , pure $ ARIA.label label
        ])
      (join
        [ guard (pending state) *> renderProgressSpinner label
        , pure (HH.text label)
        ])

renderProgressSpinner ∷ ∀ f g p m. String → Array (H.ParentHTML f g p m)
renderProgressSpinner label =
  [ HH.div
    [ HP.class_ (H.ClassName "dialog-loader-btn") ]
    [ RC.spinnerSmall ]
  , HH.span [ HP.class_ CN.srOnly ] [ HH.text label ]
  ]
