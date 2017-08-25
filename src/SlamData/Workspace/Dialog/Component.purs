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

module SlamData.Workspace.Dialog.Component
  ( Query(..)
  , Message(..)
  , ChildSlot
  , component
  , module SlamData.Workspace.Dialog.Types
  ) where

import SlamData.Prelude

import DOM.Event.Types (MouseEvent)

import Halogen as H
import Halogen.Component.ChildPath as CP
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP

import Quasar.Advanced.Types as QAT

import SlamData.Dialog.Error.Component as Error
import SlamData.License as License
import SlamData.Workspace.Dialog.Confirm.Component as Confirm
import SlamData.Workspace.Dialog.Export.Component as Export
import SlamData.Workspace.Dialog.Reason.Component as Reason
import SlamData.Workspace.Dialog.Rename.Component as Rename
import SlamData.Workspace.Dialog.Share.Component as Share
import SlamData.Workspace.Dialog.Theme.Component as Theme
import SlamData.Workspace.Dialog.Unshare.Component as Unshare
import SlamData.Workspace.Dialog.VizUnavailable.Component as VizUnavailable
-- import SlamData.Dialog.License (advancedLicenseExpired, advancedTrialLicenseExpired, licenseInvalid)
import SlamData.Monad (Slam)
import SlamData.Render.ClassName as CN
import SlamData.Workspace.Deck.Options (DeckOptions)
import SlamData.Workspace.Dialog.Types (Dialog(..))

import Utils.DOM as DOM

type State = Maybe Dialog

data Query a
  = Show Dialog a
  | Raise Message a
  | BackdropDismiss MouseEvent a

data Message
  = Dismissed
  | Confirm DeckOptions Dialog Boolean

type ChildQuery
  = Rename.Query
  ⨁ Theme.Query
  ⨁ Error.Query
  ⨁ Confirm.Query
  ⨁ Export.Query
  ⨁ Export.Query
  ⨁ Share.Query
  ⨁ Unshare.Query
  ⨁ Reason.Query
  ⨁ VizUnavailable.Query
  ⨁ Const Void

type ChildSlot
  = Unit
  ⊹ Unit
  ⊹ Unit
  ⊹ Unit
  ⊹ Unit
  ⊹ Unit
  ⊹ Unit
  ⊹ Unit
  ⊹ Unit
  ⊹ Unit
  ⊹ Void

type HTML = H.ParentHTML Query ChildQuery ChildSlot Slam

type DSL = H.ParentDSL State Query ChildQuery ChildSlot Message Slam

component ∷ H.Component HH.HTML Query Unit Message Slam
component =
  H.parentComponent
    { render
    , eval
    , initialState: const Nothing
    , receiver: const Nothing
    }

renderDialogContainer ∷ Array HTML → HTML
renderDialogContainer =
  HH.div
    [ HP.class_ CN.dialogContainer
    , HE.onClick $ HE.input BackdropDismiss
    ]

render ∷ State → HTML
render = case _ of
  Nothing →
    HH.text ""
  Just (LicenseProblem problem) →
    renderDialogContainer [ dialog $ LicenseProblem problem ]
  Just s →
    renderDialogContainer
      [ HH.div [ HP.class_ CN.dialog ] [ dialog s ] ]
  where
  dialog dlg = case dlg of
    Rename opts name →
      HH.slot' CP.cp1 unit Rename.component name
        case _ of
          Rename.Dismiss →
            Just $ H.action $ Raise Dismissed
          Rename.Rename name' →
            Just $ H.action $ Raise (Confirm opts (Rename opts name') true)

    Theme opts theme →
      HH.slot' CP.cp2 unit Theme.component theme
        case _ of
          Theme.Dismiss →
            Just $ H.action $ Raise Dismissed
          Theme.Theme maybeTheme →
            Just $ H.action $ Raise (Confirm opts (Theme opts maybeTheme) true)

    Error _ str →
      HH.slot' CP.cp3 unit Error.nonModalComponent str
        \Error.Dismiss → Just $ H.action $ Raise Dismissed

    DeleteDeck opts →
      HH.slot' CP.cp4 unit Confirm.component
        { title: "Delete deck"
        , body: "Are you sure you want to delete this deck?"
        , cancel: "Cancel"
        , confirm: "Delete"
        }
        \(Confirm.Confirm bool) → Just $ H.action $ Raise (Confirm opts dlg bool)

    Embed _ sharingInput varMaps →
      HH.slot' CP.cp5 unit Export.component
        { sharingInput
        , presentingAs: Export.Embed
        , varMaps
        }
        \Export.Dismiss → Just $ H.action $ Raise Dismissed

    Publish _ sharingInput varMaps →
      HH.slot' CP.cp6 unit Export.component
        { sharingInput
        , presentingAs: Export.Publish
        , varMaps
        }
        \Export.Dismiss → Just $ H.action $ Raise Dismissed

    Share _ sharingInput →
      HH.slot' CP.cp7 unit Share.component sharingInput
        \Share.Dismiss → Just $ H.action $ Raise Dismissed

    Unshare _ sharingInput →
      HH.slot' CP.cp8 unit Unshare.component sharingInput
        \Unshare.Dismiss → Just $ H.action $ Raise Dismissed

    Reason _ attemptedCardType reason cardPaths →
      HH.slot' CP.cp9 unit Reason.component
        { attemptedCardType
        , reason
        , cardPaths
        }
        \Reason.Dismiss → Just $ H.action $ Raise Dismissed

    VizUnavailable inp →
      HH.slot' CP.cp10 unit VizUnavailable.component inp
        \_ → Just $ H.action $ Raise Dismissed

    LicenseProblem (License.Expired licenseType) →
      case licenseType of
        QAT.Advanced → HH.text ""
        QAT.AdvancedTrial → HH.text ""

    LicenseProblem License.Invalid →
      HH.text ""

eval ∷ Query ~> DSL
eval = case _ of
  Show dialog next → H.put (Just dialog) $> next
  Raise msg next → H.put Nothing *> H.raise msg $> next
  BackdropDismiss me next → do
    isDialog ← H.liftEff $ DOM.nodeEq (DOM.target me) (DOM.currentTarget me)
    when isDialog do
      H.put Nothing
    pure next
