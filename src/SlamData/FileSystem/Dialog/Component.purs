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

module SlamData.FileSystem.Dialog.Component where

import SlamData.Prelude

import Data.Array (singleton)
import DOM.Event.Types (MouseEvent)
import Halogen as H
import Halogen.Component.ChildPath as CP
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.HTML.Properties.ARIA as ARIA
import Quasar.Advanced.Types as QAT
import SlamData.Dialog.License (advancedLicenseExpired, advancedTrialLicenseExpired, licenseInvalid)
import SlamData.FileSystem.Dialog.Component.Message (Message(..))
import SlamData.FileSystem.Dialog.Mount.Component as Mount
import SlamData.FileSystem.Dialog.Rename.Component as Rename
import SlamData.FileSystem.Resource (Resource, Mount)
import SlamData.License as License
import SlamData.Monad (Slam)
import SlamData.Render.ClassName as CN
import SlamData.Workspace.Deck.Component.CSS as CSS
import Utils.DOM as DOM

data Dialog
  = Rename Resource
  | Mount Mount.Input
  | LicenseProblem License.LicenseProblem

type State = Maybe Dialog

data Query a
  = Show Dialog a
  | BackdropDismiss MouseEvent a
  | RaiseDismiss a
  | QueryRename (Rename.Query Unit) a
  | SaveMount (Maybe Mount → a)
  | HandleChild Message a
  | AddDirsToRename (Array Resource) a

type ChildQuery
  = Rename.Query
  ⨁ Mount.Query
  ⨁ Const Void

type ChildSlot = Unit ⊹ Unit ⊹ Void

component ∷ H.Component HH.HTML Query Unit Message Slam
component =
  H.parentComponent
    { initialState: const Nothing
    , render
    , eval
    , receiver: const Nothing
    }

render ∷ State → H.ParentHTML Query ChildQuery ChildSlot Slam
render state =
    HH.div
        [ HP.classes $
            [ CN.dialogContainer ] <> (guard (isNothing state) $> CSS.invisible)
        , HE.onClick $ HE.input BackdropDismiss
        , ARIA.hidden $ show $ isNothing state
        ]
        $ maybe [] (singleton ∘ dialog) state
  where
  dialog = case _ of
    Rename res →
      HH.slot' CP.cp1 unit Rename.component res (HE.input HandleChild)
    Mount input →
      HH.slot' CP.cp2 unit Mount.component input (HE.input HandleChild)
    LicenseProblem (License.Expired licenseType) →
      case licenseType of
        QAT.Advanced → advancedLicenseExpired
        QAT.AdvancedTrial → advancedTrialLicenseExpired
    LicenseProblem License.Invalid →
      licenseInvalid

eval ∷ Query ~> H.ParentDSL State Query ChildQuery ChildSlot Message Slam
eval = case _ of
  QueryRename q next → do
    _ ← H.query' CP.cp1 unit q
    pure next
  SaveMount reply → do
    map (reply ∘ join) $ H.query' CP.cp2 unit $ H.request Mount.Save
  Show d next → do
    H.put (Just d)
    pure next
  BackdropDismiss me next → do
    isDialog ← H.liftEff $ DOM.nodeEq (DOM.target me) (DOM.currentTarget me)
    when isDialog do
      H.put Nothing
    pure next
  RaiseDismiss next → do
    H.put Nothing
    pure next
  HandleChild m next → do
    case m of
      Dismiss → H.put Nothing
      _ → H.raise m
    pure next
  AddDirsToRename dirs next → do
    _ ← H.query' CP.cp1 unit $ H.action $ Rename.AddDirs dirs
    pure next
