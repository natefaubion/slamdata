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

module SlamData.Workspace.Card.Variables.Component (variablesComponent) where

import SlamData.Prelude

import Data.Array as A
import Data.List as L

import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE

import SlamData.Wiring as Wiring
import SlamData.Workspace.Card.CardType as CT
import SlamData.Workspace.Card.Component as CC
import SlamData.Workspace.Card.Model as Card
import SlamData.Workspace.FormBuilder.Component as FB
import SlamData.Workspace.LevelOfDetails as LOD

type State = Unit
data Query a = HandleMessage FB.Message a

type DSL = CC.InnerCardParentDSL State Query FB.Query Unit
type HTML = CC.InnerCardParentHTML Query FB.Query Unit

variablesComponent ∷ CC.CardOptions → CC.CardComponent
variablesComponent =
  CC.makeCardComponent CT.variables $ H.parentComponent
    { render: render
    , eval: evalCard ⨁ evalComponent
    , initialState: const unit
    , receiver: const Nothing
    }

render ∷ State → HTML
render _ =
  HH.slot
    unit
    FB.formBuilderComponent
    unit
    (HE.input $ map right ∘ HandleMessage)

evalCard ∷ CC.CardEvalQuery ~> DSL
evalCard = case _ of
  CC.Activate next →
    pure next
  CC.Deactivate next →
    pure next
  CC.Save k →
    H.query unit (H.request FB.GetItems) <#>
      maybe [] A.fromFoldable
        ⋙ { items: _ }
        ⋙ Card.Variables
        ⋙ k
  CC.Load card next → do
    accessType ← _.accessType <$> Wiring.expose
    _ ← H.query unit $ H.action $ FB.SetAccessType accessType
    case card of
      Card.Variables { items } →
        void ∘ H.query unit $ H.action (FB.SetItems (L.fromFoldable items))
      _ → pure unit
    pure next
  CC.ReceiveInput _ _ next →
    pure next
  CC.ReceiveOutput _ _ next →
    pure next
  CC.ReceiveState _ next →
    pure next
  CC.ReceiveDimensions _ reply →
    pure $ reply LOD.High

evalComponent ∷ Query ~> DSL
evalComponent = case _ of
  HandleMessage FB.ItemUpdated next → do
    H.raise $ CC.modelUpdate
    pure next
