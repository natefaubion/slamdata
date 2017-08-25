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

module SlamData.AdminUI.Group.Item (component) where

import SlamData.Prelude

import Data.List.NonEmpty as NEL
import Data.Newtype (un)
import Data.Path.Pathy as Pathy
import Halogen as H
import Halogen.Component.Proxy (proxy)
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.HTML.Properties.ARIA as ARIA
import Quasar.Advanced.QuasarAF as QA
import SlamData.AdminUI.Types as AT
import SlamData.Monad (Slam)
import SlamData.Render.Icon as I
import SlamData.Workspace.MillerColumns.Column.Component.Item as MCI
import SlamData.Workspace.MillerColumns.ItemMenu.Component as MCIM
import Utils.Path (nameOfFileOrDir)

component
  ∷ QA.GroupPath
  → AT.GroupItem
  → H.Component HH.HTML (MCI.Query AT.GroupItem AT.GroupMessage) MCI.State (MCI.Message' AT.GroupItem AT.GroupMessage) Slam
component i = proxy ∘ component' i

data Action = Delete | DisplayUsers

data Query a
  = UpdateState MCI.State a
  | HandleSliderMessage (MCIM.Message Action) a
  | Selected a

type ChildQuery = MCIM.Query Action
type ChildSlot = Unit

type HTML = H.ParentHTML Query ChildQuery ChildSlot Slam
type DSL = H.ParentDSL Boolean Query ChildQuery ChildSlot (MCI.Message' AT.GroupItem AT.GroupMessage) Slam

getGroupName ∷ AT.GroupItem → String
getGroupName (AT.GroupItem { path: itemPath }) =
  case Pathy.peel (un QA.GroupPath itemPath) of
    Nothing → "/"
    Just (_ × n) → nameOfFileOrDir n

component'
  ∷ QA.GroupPath
  → AT.GroupItem
  → H.Component HH.HTML Query MCI.State (MCI.Message' AT.GroupItem AT.GroupMessage) Slam
component' path item =
  H.parentComponent
    { initialState: (_ == MCI.Selected)
    , render
    , eval
    , receiver: HE.input UpdateState
    }
  where
  render ∷ Boolean → HTML
  render selected =
    let
      label = getGroupName item
    in
      HH.li
        [ HP.title label
        , HP.classes $ join
            [ pure (HH.ClassName "sd-miller-column-item")
            , guard selected $> HH.ClassName "selected"
            ]
        , HE.onClick $ HE.input_ Selected
        , ARIA.label ("Select " <> label)
        ]
        [ renderItem item ]

  eval ∷ Query ~> DSL
  eval = case _ of
    UpdateState state next → do
      let new = state == MCI.Selected
      old ← H.get
      when (old /= new) $ H.put new
      pure next
    HandleSliderMessage msg next → do
      case msg of
        MCIM.ActionMsg Delete →
          H.raise $ Right $ AT.DeleteGroup { path }
        MCIM.ActionMsg DisplayUsers →
          H.raise $ Right $ AT.DisplayUsers { path }
      pure next
    Selected next → do
      H.raise $ Left $ MCI.RaisePopulate item
      pure next

renderItem ∷ AT.GroupItem → HTML
renderItem ci =
  HH.div
    [ HP.class_ (H.ClassName "sd-miller-column-item-inner") ]
    [ HH.div [ HP.class_ (H.ClassName "sd-admin-ui-group-label") ] [ HH.text (getGroupName ci) ]
    , HH.slot unit slider unit (HE.input HandleSliderMessage)
    ]

slider :: H.Component HH.HTML (MCIM.Query Action) Unit (MCIM.Message Action) Slam
slider =
  MCIM.component
    ({ icon: I.trashCanSm, label: "Delete Group", action: Delete }
      `NEL.cons`
       pure { icon: I.searchSm, label: "Display Users", action: DisplayUsers })
