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

module SlamData.AdminUI.Group where

import SlamData.Prelude

import Data.Array as Array
import Data.List as L
import Data.Path.Pathy as Pathy
import Data.String as String
import Halogen as H
import Halogen.Component.Proxy (proxyQL)
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.Query.HalogenM as HQ
import Quasar.Advanced.Types as QA
import SlamData.AdminUI.Group.Item as GI
import SlamData.AdminUI.Types as AT
import SlamData.Monad (Slam)
import SlamData.Quasar.Security (groupInfo)
import SlamData.Render.Icon as I
import SlamData.Workspace.MillerColumns.Column.Component as MCC
import SlamData.Workspace.MillerColumns.Column.Component.Request as MCREQ
import SlamData.Workspace.MillerColumns.Component as Miller
import SlamData.Workspace.MillerColumns.Component.State (ColumnsData)
import Utils.DOM as DOM
import Utils.Path as PU

renderGroupsForm ∷ Array AT.HTML
renderGroupsForm =
  [ HH.slot' AT.cpGroups unit (Miller.component columnOptions) columnState (HE.input (either AT.HandleColumns AT.HandleColumnOrItem)) ]
  where
    columnState ∷ ColumnsData AT.GroupItem QA.GroupPath
    columnState = QA.GroupPath (Pathy.rootDir) × L.Nil

    columnOptions ∷ Miller.ColumnOptions AT.GroupItem QA.GroupPath AT.GroupMessage
    columnOptions =
      Miller.ColumnOptions
        { renderColumn: component
        , renderItem: GI.component
        , label: \(AT.GroupItem { path: QA.GroupPath gp }) →
            case Pathy.peel gp of
              Nothing → "/"
              Just (_ × suffix) → PU.nameOfFileOrDir suffix
        , isLeaf: const false
        , id: \(AT.GroupItem { path }) → path
        }

type ColumnOptions = MCC.ColumnOptions AT.GroupItem QA.GroupPath AT.GroupMessage
type ColumnQuery = MCC.Query AT.GroupItem QA.GroupPath AT.GroupMessage

type Message' = MCC.Message' AT.GroupItem QA.GroupPath AT.GroupMessage

component
  ∷ ColumnOptions
  → QA.GroupPath
  → H.Component HH.HTML (MCC.Query' AT.GroupItem QA.GroupPath AT.GroupMessage) Input Message' Slam
component opts = proxyQL ∘ component' opts

data Query a = Raise Message' a | SetNewGroupText String a

type Query' = Coproduct ColumnQuery Query

type State = { item ∷ Maybe AT.GroupItem, newGroupText ∷ String, columnWidth ∷ MCC.ColumnWidth }

type DSL = H.ParentDSL State Query' ColumnQuery Unit Message' Slam
type HTML = H.ParentHTML Query' ColumnQuery Unit Slam
type Input = MCC.ColumnWidth × Maybe AT.GroupItem

component'
  ∷ ColumnOptions
  → QA.GroupPath
  → H.Component HH.HTML Query' Input Message' Slam
component' opts path =
  H.parentComponent
    { initialState: \(columnWidth × item) → { item, newGroupText: "", columnWidth }
    , render
    , eval
    , receiver: Just ∘ left ∘ H.action ∘ MCC.HandleInput
    }
  where

  column ∷ H.Component HH.HTML ColumnQuery Input Message' Slam
  column = MCC.component' opts path

  render ∷ State → HTML
  render st =
    HH.div
      [ HP.class_ (HH.ClassName "sd-admin-ui-column") ]
      [ HH.slot unit column (st.columnWidth × st.item) (Just ∘ right ∘ H.action ∘ Raise)
      , HH.form
          [ HP.class_ (HH.ClassName "sd-admin-ui-group-form")
          , HE.onSubmit (\event → addNewGroup { path, event, name: st.newGroupText })
          ]
          [ HH.input
              [ HP.id_ labelId
              , HP.class_ (HH.ClassName "sd-admin-ui-group-input")
              , HP.value st.newGroupText
              , HE.onValueInput \s → Just $ right $ H.action $ SetNewGroupText s
              , HP.placeholder "Add Group"
              , HP.attr (HH.AttrName "size") "1"
              ]
          , HH.label
              [ HP.classes (HH.ClassName <$> fold [pure "sd-admin-ui-group-input-label", guard (not inputIsEmpty) $> "active"])
              , HP.for labelId
              , HE.onClick (\event →
                  if inputIsEmpty
                  then Nothing
                  else addNewGroup { path, event: DOM.toEvent event, name: st.newGroupText })
              ]
              [ I.addCircle ]
          ]
      ]
      where
        addNewGroup = Just ∘ right ∘ H.action ∘ Raise ∘ Right ∘ AT.AddNewGroup
        inputIsEmpty = String.null st.newGroupText
        labelId = "admin-new-group" <> QA.printGroupPath path

  eval ∷ Query' ~> DSL
  eval = coproduct evalInner evalOuter

  evalInner ∷ ColumnQuery ~> DSL
  evalInner =
    maybe (HQ.halt "Inner component query failed") pure <=< H.query unit

  evalOuter ∷ Query ~> DSL
  evalOuter = case _ of
    SetNewGroupText t next → do
      H.modify (_ { newGroupText = t })
      pure next
    Raise msg next → do
      H.raise msg
      pure next

load
  ∷ Tuple QA.GroupPath { requestId ∷ MCREQ.RequestId, filter ∷ String, offset ∷ Maybe Int }
  → AT.DSL (MCREQ.LoadResponse AT.GroupItem)
load (path × { requestId }) =
  groupInfo path >>= case _ of
    Right { subGroups, members } → do
      let items = map groupFromPath (Array.filter (isDirectSubgroup path) subGroups)
      pure { requestId
           , items: L.fromFoldable items
           , nextOffset: Nothing
           }
    Left e → pure (noResult requestId)
  where
    groupFromPath ∷ QA.GroupPath → AT.GroupItem
    groupFromPath p = AT.GroupItem { path: p, name: QA.printGroupPath p }

isDirectSubgroup ∷ QA.GroupPath → QA.GroupPath → Boolean
isDirectSubgroup (QA.GroupPath parent) (QA.GroupPath child) =
  case Pathy.peel child of
    Nothing → -- Should never happen
      false
    Just (childPrefix × _) →
      Pathy.identicalPath parent childPrefix

noResult ∷ MCREQ.RequestId → MCREQ.LoadResponse AT.GroupItem
noResult requestId = { requestId, items: L.Nil, nextOffset: Nothing }
