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

module SlamData.Workspace.Card.PivotTable.Component
  ( pivotTableComponent
  ) where

import SlamData.Prelude

import Data.Argonaut (cursorGet, foldJson)
import Data.Array as Array
import Data.Map as Map
import Data.List as List

import Halogen as H
import Halogen.HTML.Indexed as HH
import Halogen.HTML.Events.Indexed as HE
import Halogen.HTML.Properties.Indexed as HP

import SlamData.Monad (Slam)
import SlamData.Quasar.Query as Quasar
import SlamData.Workspace.Card.CardType as CT
import SlamData.Workspace.Card.Chart.Axis (analyzeJArray)
import SlamData.Workspace.Card.Component as CC
import SlamData.Workspace.Card.Model as Card
import SlamData.Workspace.Card.PivotTable.Component.Query (Query(..), QueryC)
import SlamData.Workspace.Card.PivotTable.Component.State (State, initialState, modelFromState)
import SlamData.Workspace.Card.PivotTable.Pivot as Pivot
import SlamData.Workspace.Card.Port as Port

type PivotTableDSL = H.ParentDSL State Void QueryC (Const Void) Slam Void

type PivotTableHTML = H.ParentHTML Void QueryC (Const Void) Slam Void

pivotTableComponent ∷ CC.CardComponent
pivotTableComponent = CC.makeCardComponent
  { cardType: CT.PivotTable
  , component: H.parentComponent
      { render
      , eval: coproduct evalCard evalTable
      , peek: Nothing
      }
  , initialState: H.parentState initialState
  , _State: CC._PivotTableState
  , _Query: CC.makeQueryPrism' CC._PivotTableQuery
  }

render ∷ State → PivotTableHTML
render st =
  HH.div
    []
    [ HH.h4_ [ HH.text "Dimensions" ]
    , HH.ul_ (Array.mapWithIndex (renderJCursor RemoveDimension) st.dimensions)
    , renderJCursorSelect AddDimension
    , HH.h4_ [ HH.text "Columns" ]
    , HH.ul_ (Array.mapWithIndex (renderJCursor RemoveColumn) st.columns)
    , renderJCursorSelect AddColumn
    , case Array.length st.dimensions, Array.length st.columns of
        _, 0 → HH.text ""
        0, _ → renderFlat st.columns st.rows
        _, _ → renderTable st.dimensions st.columns st.rows
    ]
  where
  renderJCursor act ix c =
    HH.li_
      [ HH.text (show c)
      , HH.span
          [ HE.onClick (HE.input_ (right ∘ act ix)) ]
          [ HH.text "×" ]
      ]

  renderJCursorSelect act =
    HH.select
      [ HE.onSelectedIndexChange (HE.input (\e → right ∘ act (e - 1))) ]
      ([ HH.option [ HP.selected true ] [ HH.text "" ] ]
        <> map (\c → HH.option_ [ HH.text (show c) ]) (st.cursors))

  renderFlat cols rows =
    HH.table_ $
      [ HH.tr_ (map (\label → HH.th_ [ HH.text (show label) ]) cols)
      ] <> map HH.tr_ (renderLeaves cols rows)

  renderTable dims cols rows =
    let
      dims' = List.fromFoldable (map cursorGet dims)
      pivot = Pivot.classify dims' rows in
    HH.table_ $
      [ HH.tr_ $
          [ HH.td [ HP.colSpan (Array.length dims) ] []
          ] <> map (\label → HH.th_ [ HH.text (show label) ]) cols
      ] <> (renderRows cols pivot)

  renderRows cols =
    map HH.tr_ <<< Pivot.fold (renderLeaves cols) renderHeadings

  renderLeaves columns =
    map \json →
      map (renderColumn json) columns

  renderColumn json col =
    case cursorGet col json of
      Nothing → HH.td_ [ HH.text "" ]
      Just a  → HH.td_ [ HH.text (renderJson a) ]

  renderHeadings m =
    foldMap renderHeading (Map.toList m)

  renderHeading (Tuple k rs) =
    case Array.uncons rs of
      Just { head, tail } →
        Array.cons
          (Array.cons
            (HH.th [ HP.rowSpan (Array.length rs) ] [ HH.text (renderJson k) ])
            head)
          tail
      Nothing →
        []

  renderJson =
    foldJson show show show id show show

evalCard ∷ CC.CardEvalQuery ~> PivotTableDSL
evalCard = case _ of
  CC.EvalCard info _ next → do
    case info.input of
      Just (Port.TaggedResource res) → updateTable res
      _ → pure unit
    pure next
  CC.Activate next →
    pure next
  CC.Deactivate next →
    pure next
  CC.SetDimensions _ next →
    pure next
  CC.Save k →
    map (k ∘ Card.PivotTable ∘ modelFromState) H.get
  CC.Load card next →
    pure next
  CC.ModelUpdated _ next →
    pure next
  CC.ZoomIn next →
    pure next

evalTable ∷ Query ~> PivotTableDSL
evalTable = case _ of
  AddDimension ix next → do
    st ← H.get
    for_ (Array.index st.cursors ix) \cursor → do
      H.modify _
        { dimensions = Array.snoc st.dimensions cursor
        }
    pure next
  RemoveDimension ix next → do
    H.modify \st → st { dimensions = Array.take ix st.dimensions }
    pure next
  AddColumn ix next → do
    st ← H.get
    for_ (Array.index st.cursors ix) \cursor → do
      H.modify _
        { columns = Array.snoc st.columns cursor
        }
    pure next
  RemoveColumn ix next → do
    H.modify \st → st { columns = Array.take ix st.columns }
    pure next

updateTable ∷ Port.TaggedResourcePort → PivotTableDSL Unit
updateTable res = do
  rows ← either (const []) id <$> Quasar.all res.resource
  let
    cursors =
      List.toUnfoldable
        (Map.keys (analyzeJArray rows))
  H.modify _
    { rows = rows
    , cursors = cursors
    , dimensions = []
    , columns = []
  }
