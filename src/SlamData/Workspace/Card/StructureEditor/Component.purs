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

module SlamData.Workspace.Card.StructureEditor.Component
  ( component
  , module SlamData.Workspace.Card.StructureEditor.Component.Query
  ) where

import SlamData.Prelude

import Control.Monad.Aff.Future as Future
import Data.Json.Extended (EJson)
import Data.List as L
import Halogen as H
import Halogen.Component.Utils (busEventSource)
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import SlamData.GlobalMenu.Bus as GMB
import SlamData.Wiring as Wiring
import SlamData.Workspace.Card.CardType as CT
import SlamData.Workspace.Card.Component as CC
import SlamData.Workspace.Card.Eval.Common as CEC
import SlamData.Workspace.Card.Model as Card
import SlamData.Workspace.Card.Port as Port
import SlamData.Workspace.Card.StructureEditor.Column.Component as SECC
import SlamData.Workspace.Card.StructureEditor.Common as SEC
import SlamData.Workspace.Card.StructureEditor.Component.ChildSlot as CS
import SlamData.Workspace.Card.StructureEditor.Component.Query (Query(..))
import SlamData.Workspace.Card.StructureEditor.Component.Query as Q
import SlamData.Workspace.Card.StructureEditor.Component.State as S
import SlamData.Workspace.Card.StructureEditor.Item.Component as SEIC
import SlamData.Workspace.Card.StructureEditor.Model (Model(..))
import SlamData.Workspace.LevelOfDetails as LOD
import SlamData.Workspace.MillerColumns.Column.BasicFilter (mkFilter)
import SlamData.Workspace.MillerColumns.Component as MC

type DSL = CC.InnerCardParentDSL S.State Query CS.ChildQuery CS.ChildSlot
type HTML = CC.InnerCardParentHTML Query CS.ChildQuery CS.ChildSlot

component ∷ CC.CardOptions → CC.CardComponent
component =
  CC.makeCardComponent CT.StructureEditor $ H.lifecycleParentComponent
    { render: render
    , eval: evalCard ⨁ evalStructureEditor
    , initialState: const S.initialState
    , receiver: const Nothing
    , initializer: Just $ right $ H.action Q.Init
    , finalizer: Nothing
    }

render ∷ S.State → HTML
render state =
  HH.div
    [ HP.class_ (HH.ClassName "sd-structure-editor-container") ]
    [ renderColumns ]

renderColumns ∷ HTML
renderColumns =
  HH.slot'
    CS.cpColumns
    unit
    (MC.component columnOptions)
    (SEC.rootColumn × L.Nil)
    (map right ∘ HE.input HandleColumnsMessage)

columnOptions ∷ SECC.ColumnOptions
columnOptions =
  MC.ColumnOptions
    { renderColumn: SECC.component
    , renderItem: SEIC.component
    , label: SEC.columnItemLabel
    , isLeaf: SEC.columnPathIsLeaf
    , id: SEC.columnItemPath
    }

evalStructureEditor ∷ Query ~> DSL
evalStructureEditor = case _ of
  Q.Init next → do
    { auth } ← Wiring.expose
    H.subscribe $ busEventSource (\msg → right (Q.HandleSignInMessage msg H.Listening)) auth.signIn
    pure next
  Q.HandleSignInMessage msg next → do
    case msg of
      GMB.SignInSuccess → void $ H.query' CS.cpColumns unit $ H.action $ MC.Reload
      _ → pure unit
    pure next
  Q.HandleColumnsMessage msg next → do
    case msg of
      Left (MC.LoadRequest (path × req)) → do
        res ← load path req
        void $ H.query' CS.cpColumns unit $ H.action $ MC.FulfilLoadRequest (path × res)
      Left (MC.SelectionChanged s _ _) → do
        H.modify (_ {selectedPath = snd s})
        H.raise CC.modelUpdateSilently
      _ →
        pure unit
    pure next

load
  ∷ SEC.ColumnPath
  → MC.LoadRequest
  → DSL (MC.LoadResponse SEC.ColumnItem)
load path { requestId, filter } =
  H.gets _.resource >>= case _ of
    Just (_ × future) → do
      records ← Future.wait future
      let items = L.filter (mkFilter filter ∘ SEC.columnItemLabel) (SEC.analyse records path)
      pure { requestId, items, nextOffset: Nothing }
    _ → pure noResult
  where
  noResult = { requestId, items: L.Nil, nextOffset: Nothing }

fetchData ∷ Port.Resource → DSL (Array EJson)
fetchData res = do
  { path } ← Wiring.expose
  either (const []) id <$> CEC.sampleResourceEJson path res (Just { offset: 0, limit: 1000 })

evalCard ∷ CC.CardEvalQuery ~> DSL
evalCard = case _ of
  CC.Activate next →
    pure next
  CC.Deactivate next →
    pure next
  CC.Save k → do
    selectedPath ← H.gets _.selectedPath
    pure $ k $ Card.StructureEditor $ Model {view: SEC.columnItemPath <$> selectedPath}
  CC.Load (Card.StructureEditor model) next → do
    -- When restoring the state from the Model, we insert a dummy Weight.
    -- The weight doesn't matter, because we only use the items in the column header.
    let restoreColumnItem cp = SEC.ColumnItem cp (SEC.Weight 0.0)
    void $ H.query' CS.cpColumns unit $ H.action $ MC.Populate $ SEC.all × map restoreColumnItem (unwrap model).view
    pure next
  CC.Load _ next →
    pure next
  CC.ReceiveInput x y next → do
    let newResource = Port.extractResource y
    oldResource ← H.gets (map fst ∘ _.resource)
    when (oldResource /= newResource) do
      fut ← maybe (pure (pure [])) (Future.defer ∘ fetchData) newResource
      let resource = map (_ × fut) newResource
      H.modify \st → st { cycle = st.cycle + 1, resource = resource }
    _ ← H.query' CS.cpColumns unit $ H.action MC.Reload
    pure next
  CC.ReceiveOutput _ _ next →
    pure next
  CC.ReceiveState _ next → do
    pure next
  CC.ReceiveDimensions dims reply → do
    pure $ reply
      if dims.width < 250.0 ∨ dims.height < 50.0
      then LOD.Low
      else LOD.High
