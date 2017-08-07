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

module SlamData.Workspace.Card.Troubleshoot.Component where

import SlamData.Prelude
import Data.Array as Array
import Data.Map as Map
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import SlamData.Render.ClassName as CN
import SlamData.Workspace.Card.CardType as CT
import SlamData.Workspace.Card.Component as CC
import SlamData.Workspace.Card.Model as Card
import SlamData.Workspace.Card.Port as Port
import SlamData.Workspace.Card.Port.VarMap as VM
import SlamData.Workspace.Card.Troubleshoot.Component.Query (Query)
import SlamData.Workspace.Card.Troubleshoot.Component.State (State, initialState)
import SlamData.Workspace.LevelOfDetails as LOD
import SqlSquared as Sql
import Utils.Path as PU

type DSL = CC.InnerCardDSL State Query
type HTML = CC.InnerCardHTML Query

troubleshootComponent ∷ CC.CardOptions → CC.CardComponent
troubleshootComponent =
  CC.makeCardComponent CT.Troubleshoot $ H.component
    { render: render
    , eval: evalCard ⨁ (absurd ∘ unwrap)
    , initialState: const initialState
    , receiver: const Nothing
    }

render ∷ State → HTML
render { varMap } =
  HH.table
    [ HP.classes
        [ HH.ClassName "form-builder"
        , CN.table
        , CN.tableStriped
        ]
    ]
    [ HH.thead_
        [ HH.tr_
            [ HH.th_ [ HH.text "Name" ]
            , HH.th_ [ HH.text "Value" ]
            ]
        ]
    , HH.tbody_ $ foldMap (uncurry renderItem) $ asArray $ Map.toUnfoldable $ VM.snapshot varMap
    ]

  where
    renderItem ∷ VM.Var → VM.VarMapValue → Array HTML
    renderItem (VM.Var name) val =
      [ HH.tr_
          [ HH.td_ [ HH.text name ]
          , HH.td_ $ renderCell val
          ]
      ]

    renderCell ∷ VM.VarMapValue → Array HTML
    renderCell = case _ of
      VM.Expr expr → [ HH.code_ [ HH.text $ Sql.print expr ] ]
      VM.Resource res → [ HH.text $ PU.printAnyFilePath $ Port.filePath res ]
      VM.Union ptrs → HH.div_ ∘ renderCell <$> Array.mapMaybe (flip VM.lookupValue varMap) ptrs

evalCard ∷ CC.CardEvalQuery ~> DSL
evalCard = case _ of
  CC.Activate next →
    pure next
  CC.Deactivate next →
    pure next
  CC.Save k →
    pure $ k Card.Troubleshoot
  CC.Load _ next →
    pure next
  CC.ReceiveInput _ varMap next → do
    H.modify _ { varMap = varMap }
    pure next
  CC.ReceiveOutput _ _ next →
    pure next
  CC.ReceiveState _ next →
    pure next
  CC.ReceiveDimensions _ reply →
    pure $ reply LOD.High
