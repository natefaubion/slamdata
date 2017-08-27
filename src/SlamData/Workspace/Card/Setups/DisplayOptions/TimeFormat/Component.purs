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
module SlamData.Workspace.Card.Setups.DisplayOptions.TimeFormat.Component where

import SlamData.Prelude

import Data.Formatter.DateTime as FDT
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import SlamData.Render.Form as RF
import SlamData.Render.Form.ClassNames as RFCN
import SlamData.Workspace.Card.Setups.DisplayOptions.Common.Query as CQ
import SlamData.Workspace.Card.Setups.DisplayOptions.TimeFormat.State as S

type Query = CQ.Query S.State

type HTML = H.ComponentHTML Query

component ∷ ∀ m. H.Component HH.HTML Query (Maybe FDT.Formatter) (Maybe FDT.Formatter) m
component =
  H.lifecycleComponent
    { render
    , eval: CQ.evalInfallible S.toModel
    , initialState: maybe S.initialState S.fromModel
    , receiver: const Nothing
    , initializer: Just (H.action CQ.Init)
    , finalizer: Nothing
    }

render ∷ S.State → HTML
render { hours24, seconds } =
  HH.ul
    [ HP.class_ RFCN.options ]
    [ HH.li_ [ RF.renderCheckbox "24-hour clock" hours24 (CQ.Modify ∘ flip (_ { hours24 = _ })) ]
    , HH.li_ [ RF.renderCheckbox "Show seconds" seconds (CQ.Modify ∘ flip (_ { seconds = _ })) ]
    ]
