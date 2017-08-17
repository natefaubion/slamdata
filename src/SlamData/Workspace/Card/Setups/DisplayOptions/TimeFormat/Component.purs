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
import Data.List ((:))
import Data.List as L
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import SlamData.Render.Form as RF
import SlamData.Workspace.Card.Setups.DisplayOptions.Common.Query as CQ

type Query = CQ.Query State

type State =
  { hours24 ∷ Boolean
  , seconds ∷ Boolean
  }

initialState ∷ State
initialState =
  { hours24: true
  , seconds: true
  }

fromModel ∷ FDT.Formatter → State
fromModel fmt =
  { hours24: L.elem FDT.Hours24 fmt
  , seconds: L.elem FDT.SecondsTwoDigits fmt
  }

toModel ∷ State → FDT.Formatter
toModel st = join
  $ pure (if st.hours24 then FDT.Hours24 else FDT.Hours12)
  : (FDT.Placeholder ":" : FDT.MinutesTwoDigits : L.Nil)
  : (guard st.seconds *> FDT.Placeholder ":" : FDT.SecondsTwoDigits : L.Nil)
  : (guard (not st.hours24) *> FDT.Placeholder " " : FDT.Meridiem : L.Nil)
  : L.Nil

type HTML = H.ComponentHTML Query

component ∷ ∀ m. H.Component HH.HTML Query (Maybe FDT.Formatter) (Maybe FDT.Formatter) m
component =
  H.lifecycleComponent
    { render
    , eval: CQ.evalInfallible toModel
    , initialState: maybe initialState fromModel
    , receiver: const Nothing
    , initializer: Just (H.action CQ.Init)
    , finalizer: Nothing
    }

render ∷ State → HTML
render { hours24, seconds } =
  HH.div
    [ HP.class_ (H.ClassName "sd-display-options-time") ]
    [ HH.ul_
        [ HH.li_ [ RF.renderCheckbox "24-hour clock" hours24 (CQ.Modify ∘ flip (_ { hours24 = _ })) ]
        , HH.li_ [ RF.renderCheckbox "Show seconds" seconds (CQ.Modify ∘ flip (_ { seconds = _ })) ]
        ]
    ]
