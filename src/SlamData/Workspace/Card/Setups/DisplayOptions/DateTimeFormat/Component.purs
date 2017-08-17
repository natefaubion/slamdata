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
module SlamData.Workspace.Card.Setups.DisplayOptions.DateTimeFormat.Component
  ( Input
  , Query
  , Message(..)
  , component
  ) where

import SlamData.Prelude

import Data.Formatter.DateTime as FDT
import Halogen as H
import Halogen.HTML as HH
import SlamData.Monad (Slam)

type Input = Maybe FDT.Formatter

type State = Unit
data Query a = NoOp a

type Message = Maybe FDT.Formatter

type HTML = H.ComponentHTML Query
type DSL = H.ComponentDSL State Query Message Slam

component ∷ H.Component HH.HTML Query Input Message Slam
component =
  H.component
    { render
    , eval
    , initialState: const unit
    , receiver: const Nothing
    }

render ∷ Unit → HTML
render _ = HH.text "Date & time"

eval ∷ Query ~> DSL
eval = case _ of
  NoOp next → do
    pure next
