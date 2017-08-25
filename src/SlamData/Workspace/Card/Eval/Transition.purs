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

module SlamData.Workspace.Card.Eval.Transition
  ( Eval(..)
  , tagEval
  ) where

import SlamData.Prelude

import Quasar.Types (SQL)

import SlamData.Workspace.Card.DownloadOptions.Component.State as Download
import SlamData.Workspace.Card.Markdown.Model as Markdown
import SlamData.Workspace.Card.Viz.Model as Viz
import SlamData.Workspace.Card.Open.Model as Open
import SlamData.Workspace.Card.Table.Model as Table
import SlamData.Workspace.Card.Variables.Model as Variables
import SlamData.Workspace.Card.Setups.Viz.Model as SetupViz

data Eval
  = Pass
  | Composite
  | Terminal
  | Query SQL
  | Search String
  | Cache (Maybe String)
  | Error String
  | Markdown String
  | MarkdownForm Markdown.Model
  | Open Open.Model
  | Variables Variables.Model
  | DownloadOptions Download.State
  | SetupViz SetupViz.Model
  | Table Table.Model
  | Viz Viz.Model


tagEval ∷ Eval → String
tagEval = case _ of
  Pass → "Pass"
  Composite → "Composite"
  Terminal → "Terminal"
  Query str → "Query " <> show str
  Search str → "Search " <> show str
  Cache str → "Cache " <> show str
  Error str → "Error " <> show str
  Markdown str → "Markdown " <> show str
  Open _ → "Open"
  MarkdownForm m → "MarkdownForm"
  Variables m → "Variables"
  DownloadOptions m → "DownloadOptions"
  SetupViz _ → "SetupViz"
  Table _ → "Table"
  Viz _ → "Viz"
