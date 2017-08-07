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

module SlamData.FileSystem.Dialog.Mount.Component.ChildSlot where

import SlamData.Prelude

import Halogen.Component.ChildPath as CP

import SlamData.FileSystem.Dialog.Mount.Couchbase.Component as Couchbase
import SlamData.FileSystem.Dialog.Mount.MarkLogic.Component as MarkLogic
import SlamData.FileSystem.Dialog.Mount.Mimir.Component as Mimir
import SlamData.FileSystem.Dialog.Mount.Module.Component as Module
import SlamData.FileSystem.Dialog.Mount.MongoDB.Component as MongoDB
import SlamData.FileSystem.Dialog.Mount.SparkFTP.Component as SparkFTP
import SlamData.FileSystem.Dialog.Mount.SparkHDFS.Component as SparkHDFS
import SlamData.FileSystem.Dialog.Mount.SparkLocal.Component as SparkLocal
import SlamData.FileSystem.Dialog.Mount.Unknown.Component as Unknown
import SlamData.FileSystem.Dialog.Mount.View.Component as View

type ChildQuery
  = MongoDB.Query
  ⨁ View.Query
  ⨁ Couchbase.Query
  ⨁ MarkLogic.Query
  ⨁ SparkFTP.Query
  ⨁ SparkHDFS.Query
  ⨁ SparkLocal.Query
  ⨁ Mimir.Query
  ⨁ Unknown.Query
  ⨁ Module.Query
  ⨁ Const Void

type ChildSlot
  = Unit
  ⊹ Unit
  ⊹ Unit
  ⊹ Unit
  ⊹ Unit
  ⊹ Unit
  ⊹ Unit
  ⊹ Unit
  ⊹ Unit
  ⊹ Unit
  ⊹ Void

cpMongoDB ∷ CP.ChildPath MongoDB.Query ChildQuery Unit ChildSlot
cpMongoDB = CP.cp1

cpView ∷ CP.ChildPath View.Query ChildQuery Unit ChildSlot
cpView = CP.cp2

cpCouchbase ∷ CP.ChildPath Couchbase.Query ChildQuery Unit ChildSlot
cpCouchbase = CP.cp3

cpMarkLogic ∷ CP.ChildPath MarkLogic.Query ChildQuery Unit ChildSlot
cpMarkLogic = CP.cp4

cpSparkFTP ∷ CP.ChildPath SparkFTP.Query ChildQuery Unit ChildSlot
cpSparkFTP = CP.cp5

cpSparkHDFS ∷ CP.ChildPath SparkHDFS.Query ChildQuery Unit ChildSlot
cpSparkHDFS = CP.cp6

cpSparkLocal ∷ CP.ChildPath SparkLocal.Query ChildQuery Unit ChildSlot
cpSparkLocal = CP.cp7

cpMimir ∷ CP.ChildPath Mimir.Query ChildQuery Unit ChildSlot
cpMimir = CP.cp8

cpUnknown ∷ CP.ChildPath Unknown.Query ChildQuery Unit ChildSlot
cpUnknown = CP.cp9

cpModule ∷ CP.ChildPath Module.Query ChildQuery Unit ChildSlot
cpModule = CP.cp10
