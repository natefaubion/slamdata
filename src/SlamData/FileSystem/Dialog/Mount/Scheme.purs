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

module SlamData.FileSystem.Dialog.Mount.Scheme where

import SlamData.Prelude

data Scheme
  = MongoDB
  | SQL2
  | Module
  | Couchbase
  | MarkLogic
  | SparkHDFS
  | SparkFTP
  | SparkLocal

derive instance eqScheme ∷ Eq Scheme

derive instance ordScheme ∷ Ord Scheme

schemeToString :: Scheme -> String
schemeToString MongoDB = "MongoDB"
schemeToString SQL2 = "SQL²"
schemeToString Module = "SQL² Module"
schemeToString Couchbase = "Couchbase"
schemeToString MarkLogic = "MarkLogic"
schemeToString SparkHDFS = "HDFS on Spark"
schemeToString SparkFTP = "FTP on Spark"
schemeToString SparkLocal = "Local on Spark"

schemeFromString :: String -> Maybe Scheme
schemeFromString "MongoDB" = Just MongoDB
schemeFromString "SQL²" = Just SQL2
schemeFromString "SQL² Module" = Just Module
schemeFromString "Couchbase" = Just Couchbase
schemeFromString "MarkLogic" = Just MarkLogic
schemeFromString "HDFS on Spark" = Just SparkHDFS
schemeFromString "FTP on Spark" = Just SparkFTP
schemeFromString "Local on Spark" = Just SparkLocal
schemeFromString _ = Nothing

schemes :: Array Scheme
schemes =
  [ MongoDB
  , SQL2
  , Module
  , Couchbase
  , MarkLogic
  , SparkHDFS
  , SparkFTP
  , SparkLocal
  ]
