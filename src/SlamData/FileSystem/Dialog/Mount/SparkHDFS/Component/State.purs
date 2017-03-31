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

module SlamData.FileSystem.Dialog.Mount.SparkHDFS.Component.State
  ( State
  , initialState
  , _sparkHost
  , _hdfsHost
  , updatePropAt
  , updateAvailableProps
  , fromConfig
  , toConfig
  , module MCS
  ) where

import SlamData.Prelude

import Data.Array as Array
import Data.Lens (Lens', lens)
import Data.URI.Path (printPath) as URI
import Data.URI.Host (printHost) as URI
import Data.StrMap as SM

import Quasar.Mount.SparkHDFS (Config)

import SlamData.FileSystem.Dialog.Mount.Common.State as MCS

data InputType = StringInput | NumberInput | IntInput

type State =
  { sparkHost ∷ MCS.MountHost
  , hdfsHost ∷ MCS.MountHost
  , path ∷ String
  , props ∷ Array MCS.MountProp
  , availableProps ∷ Array String
  }

propNames ∷ Array String
propNames =
  [ "spark.executor.memory"
  , "spark.executor.cores"
  , "spark.executor.extraJavaOptions"
  , "spark.default.parallelism"
  , "spark.files.maxPartitionBytes"
  , "spark.driver.cores"
  , "spark.driver.maxResultSize"
  , "spark.driver.memory"
  , "spark.local.dir"
  , "spark.reducer.maxSizeInFlight"
  , "spark.reducer.maxReqsInFlight"
  , "spark.shuffle.file.buffer"
  , "spark.shuffle.io.retryWait"
  , "spark.memory.fraction"
  , "spark.memory.storageFraction"
  , "spark.cores.max"
  , "spark.speculation"
  , "spark.tasks.cpus"
  ]

initialState ∷ State
initialState =
  { sparkHost: MCS.initialTuple
  , hdfsHost: MCS.initialTuple
  , path: ""
  , props: mempty
  , availableProps: propNames
  }

updateAvailableProps ∷ State → State
updateAvailableProps st = st { availableProps = availableProps }
  where
  availableProps ∷ Array String
  availableProps = Array.difference propNames (fst <$> st.props)

updatePropAt ∷ Int → (String × String) → State → State
updatePropAt ix value st = updateAvailableProps $ st { props = props' }
  where
  props'
    | fst value ≡ "" ∧ snd value ≡ "" = fromMaybe st.props $ Array.deleteAt ix st.props
    | ix >= Array.length st.props = Array.snoc st.props value
    | otherwise = fromMaybe st.props $ Array.updateAt ix value st.props

_sparkHost ∷ ∀ r a. Lens' { sparkHost ∷ a | r } a
_sparkHost = lens _.sparkHost (_ { sparkHost = _ })

_hdfsHost ∷ ∀ r a. Lens' { hdfsHost ∷ a | r } a
_hdfsHost = lens _.hdfsHost (_ { hdfsHost = _ })

fromConfig ∷ Config → State
fromConfig { sparkHost, hdfsHost, path, props } =
  updateAvailableProps
    { sparkHost: bimap URI.printHost (maybe "" show) sparkHost
    , hdfsHost: bimap URI.printHost (maybe "" show) hdfsHost
    , path: maybe "" URI.printPath path
    , props: map (fromMaybe "") <$> SM.toUnfoldable props
    , availableProps: propNames
    }

toConfig ∷ State → Either String Config
toConfig { sparkHost, hdfsHost, path, props } = do
  when (MCS.isEmpty (fst sparkHost)) $ Left "Please enter a Spark host"
  sparkHost' ← lmap ("Spark host: " <> _) $ MCS.parseHost sparkHost
  when (MCS.isEmpty (fst hdfsHost)) $ Left "Please enter an HDFS host"
  hdfsHost' ← lmap ("HDFS host: " <> _) $ MCS.parseHost hdfsHost
  let
    props' = props <#> \(Tuple key value) →
      if key ≡ "" ∨ value ≡ "" then Nothing else Just (Tuple key (Just value))
  pure
    { sparkHost: sparkHost'
    , hdfsHost: hdfsHost'
    , path: MCS.parsePath' =<< MCS.nonEmptyString path
    , props: SM.fromFoldable $ Array.catMaybes props'
    }
