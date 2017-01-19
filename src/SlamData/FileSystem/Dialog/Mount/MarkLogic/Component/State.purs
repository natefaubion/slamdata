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

module SlamData.FileSystem.Dialog.Mount.MarkLogic.Component.State
  ( State
  , initialState
  , fromConfig
  , toConfig
  , module MCS
  ) where

import SlamData.Prelude

import Data.URI.Path (printPath) as URI
import Data.URI.Host (printHost) as URI

import Global as Global

import Quasar.Mount.MarkLogic (Config, Format(..))

import SlamData.FileSystem.Dialog.Mount.Common.State as MCS

type State =
  { host ∷ MCS.MountHost
  , user ∷ String
  , password ∷ String
  , path ∷ String
  , format ∷ Format
  }

initialState ∷ State
initialState =
  { host: MCS.initialTuple
  , user: ""
  , password: ""
  , path: ""
  , format: XML
  }

fromConfig ∷ Config → State
fromConfig { host, user, password, path, format } =
  { host: bimap URI.printHost (maybe "" show) host
  , user: maybe "" Global.decodeURIComponent user
  , password: maybe "" Global.decodeURIComponent password
  , path: maybe "" URI.printPath path
  , format
  }

toConfig ∷ State → Either String Config
toConfig { host, user, password, path, format } = do
  when (MCS.isEmpty (fst host)) $ Left "Please enter host"
  host' ← lmap ("Host: " <> _) $ MCS.parseHost host
  when (isNothing (snd host')) $ Left "Please enter port"
  when (not MCS.isEmpty user || not MCS.isEmpty password) do
    when (MCS.isEmpty user) $ Left "Please enter user name"
    when (MCS.isEmpty password) $ Left "Please enter password"
  pure
    { host: host'
    , user: Global.encodeURIComponent <$> MCS.nonEmptyString user
    , password: Global.encodeURIComponent <$> MCS.nonEmptyString password
    , path: MCS.parsePath' =<< MCS.nonEmptyString path
    , format
    }
