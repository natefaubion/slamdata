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

module SlamData.FileSystem.Dialog.Download.Component.State where

import SlamData.Prelude

import SlamData.Download.Model as DM
import SlamData.FileSystem.Resource as R

type State = DM.DownloadModel (resource ∷ R.Resource, error ∷ Maybe String)

initialState ∷ R.Resource → State
initialState resource =
  { resource
  , targetName:
      let name = R.resourceName resource
      in if name == "" then "archive" else name
  , compress: false
  , options: DM.initialOptions resource
  , error: Nothing
  }

validate ∷ State → State
validate state@{ targetName } =
  case DM.validFilename targetName of
    Left _ → state { error = Just "Please enter a valid target filename" }
    Right name → state { error = Nothing }
