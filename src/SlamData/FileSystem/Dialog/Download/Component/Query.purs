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

module SlamData.FileSystem.Dialog.Download.Component.Query where

import SlamData.Download.Model
import DOM.Event.Types (Event)
import Network.HTTP.RequestHeader (RequestHeader)

data Query a
  = TargetTyped String a
  | ToggleCompress a
  | SetOutput OutputType a
  | RaiseDismiss a
  | ModifyCSVOpts (CSVOptions -> CSVOptions) a
  | ModifyJSONOpts (JSONOptions -> JSONOptions) a
  | SetAuthHeaders (Array RequestHeader) a
  | PreventDefaultAndNewTab String Event a
