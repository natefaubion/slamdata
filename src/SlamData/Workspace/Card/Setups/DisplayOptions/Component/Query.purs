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
module SlamData.Workspace.Card.Setups.DisplayOptions.Component.Query where

import SlamData.Prelude

import SlamData.Workspace.Card.Setups.DisplayOptions.Model as M

data Query a
  = SetHorzAlignment M.Alignment a
  | SetVertAlignment M.Alignment a
  | ToggleStyle M.StyleOption Boolean a
  | SetSize M.Size a
  | SetFormat M.Format a
  | HandleFormatChange (Maybe M.FormatOptions) a
  | Raise Message a

data Message
  = Confirm M.DisplayOptions
  | Dismiss
