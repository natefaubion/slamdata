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
module SlamData.Workspace.Card.Setups.DisplayOptions.Component.State where

import SlamData.Prelude

import SlamData.Workspace.Card.Setups.DisplayOptions.Model as M

type State =
  { alignment ∷ M.Alignments
  , style ∷ M.Style
  , format ∷ M.Format
  , formatValue ∷ Maybe M.FormatOptions
  }

fromModel ∷ M.DisplayOptions → State
fromModel (M.DisplayOptions { alignment, style, format }) =
  { alignment
  , style
  , format: M.formatFromOptions format
  , formatValue: Just format
  }

toModel ∷ State → Maybe M.DisplayOptions
toModel st@{ alignment, style } = do
  format ← st.formatValue
  pure $ M.DisplayOptions { alignment, style, format }
