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
module SlamData.Workspace.Card.Setups.DisplayOptions.Component
  ( component
  , module Q
  , module M
  ) where

import SlamData.Prelude

import Halogen as H
import Halogen.HTML as HH
import SlamData.Monad (Slam)
import SlamData.Workspace.Card.Setups.DisplayOptions.Component.ChildSlot as CS
import SlamData.Workspace.Card.Setups.DisplayOptions.Component.State as S
import SlamData.Workspace.Card.Setups.DisplayOptions.Component.Query as Q
import SlamData.Workspace.Card.Setups.DisplayOptions.Component.Render as R
import SlamData.Workspace.Card.Setups.DisplayOptions.Model as M

type DSL = H.ParentDSL S.State Q.Query CS.ChildQuery CS.ChildSlot Q.Message Slam

component ∷ H.Component HH.HTML Q.Query M.DisplayOptions Q.Message Slam
component =
  H.parentComponent
    { render: R.render
    , eval
    , initialState: S.fromModel
    , receiver: const Nothing
    }

eval ∷ Q.Query ~> DSL
eval = case _ of
  Q.SetHorzAlignment align next → do
    H.modify (_ { alignment { horz = align } })
    pure next
  Q.SetVertAlignment align next → do
    H.modify (_ { alignment { vert = align } })
    pure next
  Q.ToggleStyle opt b next → do
    H.modify (\st → st { style = M.toggleStyle opt b st.style })
    pure next
  Q.SetFormat fmt next → do
    oldFormat ← H.gets _.format
    when (fmt /= oldFormat) do
      let fmtV = if fmt == M.Default then Just M.DefaultFormat else Nothing
      H.modify (_ { format = fmt, formatValue = fmtV })
    pure next
  Q.HandleFormatChange opts next → do
    H.modify (_ { formatValue = opts })
    pure next
  Q.Raise msg next → do
    H.raise msg
    pure next
