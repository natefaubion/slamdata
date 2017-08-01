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
module SlamData.Workspace.Card.Setups.FormatOptions.Component
  ( Query
  , Message(..)
  , component
  , module M
  ) where

import SlamData.Prelude

import Halogen as H
import Halogen.HTML as HH
import SlamData.Monad (Slam)
import SlamData.Workspace.Card.Setups.Dialog as CSD
import SlamData.Workspace.Card.Setups.FormatOptions.Model as M

data Query a
  = NoOp a
  | HandleDismiss a

data Message
  = Dismiss

type HTML = H.ComponentHTML Query
type DSL = H.ComponentDSL M.FormatOptions Query Message Slam

component ∷ H.Component HH.HTML Query M.FormatOptions Message Slam
component =
  H.component
    { render
    , eval
    , initialState: id
    , receiver: const Nothing
    }

render ∷ M.FormatOptions → HTML
render _ =
  CSD.pickerDialog
    { onDismiss: HandleDismiss
    , onConfirm: const NoOp
    , selection: Nothing
    , isSelectable: const false
    , classes: [ HH.ClassName "sd-format-options" ]
    , title: [ HH.text "Formatting options" ]
    , content: [HH.text "" ]
    }


eval ∷ Query ~> DSL
eval = case _ of
  NoOp next → pure next
  HandleDismiss next → do
    H.raise Dismiss
    pure next
