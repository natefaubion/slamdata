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
  ( Query
  , Message(..)
  , component
  , module M
  ) where

import SlamData.Prelude

import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import SlamData.Monad (Slam)
import SlamData.Workspace.Card.Setups.Dialog as CSD
import SlamData.Workspace.Card.Setups.DisplayOptions.Model as M
import SlamData.Render.Form as RF

data Query a
  = Modify (M.DisplayOptionsR → M.DisplayOptionsR) a
  | Raise Message a

data Message
  = Confirm M.DisplayOptions
  | Dismiss

type HTML = H.ComponentHTML Query
type DSL = H.ComponentDSL M.DisplayOptions Query Message Slam

component ∷ H.Component HH.HTML Query M.DisplayOptions Message Slam
component =
  H.component
    { render
    , eval
    , initialState: id
    , receiver: const Nothing
    }

render ∷ M.DisplayOptions → HTML
render st@(M.DisplayOptions { alignment, style, format }) =
  CSD.pickerDialog
    { onDismiss: Raise Dismiss
    , onConfirm: Raise ∘ Confirm
    , selection: Just st
    , isSelectable: const true
    , classes: [ HH.ClassName "sd-format-options" ]
    , title: [ HH.text "Formatting options" ]
    , content:
        [ renderAlignment alignment
        , renderStyle style
        ]
    }

renderAlignment ∷ { horz ∷ M.Alignment, vert ∷ M.Alignment } → HTML
renderAlignment { horz, vert } =
  HH.div_
    [ HH.div_
        [ HH.label_
            [ HH.text "Horizontal alignment"
            , RF.renderSelect
                M.alignments
                horz
                M.horzAlign
                (Modify ∘ flip (_ { alignment { horz = _ } }))
            ]
        ]
    , HH.div_
        [ HH.label_
            [ HH.text "Vertical alignment"
            , RF.renderSelect
                M.alignments
                vert
                M.vertAlign
                (Modify ∘ flip (_ { alignment { vert = _ } }))
            ]
        ]
    ]

renderStyle ∷ M.Style → HTML
renderStyle style =
  HH.div_
    [ HH.ul_
        [ HH.li_ [ renderStyle M.Emphasis "Emphasis" ]
        , HH.li_ [ renderStyle M.Strong "Strong" ]
        , HH.li_ [ renderStyle M.Underline "Underline" ]
        ]
    ]
  where
    renderStyle ∷ M.StyleOption → String → HTML
    renderStyle opt label =
      RF.renderCheckbox
        label
        (M.hasStyle opt style)
        (\b → Modify (\st → st { style = M.toggleStyle opt b st.style }))

eval ∷ Query ~> DSL
eval = case _ of
  Modify f next → do
    H.modify (_Newtype f)
    pure next
  Raise msg next → do
    H.raise msg
    pure next
