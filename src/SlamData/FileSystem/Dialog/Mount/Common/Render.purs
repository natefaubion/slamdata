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

module SlamData.FileSystem.Dialog.Mount.Common.Render where

import SlamData.Prelude

import Data.Array ((..), length)
import Data.Array as Arr
import Data.Lens (Lens', Traversal', (.~), (^.))
import Data.Lens.Index (ix)
import Data.Profunctor.Strong (first, second)
import Data.String.Regex as Rx
import Data.String.Regex.Flags as RXF

import DOM.HTML.Indexed (HTMLinput)

import Halogen as H
import Halogen.HTML.Events as HE
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Halogen.Themes.Bootstrap3 as B

import SlamData.FileSystem.Dialog.Mount.Common.SettingsQuery (SettingsQuery(..))
import SlamData.FileSystem.Dialog.Mount.Common.State as MCS
import SlamData.Render.CSS as Rc

section :: forall p i. String -> Array (H.HTML p i) -> H.HTML p i
section title inner =
  HH.div
    [ HP.classes [ B.panel, B.panelDefault ] ]
    [ HH.div
        [ HP.class_ B.panelHeading ]
        [ HH.text title ]
    , HH.div
        [ HP.class_ B.panelBody ]
        inner
    ]

propListTable
  ∷ ∀ s p
  . Array (H.HTML p (SettingsQuery s))
  → H.HTML p (SettingsQuery s)
propListTable inner =
  HH.div
    [ HP.class_ Rc.mountProps ]
    [ HH.table
        [ HP.classes [B.table, B.tableBordered] ]
        [ HH.thead_
            [ HH.tr_
                [ HH.th_ [ HH.text "Name" ]
                , HH.th_ [ HH.text "Value" ]
                ]
            ]
        , HH.tbody_
            [ HH.tr_
                [ HH.td
                    [ HP.colSpan 2 ]
                    [ HH.div
                        [ HP.class_ Rc.mountPropsScrollbox ]
                        [ HH.table_ inner ]
                    ]
                ]
            ]
        ]
    ]

propList
  :: forall s p
   . Lens' s (Array (Tuple String String))
  -> s
  -> H.HTML p (SettingsQuery s)
propList _props state = propListTable propRows
  where
  propRows = prop <$> 0 .. (Arr.length (state ^. _props) - 1)

  prop :: Int -> H.HTML p (SettingsQuery s)
  prop index =
    HH.tr_ [ part first, part second ]
    where
    part :: Lens' (Tuple String String) String -> H.HTML p (SettingsQuery s)
    part lens = HH.td_ [ input state (_props <<< ix index <<< lens) classes ]
    classes = [ HP.classes [B.formControl, B.inputSm] ]

-- | A labelled section within the form.
label ∷ ∀ p i. String → Array (H.HTML p i) → H.HTML p i
label text inner = HH.label_ $ [ HH.span_ [ HH.text text ] ] <> inner

-- | A basic text input field that uses a lens to read from and update the
-- | state.
input
  ∷ ∀ s p
  . s
  → Traversal' s String
  → Array (HH.IProp HTMLinput (SettingsQuery s Unit))
  → H.HTML p (SettingsQuery s)
input state lens =
  input' id state lens -- can't eta reduce further here as the typechecker doesn't like it

-- | A basic text input field that uses a lens to read from and update the
-- | state, and allows for the input value to be modified.
input'
  ∷ ∀ s p
  . (String → String)
  → s
  → Traversal' s String
  → Array (HH.IProp HTMLinput (SettingsQuery s Unit))
  → H.HTML p (SettingsQuery s)
input' f state lens attrs =
  HH.input
    $ [ HP.class_ B.formControl
      , HE.onValueInput (HE.input \val → ModifyState (lens .~ f val))
      , HP.value (state ^. lens)
      ]
    <> attrs

hosts ∷ ∀ s. s → Lens' s (Array MCS.MountHost) → H.ComponentHTML (SettingsQuery s)
hosts state lens =
  HH.div
    [ HP.class_ Rc.mountHostList ]
    $ hostIx <$> 0 .. (length (state ^. lens) - 1)

  where
  hostIx index =
    HH.div
      [ HP.class_ Rc.mountHost ]
      [ label "Host"
          [ input' rejectNonHostname state (lens <<< ix index <<< MCS._host) [] ]
      , label "Port"
          [ input' rejectNonPort state (lens <<< ix index <<< MCS._port) [] ]
      ]

host ∷ ∀ s. s → Lens' s MCS.MountHost → H.ComponentHTML (SettingsQuery s)
host state lens =
  HH.div
    [ HP.class_ Rc.mountHostList ]
    [ HH.div
        [ HP.class_ Rc.mountHost ]
        [ label "Host"
            [ input' rejectNonHostname state (lens ∘ MCS._host) [] ]
        , label "Port"
            [ input' rejectNonPort state (lens ∘ MCS._port) [] ]
        ]
    ]

rejectNonHostname ∷ String → String
rejectNonHostname = Rx.replace rxNonHostname ""

rxNonHostname ∷ Rx.Regex
rxNonHostname =
  unsafePartial fromRight $
    Rx.regex "[^0-9a-z\\-\\._~%]" (RXF.ignoreCase <> RXF.global)

rejectNonPort ∷ String → String
rejectNonPort = Rx.replace rxNonPort ""

rxNonPort :: Rx.Regex
rxNonPort =
  unsafePartial fromRight $
    Rx.regex "[^0-9]" (RXF.global)
