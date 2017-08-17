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
module SlamData.Workspace.Card.Setups.DisplayOptions.Common.Render where

import SlamData.Prelude

import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import SlamData.Render.ClassName as CN

renderError ∷ ∀ f. Maybe String → H.ComponentHTML f
renderError = maybe (HH.text "") (HH.div props ∘ pure ∘ HH.text)
  where
  props = [ HP.classes [ CN.alert, CN.alertDanger, H.ClassName ("sd-dialog-error-box") ] ]
