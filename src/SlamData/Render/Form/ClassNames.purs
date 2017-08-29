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

module SlamData.Render.Form.ClassNames where

import Halogen as H

group ∷ H.ClassName
group = H.ClassName "sd-form-group"

input ∷ H.ClassName
input = H.ClassName "sd-form-input"

inputAddon ∷ H.ClassName
inputAddon = H.ClassName "sd-form-input-addon"

label ∷ H.ClassName
label = H.ClassName "sd-form-label"

options ∷ H.ClassName
options = H.ClassName "sd-form-options"

optionsLabel ∷ H.ClassName
optionsLabel = H.ClassName "sd-form-options-label"
