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

module SlamData.Workspace.Card.Setups.Auxiliary.Reset where

import SlamData.Prelude

import Data.Functor.Variant (VariantF, FProxy)

import Halogen as H
import Halogen.HTML as HH

type ResetF m r = VariantF ( reset ∷ FProxy (Tuple m) | r)

type AuxComponent a b m = H.Component HH.HTML (ResetF b a) (Maybe b) b m
